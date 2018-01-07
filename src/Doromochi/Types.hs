{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- | Define doromochi specific types
module Doromochi.Types
  ( AppRoot (..)
  , PomodoroIntervals (..)
  , Seconds (..)
  , (.*)
  , hours
  , minutes
  , asMinutes
  , simpler
  , oneBlockOf
  , oneCycleOf
  , PomodoroStep (..)
  , calcStep
  , correspondZunko
  , guidance
  , PomodoroTimer (..)
  , newDefaultTimer
  , readDefaultTimer
  , startClock
  , JavaFX (..)
  , runJavaFX
  , liftJ
  , liftJIO
  ) where

import Control.Concurrent.Suspend (sDelay)
import Control.Concurrent.Timer (TimerIO, newTimer, repeatedStart)
import Control.Monad (when)
import Control.Monad.Reader (MonadReader, ReaderT(..))
import Data.Default (Default(..))
import Data.IORef (IORef, modifyIORef, newIORef)
import Data.Traversable (forM)
import Doromochi.FilePath
import Java
import JavaFX
import Safe (readMay)
import System.Environment (getEnv)
import Text.Printf (printf)

-- | A javafx application's resources
data AppRoot = AppRoot
  { primStage :: Stage       -- ^ This is the only existing in this app
  , fxApp     :: Application -- ^ This is the only existing too
  }

-- | Times of "pomodoro technic", 'PomodoroTimer' watches this
data PomodoroIntervals = PomodoroIntervals
  { timeOnTask :: Seconds -- ^ A time on a working
  , timeOnShortRest :: Seconds -- ^ A rest time
  , lengthToLongRest :: Int
  , timeOnLongRest :: Seconds -- ^ A long rest time, this rest time comes after the cycle between 'timeOnTask' and 'timeOnShortRest' at 'lengthToLongRest' th is end
  } deriving (Show, Read)

instance Default PomodoroIntervals where
  def = PomodoroIntervals { timeOnTask = minutes 25
                          , timeOnShortRest = minutes 5
                          , lengthToLongRest = 3
                          , timeOnLongRest = minutes 30
                          }


-- A representation of times (seconds)
newtype Seconds = Seconds
  { unSeconds :: Int
  } deriving (Num, Integral, Real, Eq, Ord, Enum)

-- | x times (but x is `Int`)
(.*) :: Seconds -> Int -> Seconds
s .* x = s * Seconds x

--TODO: Enable doctest
--
-- |
-- >>> default (Seconds)
--
-- >>> show 28856
-- "08:00:56"
-- >>> show 29156
-- "08:05:56"
-- >>> show 27733
-- "07:42:13"
-- >>> show 43200
-- "12:00:00"
-- >>> show 3120
-- "00:52:00"
instance Show Seconds where
  show x = let (h, m, Seconds s') = simpler x
           in printf "%02d:%02d:%02d" h m s'

instance Read Seconds where
  readsPrec _ = \token ->
    let ((h, rest)   : _) = lex token
        ((m, rest')  : _) = lex $ tail rest
        ((s, rest'') : _) = lex $ tail rest'
        maybeHour   = readMay h
        maybeMinute = readMay m
        maybeSecond = readMay s
    in calc maybeHour maybeMinute maybeSecond rest''
      where
        calc :: Maybe Int -> Maybe Int -> Maybe Int -> String -> [(Seconds, String)]
        calc (Just h) (Just m) (Just s) rest = [(Seconds $ (h * 60 * 60) + (m * 60) + s, rest)]
        calc _ _ _ _ = []

-- | To the format of (hours, minutes, seconds)
simpler :: Seconds -> (Int, Int, Seconds)
simpler (Seconds x) = let h = x `div` 60 `div` 60
                          m = x `div` 60 `mod` 60
                          s = x `mod` 60
                      in (h, m, Seconds s)

-- | Take an `Int` as minutes, Give its 'Seconds'
minutes :: Int -> Seconds
minutes = Seconds . (60*)

-- | Similar to 'minutes', for minutes
hours :: Int -> Seconds
hours = (60*) . minutes

-- |
-- Take a 'Seconds', Give it as minutes
--
-- >>> default (Seconds)
--
-- >>> asMinutes $ minutes 60
-- 60
--
-- >>> asMinutes $ hours 1
-- 60
--
-- >>> asMinutes $ hours 1 + minutes 10
-- 70
asMinutes :: Seconds -> Int
asMinutes = (`div` 60) . unSeconds


-- | A time of a short cycle ('timeOnTask' + 'timeOnShortRest')
oneBlockOf :: PomodoroIntervals -> Seconds
oneBlockOf (PomodoroIntervals {..}) = timeOnTask + timeOnShortRest

-- | A time of a long cycle ('oneBlockOf' it * 'lengthToLongRest' + 'timeOnLongRest')
oneCycleOf :: PomodoroIntervals -> Seconds
oneCycleOf prefs@(PomodoroIntervals {..}) = oneBlockOf prefs .* lengthToLongRest + timeOnLongRest


--NOTE: Can haddock generate ?
-- | A step of the pomodoro technic with appendix states
data PomodoroStep =
    OnTask
      Seconds -- ^ seconds to the next short rest
      Seconds -- ^ seconds to the next long rest
    | OnShortRest
      Seconds -- ^ seconds to the next working
      Seconds -- ^ seconds to the next long rest
    | OnLongRest
      Seconds -- ^ seconds to the next working

{-
                       timeOnShortRest
|       timeOnTask        |  v  |
|-------------------------|-----|
|          a 'block'            |

|                              n th blocks (n = lengthToLongRest)                               |         timeOnLongRest       |
|-------------------------|-----|-------------------------|-----|-------------------------|-----|------------------------------|
|                                                        a 'cycle'                                                             |
-}

--FIXME: The long rest comes instead of the short rest after the cycle is end n th. the short/long rest are not duplicated 
-- | Calculate where 'Seconds' on 'PomodoroIntervals' is in
calcStep :: PomodoroIntervals -> Seconds -> PomodoroStep
calcStep prefs sec
  | onLongRest sec prefs
    = OnLongRest (nextWorking sec prefs)
  | onShortRest sec prefs
    = OnShortRest (nextWorking sec prefs) (nextLongRest sec prefs)
  | onTask sec prefs
    = OnTask (nextShortRest sec prefs) (nextLongRest sec prefs)
  | otherwise -- Usually, this is not passed through
    = error $ "calcStep: fatal error ! (" ++ show prefs ++ ", " ++ show sec ++ ")"
  where
    -- Remove times of any cycle from a 'Seconds'
    -- (Extract a time, it is after the current cycle starts)
    currentSec :: Seconds -> PomodoroIntervals -> Seconds
    currentSec sec prefs = sec `mod` oneCycleOf prefs

    onTask :: Seconds -> PomodoroIntervals -> Bool
    onTask sec prefs@(PomodoroIntervals {timeOnTask}) =
      let now = currentSec sec prefs `mod` oneBlockOf prefs
      in 0 <= now && now < timeOnTask

    onShortRest :: Seconds -> PomodoroIntervals -> Bool
    onShortRest sec prefs@(PomodoroIntervals {..}) =
      let now = currentSec sec prefs `mod` oneBlockOf prefs
      in timeOnTask <= now && now < oneBlockOf prefs .* lengthToLongRest

    onLongRest :: Seconds -> PomodoroIntervals -> Bool
    onLongRest sec prefs@(PomodoroIntervals {..}) =
      let now = currentSec sec prefs
      in oneBlockOf prefs .* lengthToLongRest <= now && now < oneCycleOf prefs

    nextWorking :: Seconds -> PomodoroIntervals -> Seconds
    nextWorking sec prefs =
      let now = currentSec sec prefs `mod` oneBlockOf prefs
      in oneBlockOf prefs - now

    nextLongRest :: Seconds -> PomodoroIntervals -> Seconds
    nextLongRest sec prefs@(PomodoroIntervals {lengthToLongRest}) =
      let now = currentSec sec prefs
      in oneBlockOf prefs .* lengthToLongRest - now

    nextShortRest :: Seconds -> PomodoroIntervals -> Seconds
    nextShortRest sec prefs@(PomodoroIntervals {timeOnTask}) =
      let now = currentSec sec prefs `mod` oneBlockOf prefs
      in timeOnTask - now


-- |
-- Return an expected file path of a png.
--
-- This doesn't include a directory path, return only the path of a file.
-- This means this result requires to be combined with $HOME or somewhere.
correspondZunko :: PomodoroIntervals -> PomodoroStep -> FilePath
correspondZunko (PomodoroIntervals {..}) (OnTask timeToNextShortRest _) =
  let secAfterWorkingIsStarted = timeOnTask - timeToNextShortRest
      halfTimeOnTask = timeOnTask `div` 2
  in if | halfTimeOnTask <= secAfterWorkingIsStarted -> zunkoOnTaskFirstHalf
        | otherwise -> zunkoOnTaskLastHalf
correspondZunko _ (OnShortRest _ _) = zunkoOnShortRest
correspondZunko _ (OnLongRest _) = zunkoOnLongRest

--TODO: Enable doctest (Fix dependencies problem)
-- |
-- Show it as a guidance for the next step
--
-- >>> default (Seconds)
--
-- >>> guidance $ OnTask (minutes 15) (minutes 80)
-- "次の休憩まであと15分（次の長休憩まであと80分）"
--
-- >>> guidance $ OnShortRest (minutes 4) (minutes 64)
-- "次の仕事時間まであと4分（次の長休憩まであと64分）"
--
-- >>> guidance $ OnLongRest (minutes 10)
-- "次の仕事時間まであと10分"
--
-- NOTICE:
-- This allows conditions that maybe invalid.
-- For example,
-- a time to the next short rest should be less than a time to next long rest on a working.
-- (That time to next long rest is not existent)
--
-- >>> guidance $ OnTask (minutes 80) (minutes 15)
-- "次の休憩まであと80分（次の長休憩まであと15分）"
guidance :: PomodoroStep -> String
guidance (OnTask timeToNextShortRest timeToNextLongRest)
    = printf "次の休憩まであと%d分（次の長休憩まであと%d分）"
        (asMinutes $ timeToNextShortRest + minutes 1) --NOTE: Why `+ 1` because a person maybe confused when they see "次の休憩まであと0分", and "次の休憩まであと24分" when the starting is just gotten
        (asMinutes $ timeToNextLongRest + minutes 1)
guidance (OnShortRest timeToNextWorking timeToNextLongRest)
    = printf "次の仕事時間まであと%d分（次の長休憩まであと%d分）"
        (asMinutes $ timeToNextWorking + minutes 1)
        (asMinutes $ timeToNextLongRest + minutes 1)
guidance (OnLongRest timeToNextWorking)
    = printf "次の仕事時間まであと%d分"
        (asMinutes $ timeToNextWorking + minutes 1)


-- | A state on between `'JavaFX` a' and `'Java' a`, measures the pomodoro times
data PomodoroTimer = PomodoroTimer
  { intervalPrefs :: IORef PomodoroIntervals -- ^ Preferences of 'PomodoroIntervals'
  , clockRef :: IORef Seconds -- ^ An unique clock on a `'JavaFX' a`, be counted up by 'clockTimer'
  , clockTimer :: TimerIO -- ^ By 'startClock', be set an action to increment 'pomodoroClock' a seconds
  }

-- |
-- A timer, that does nothing without 'startClock'.
--
-- This doesn't load the user configuration of 'PomodoroIntervals'.
-- Please use 'readDefaultTimer' instead if you want to load it.
newDefaultTimer :: Java a PomodoroTimer
newDefaultTimer = io $ PomodoroTimer <$> newIORef def <*> newIORef 1 <*> newTimer

-- |
-- Similar to 'newDefaultTimer' but only 'intervalPrefs' is loaded from the config file.
--
-- And if the user config cannot be found, or the user config is invalid format, return `Nothing`.
readDefaultTimer :: Java a (Maybe PomodoroTimer)
readDefaultTimer = do
  x <- newDefaultTimer
  maybeConfig <- io $ return . readMay =<< readFile =<< (++ configOfIntervals) . (++ "/.config/doromochi/") <$> getEnv "HOME"
  io . forM maybeConfig $ \config -> do
    configRef <- newIORef config
    return x { intervalPrefs = configRef }

-- |
-- Start a cycle of pomodoro technic.
--
-- The reason why this is X, because this is expected to be passed to a handler
-- (e.g. 'setOnButtonAction').
startClock :: PomodoroTimer -> Java a ()
startClock (PomodoroTimer _ clockRef timerIO) = io $ do
  let incrementClock = modifyIORef clockRef (+1)
  succeed <- repeatedStart timerIO incrementClock _1sec
  when (not succeed) $ error "startClock: fatal error ! (A `timerIO` couldn't be started)"
  where
    _1sec = sDelay 1


-- | This is impure type because 'IORef a's are passed to `Java b`s and to be modified
type AppEnv = (AppRoot, PomodoroTimer)

--TODO: Use STM for `'IORef' 'PomodoroTimer'`
-- | Store resources that depends the javafx application
newtype JavaFX a b = JavaFX
  { unJavaFX :: ReaderT AppEnv (Java a) b
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadReader AppEnv
             )

-- | Extract `'JavaFX' a`
runJavaFX :: JavaFX a b -> AppEnv -> Java a b
runJavaFX = runReaderT . unJavaFX

-- | Lift up a `'Java a'` as the `'JavaFX a'`
liftJ :: Java a b -> JavaFX a b
liftJ x = JavaFX . ReaderT $ \_ -> x

-- | Lift up an `IO` to `'JavaFX' a`
liftJIO :: IO b -> JavaFX a b
liftJIO = liftJ . io
