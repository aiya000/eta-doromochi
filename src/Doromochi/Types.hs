{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

-- | Define doromochi specific types
module Doromochi.Types
  ( AppCore (..)
  , PomodoroIntervals (..)
  , Seconds (..)
  , (.*)
  , hours
  , asMinutes
  , simpler
  , PomodoroStep (..)
  , calcStep
  , correspondZunko
  , guidance
  , PomodoroTimer (..)
  , newDefaultTimer
  , startClock
  , RST
  , rst
  , runRST
  , JavaFX (..)
  , runJavaFX
  , liftJ
  ) where

import Control.Concurrent.Suspend (sDelay)
import Control.Concurrent.Timer (TimerIO, newTimer, repeatedStart)
import Control.Monad (void)
import Control.Monad.RWS.Strict (RWST(..), evalRWST)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State.Strict (MonadState(..))
import Data.Default (Default(..))
import Data.IORef (IORef, newIORef, modifyIORef, readIORef)
import Doromochi.FilePath (zunkoOnTaskFirstHalf, zunkoOnTaskLastHalf, zunkoOnShortRest, zunkoOnLongRest)
import Java
import JavaFX
import Text.Printf (printf)

-- | A javafx application's resources
data AppCore = AppCore
  { primStage :: Stage       -- ^ This is the only existing in this app
  , fxApp     :: Application -- ^ This is the only existing too
  }

-- | Times of "pomodoro technic", 'PomodoroTimer' watches this
data PomodoroIntervals = PomodoroIntervals
  { timeOnTask :: Seconds -- ^ A time on a working
  , timeOnShortRest :: Seconds -- ^ A rest time
  , lengthToLongRest :: Int
  , timeOnLongRest :: Seconds -- ^ A long rest time, this rest time comes after the cycle between 'timeOnTask' and 'timeOnShortRest' at 'lengthToLongRest' th is end
  }

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

--FIXME: The long rest comes instead of the short rest after the cycle is end n th. the short/long rest are not duplicated 
-- | Calculate where 'Seconds' on 'PomodoroIntervals' is in
calcStep :: PomodoroIntervals -> Seconds -> PomodoroStep
calcStep prefs sec
  | onTask sec prefs
    = OnTask (nextShortRest sec prefs) (nextLongRest sec prefs)
  | onShortRest sec prefs
    = OnShortRest (nextWorking sec prefs) (nextLongRest sec prefs)
  | otherwise -- If it is not on the task and is not on the short rest, so it is on the long rest
    = OnLongRest (nextWorking sec prefs)
  where
    -- A time of a short cycle
    oneBlockOf :: PomodoroIntervals -> Seconds
    oneBlockOf (PomodoroIntervals {..}) = timeOnTask + timeOnShortRest

    -- A time of a long cycle
    oneCycleOf :: PomodoroIntervals -> Seconds
    oneCycleOf prefs@(PomodoroIntervals {..}) = oneBlockOf prefs .* lengthToLongRest + timeOnLongRest

    -- Remove times of any cycle from a 'Seconds'
    -- (Extract a time, it is after the current cycle starts)
    currentSec :: Seconds -> PomodoroIntervals -> Seconds
    currentSec sec prefs = sec `mod` oneCycleOf prefs

    onTask :: Seconds -> PomodoroIntervals -> Bool
    onTask sec prefs@(PomodoroIntervals {timeOnTask}) = currentSec sec prefs < timeOnTask

    onShortRest :: Seconds -> PomodoroIntervals -> Bool
    onShortRest sec prefs = currentSec sec prefs < oneBlockOf prefs

    nextWorking :: Seconds -> PomodoroIntervals -> Seconds
    nextWorking sec prefs = oneBlockOf prefs - currentSec sec prefs

    nextLongRest :: Seconds -> PomodoroIntervals -> Seconds
    nextLongRest sec prefs@(PomodoroIntervals {lengthToLongRest}) =
      oneBlockOf prefs .* lengthToLongRest - currentSec sec prefs

    nextShortRest :: Seconds -> PomodoroIntervals -> Seconds
    nextShortRest sec prefs@(PomodoroIntervals {timeOnTask}) =
      timeOnTask - currentSec sec prefs

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


--TODO: Move 'globalClock' and 'tickTimer' to the Reader
--TODO: Use STM in 'globalClock' and 'tickTimer'
-- | A state on between `'JavaFX` a' and `'Java' a`, measures the pomodoro times
data PomodoroTimer = PomodoroTimer
  { intervalPrefs :: PomodoroIntervals -- ^ Preferences of 'PomodoroIntervals'
  , globalClock :: IORef Seconds -- ^ An unique clock on `'JavaFX' a`, be counted up by 'tickTimer'
  , tickTimer :: IORef TimerIO -- ^ Increment 'globalClock' on a second after 'startClock' is executed
  }

-- | A timer, that does nothing without 'startClock'
newDefaultTimer :: Java a PomodoroTimer
newDefaultTimer = PomodoroTimer def <$> io (newIORef 1) <*> io (newTimer >>= newIORef)

-- |
-- Start a cycle of pomodoro technic with 'PomodoroTimer' of '`JavaFX` a'.
--
-- This is in `'Java' a`
-- because this is expected to be passed to a handler
-- (e.g. 'setOnButtonAction').
startClock :: PomodoroTimer -> Java a ()
startClock (PomodoroTimer _ clockRef timerRef) = io $ do
  let _1sec = sDelay 1
  let incrementClock = modifyIORef clockRef (+1)
  timer <- readIORef timerRef
  void $ repeatedStart timer incrementClock _1sec


type RST r s m a = RWST r () s m a

--TODO: Remove `Monad m` restriction
rst :: Monad m => (r -> s -> m (a, s)) -> RST r s m a
rst f = RWST $ \r s -> do
  (x, y) <- f r s
  return (x, y, ())

--TODO: Remove `Monad m` restriction
runRST :: Monad m => RST r s m a -> r -> s -> m (a, s)
runRST x r s = runRWST x r s >>= \(a, b, ()) -> return (a, b)


-- | Store resources that depends the javafx application
newtype JavaFX a b = JavaFX
  { unJavaFX :: RST AppCore PomodoroTimer (Java a) b
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadReader AppCore
             , MonadState PomodoroTimer
             )

-- | Extract `'JavaFX' a`
runJavaFX :: JavaFX a b -> AppCore -> PomodoroTimer -> Java a b
runJavaFX context r s = do
  (x, _) <- evalRWST (unJavaFX context) r s
  return x

-- | Lift up a `'Java a'` as the `'JavaFX a'`
liftJ :: Java a b -> JavaFX a b
liftJ x = let j = rst $ \_ timer -> (, timer) <$> x
          in JavaFX j
