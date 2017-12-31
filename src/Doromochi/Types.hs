{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Define doromochi specific types
module Doromochi.Types
  ( AppCore (..)
  , PomodoroIntervals (..)
  , PomodoroTimer (..)
  , newEmptyPomodoroTimer
  , JavaFX (..)
  , runJavaFX
  , liftJ
  ) where

import Control.Concurrent.Suspend (Delay, mDelay)
import Control.Concurrent.Timer (Timer, newTimer)
import Data.Default (Default(..))
import Java
import JavaFX

-- | These are needed in the javafx application
data AppCore = AppCore
  { primStage :: Stage       -- ^ This is the only existing in this app
  , fxApp     :: Application -- ^ This is the only existing too
  }

-- |
-- Your times of your pomodoro intervals,
-- 'PomodoroTimer' watches this.
data PomodoroIntervals = PomodoroIntervals
  { timeOnTask :: Delay -- ^ A time on a working
  , timeOnShortRest :: Delay -- ^ A rest time
  , lengthToLongRest :: Int
  , timeOnLongRest :: Delay -- ^ A long rest time, this rest time comes after the cycle between 'timeOnTask' and 'timeOnShortRest' at 'lengthToLongRest' th is end
  }

instance Default PomodoroIntervals where
  def = PomodoroIntervals { timeOnTask = mDelay 25
                          , timeOnShortRest = mDelay 5
                          , lengthToLongRest = 3
                          , timeOnLongRest = mDelay 30
                          }

-- | A state
data PomodoroTimer = PomodoroTimer
  { pomodoroTimer :: Timer IO
  , intervalPrefs :: PomodoroIntervals
  }

-- | A timer, that does nothing
newEmptyPomodoroTimer :: Java a PomodoroTimer
newEmptyPomodoroTimer = flip PomodoroTimer def <$> io newTimer


-- | Store resources that depends the javafx application
newtype JavaFX a b = JavaFX
  { unJavaFX :: ReaderT AppCore (Java a) b
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadReader AppCore
             )

-- | Extract `'JavaFX' a`
runJavaFX :: JavaFX a b -> AppCore -> Java a b
runJavaFX context x = flip runReaderT x $ unJavaFX context

-- | Lift up a `'Java a'` as the `'JavaFX a'`
liftJ :: Java a b -> JavaFX a b
liftJ x = JavaFX . ReaderT $ const x
