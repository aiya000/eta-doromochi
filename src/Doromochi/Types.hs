{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}

-- | Define doromochi specific types
module Doromochi.Types
  ( AppCore (..)
  , PomodoroIntervals (..)
  , PomodoroTimer (..)
  , newDefaultTimer
  , RST
  , rst
  , runRST
  , JavaFX (..)
  , runJavaFX
  , liftJ
  ) where

import Control.Concurrent.Suspend (Delay, mDelay)
import Control.Concurrent.Timer (Timer, newTimer)
import Control.Monad.RWS.Strict (RWST(..), evalRWST)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State.Strict (MonadState)
import Data.Default (Default(..))
import Data.IORef (IORef, newIORef)
import Java
import JavaFX

default (Int) 

-- | A javafx application's resources
data AppCore = AppCore
  { primStage :: Stage       -- ^ This is the only existing in this app
  , fxApp     :: Application -- ^ This is the only existing too
  }

-- | Times of "pomodoro technic", 'PomodoroTimer' watches this
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

-- | A state on between `'JavaFX` a' and `'Java' a`, measures the pomodoro times
data PomodoroTimer = PomodoroTimer
  { intervalPrefs :: PomodoroIntervals -- ^ Preferences of 'PomodoroIntervals'
  , stopWatch :: IORef Int -- ^ A unique clock on `'JavaFX' a`, measure seconds with 'tickTimer'
  , tickTimer :: Timer IO -- ^ Increment 'stopWatch' on a second after 'startPomodoroCycle' is executed
  }

-- | A timer, that does nothing without 'startPomodoroCycle'
newDefaultTimer :: Java a PomodoroTimer
newDefaultTimer = PomodoroTimer def <$> io (newIORef 0) <*> io newTimer

--TODO: Implement
-- | Start a cycle of pomodoro technic with 'PomodoroTimer' of '`JavaFX` a'
startPomodoroCycle :: JavaFX a ()
startPomodoroCycle = undefined


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
