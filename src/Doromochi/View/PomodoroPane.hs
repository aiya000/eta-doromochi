module Doromochi.View.PomodoroPane
  ( PomodoroPane
  , newPomodoroPane
  ) where

import Control.Monad.State.Strict (get)
import Data.IORef (IORef, readIORef)
import Doromochi.Types
import Java
import Java.Doromochi (newFile, toURI, toURL)
import JavaFX
import System.Environment (getEnv)

-- |
-- This shows a images of zunko and a timer watch.
-- A timer watch means a pomodoro intervals.
-- The zunko images are changed at each interval switch.
type PomodoroPane = FlowPane
--TODO: ^ Use newtype or data


-- | Make a 'PomodoroPane'
newPomodoroPane :: JavaFX a PomodoroPane
newPomodoroPane = do
  PomodoroTimer prefs clockRef _ <- get
  liftJ $ do
    stopTimerButton <- newButton "Stop" --TODO: Implement the action
    zunkoImage <- newImageViewOfEmpty
    guideLabel <- newLabel "ここはガイドです"
    startWatcher prefs clockRef zunkoImage guideLabel
    newFlowPane verticalOrient [ superCast zunkoImage
                               , superCast stopTimerButton
                               , superCast guideLabel
                               ]
  where
    startWatcher :: PomodoroIntervals -> IORef Seconds -> ImageView -> Label -> Java a ()
    startWatcher prefs clockRef zunkoImage guideLabel = do
      let watchClock _ = do
            currentStep <- io $ calcStep prefs <$> readIORef clockRef
            currentZunko <- newZunkoImageOfStep prefs currentStep
            zunkoImage <.> setImage currentZunko
            guideLabel <.> setText (guidance currentStep)
      timeline <- newKeyFrame (millis 1000) watchClock [] >>= newTimeline . (:[])
      timeline <.> setCycleCount indefinite
      timeline <.> playAnime

    -- Read a 'Image' with the correspond png file with a 'PomodoroStep'
    newZunkoImageOfStep :: PomodoroIntervals -> PomodoroStep -> Java a Image
    newZunkoImageOfStep prefs step = do
      imagesDir <- io $ (++ "/.config/doromochi/images/") <$> getEnv "HOME"
      let correspondFile = imagesDir ++ correspondZunko prefs step
      newFile correspondFile
        >- toURI
        >- toURL
        >- withThis (return . fromJava . toString)
        >>= newImage
