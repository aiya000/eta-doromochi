module Doromochi.View.PomodoroPane
  ( PomodoroPane
  , newPomodoroPane
  ) where

import Control.Monad.Reader (ask)
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
  timerRef <- snd <$> ask
  liftJ $ do
    zunkoImage <- newImageViewOfEmpty
    guideLabel <- newLabel "" -- e.g. "次の休憩まであと15分（次の長休憩まであと80分）"
    timeLabel <- newLabel "" -- e.g. "Time: 00:25:20"
    buttons <- makeButtons
    startWatcher timerRef ( zunkoImage
                          , guideLabel
                          , timeLabel
                          )
    newFlowPane verticalOrient [ superCast zunkoImage
                               , superCast timeLabel
                               , superCast guideLabel
                               , superCast buttons
                               ]
  where
    makeButtons :: Java a FlowPane
    makeButtons = do
      stopTimerButton <- newButton "Stop" --TODO: Implement the action
      resetTimerButton <- newButton "Reset" --TODO: Implement the action
      newFlowPane horizontalOrient $ map superCast [stopTimerButton, resetTimerButton]

    startWatcher :: IORef PomodoroTimer -> (ImageView , Label, Label) -> Java a ()
    startWatcher timerRef (zunkoImage, guideLabel, timeLabel) = do
      PomodoroTimer prefs now _ <- io $ readIORef timerRef
      let watchClock _ = do
            timeLabel <.> setText ("Time " ++ show now)
            let currentStep = calcStep prefs now
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
