module Doromochi.View.PomodoroPane
  ( PomodoroPane
  , newPomodoroPane
  ) where

import Control.Monad.State.Strict (get)
import Data.IORef (IORef, readIORef)
import Doromochi.Types (JavaFX, liftJ, PomodoroTimer(..), Seconds, guidance, PomodoroIntervals(..))
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
    newFlowPane verticalOrient [ superCast restTimeZunko
                               , superCast stopTimerButton
                               , superCast guideLabel
                               ]
  where
    startWatcher :: PomodoroIntervals -> IORef Seconds -> ImageView -> Label -> Java a ()
    startWatcher prefs clockRef zunkoImage guideLabel = do
      let watchClock _ = do
            now <- io $ readIORef clockRef
            let currentStep = foo prefs now
            let file = imageOf currentStep
            zunkoImage <.> setImage file
            let whenNextStepIsCome = guidance currentStep prefs now
            guideLabel <.> setText whenNextStepIsCome
      timeline <- newKeyFrame (millis 1000) watchClock [] >>= newTimeline . (:[])
      timeline <.> setCycleCount indefinite
      timeline <.> playAnime


-- | Make an image view with `~/.config/doromochi/images/rest_time.png` for 'DoromochiPane'
makeRestTimeImageView :: Java a ImageView
makeRestTimeImageView = do
  configDir <- io $ (++ "/.config/doromochi") <$> getEnv "HOME"
  imageView <- newImageViewFrom $ configDir ++ "/images/rest_time.png"
  imageView <.> setFitHeight 512
  imageView <.> setFitWidth  512
  return imageView
  where
    newImageViewFrom :: FilePath -> Java a ImageView
    newImageViewFrom x =
      newFile x >- toURI
                >- toURL
                >- withThis (return . fromJava . toString)
                >>= newImage
                >>= newImageView
