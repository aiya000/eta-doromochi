module Doromochi.View.PomodoroPane
  ( PomodoroPane
  , newPomodoroPane
  ) where

import Doromochi.Types (JavaFX, liftJ)
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
newPomodoroPane = liftJ $ do
  restTimeZunko <- makeRestTimeImageView
  stopTimerButton <- newButton "Stop" --TODO: Implement the action
  newFlowPane verticalOrient [ superCast restTimeZunko
                             , superCast stopTimerButton
                             ]


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
