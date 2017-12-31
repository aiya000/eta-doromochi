module Doromochi.View.DoromochiPane
  ( DoromochiPane
  , newDoromochiPane
  ) where

import Control.Monad.Reader (asks)
import Doromochi.Types (runJavaFX, JavaFX, liftJ, AppCore(..), newDefaultTimer)
import Doromochi.View.LicensePane (newLicensePane)
import Doromochi.View.PomodoroPane (newPomodoroPane)
import Java
import JavaFX

-- |
-- The primary (main) content of this app,
-- also this is shown when this app starts
type DoromochiPane = BorderPane
--TODO: ^ Use newtype or data


-- | Make a 'DoromochiPane'
newDoromochiPane :: JavaFX a DoromochiPane
newDoromochiPane = do
  menuBar <- makeMenuBar
  startButton <- liftJ $ newButton "Start"
  self <- liftJ $ newBorderPane (Just startButton) (Just menuBar) nil nil nil
  intentToPomodoroPane <- makeIntentToPomodoroPane self
  liftJ $ startButton <.> setOnButtonAction intentToPomodoroPane
  --liftJ $ startButton <.> setOnButtonAction (intentToPomodoroPane >> startPomodoroCycle)
  return self
  where
    nil :: Maybe Node
    nil = Nothing

    makeIntentToPomodoroPane :: DoromochiPane -> JavaFX a (ActionEvent -> Java (EventHandler ActionEvent) ())
    makeIntentToPomodoroPane doro = do
      pomodoroPane <- newPomodoroPane
      return $ \_ -> doro <.> setCenter pomodoroPane


-- | Make a menu bar for 'DoromochiPane'
makeMenuBar :: JavaFX a MenuBar
makeMenuBar = do
  openLisenceWindow <- makeOpenLisenceWindow
  liftJ $ do
    menuBar <- newMenuBar
    licenseMenu <- newMenu "Library"
    licenseItem <- newMenuItem "License"
    licenseItem <.> setOnMenuItemAction openLisenceWindow
    licenseMenu <.> getMenuItems >- addChild licenseItem
    menuBar <.> getMenus >- addChild licenseMenu
    return menuBar
  where
    -- Open a new window for 'LicensePane'
    makeOpenLisenceWindow :: JavaFX a (ActionEvent -> Java (EventHandler ActionEvent) ())
    makeOpenLisenceWindow = do
      app <- asks fxApp
      return $ \_ -> do
        stage <- newStage
        emptyTimer <- newDefaultTimer
        licensePane <- flip (runJavaFX newLicensePane) emptyTimer $ AppCore stage app
        scene <- newSceneWithoutSize licensePane
        stage <.> setScene scene
        stage <.> showStage
