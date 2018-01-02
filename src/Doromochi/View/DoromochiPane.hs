module Doromochi.View.DoromochiPane
  ( DoromochiPane
  , newDoromochiPane
  ) where

import Control.Monad.Reader (asks)
import Control.Monad.State.Strict (get)
import Doromochi.Types (runJavaFX, JavaFX, liftJ, AppCore(..), newDefaultTimer, startClock)
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
  startClock' <- makeStartClockEvent
  liftJ $ startButton <.> setOnButtonAction (intentToPomodoroPane .>. startClock')
  return self
  where
    nil :: Maybe Node
    nil = Nothing

    makeIntentToPomodoroPane :: DoromochiPane -> JavaFX a (ActionEvent -> Java (EventHandler ActionEvent) ())
    makeIntentToPomodoroPane doro = do
      pomodoroPane <- newPomodoroPane
      stage <- asks primStage
      return $ \_ -> do
        doro <.> setCenter pomodoroPane
        superCast stage <.> setHeight 700
        superCast stage <.> setWidth 700

    makeStartClockEvent :: JavaFX a (ActionEvent -> Java (EventHandler ActionEvent) ())
    makeStartClockEvent = do
      timer <- get
      return $ \_ -> startClock timer

    (.>.) :: Monad m => (a -> m b) -> (a -> m c) -> a -> m c
    (.>.) f g x = f x >> g x


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
