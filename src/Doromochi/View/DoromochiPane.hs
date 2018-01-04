module Doromochi.View.DoromochiPane
  ( DoromochiPane
  , newDoromochiPane
  ) where

import Control.Monad.Reader (ask, asks)
import Data.IORef (newIORef)
import Doromochi.Types
import Doromochi.View.LicensePane (newLicensePane)
import Doromochi.View.PomodoroPane (newPomodoroPane)
import Doromochi.View.PreferencesPane (newPreferencesPane)
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
      stage <- asks $ primStage . fst
      return $ \_ -> do
        doro <.> setCenter pomodoroPane
        superCast stage <.> setHeight 700
        superCast stage <.> setWidth 700

    makeStartClockEvent :: JavaFX a (ActionEvent -> Java (EventHandler ActionEvent) ())
    makeStartClockEvent = do
      timerRef <- snd <$> ask
      return $ \_ -> startClock timerRef

    (.>.) :: Monad m => (a -> m b) -> (a -> m c) -> a -> m c
    (.>.) f g x = f x >> g x


-- | Make a menu bar for 'DoromochiPane'
makeMenuBar :: JavaFX a MenuBar
makeMenuBar = do
  libraryMenu <- makeLibraryMenu
  prefsMenu   <- makePrefsMenu
  liftJ $ do
    menuBar <- newMenuBar
    menuBar <.> getMenus >- addChild libraryMenu
    menuBar <.> getMenus >- addChild prefsMenu
    return menuBar


makeLibraryMenu :: JavaFX a Menu
makeLibraryMenu = do
  openLisenceWindow <- makeOpenLisenceWindow
  liftJ $ do
    licenseItem <- newMenuItem "License"
    licenseItem <.> setOnMenuItemAction openLisenceWindow
    self <- newMenu "Library"
    self <.> getMenuItems >- addChild licenseItem
    return self
  where
    -- Open a new window for 'LicensePane'
    makeOpenLisenceWindow :: JavaFX a (ActionEvent -> Java (EventHandler ActionEvent) ())
    makeOpenLisenceWindow = do
      app <- asks $ fxApp . fst
      return $ \_ -> do
        stage <- newStage
        emptyTimerRef <- newDefaultTimer >>= io . newIORef
        licensePane <- runJavaFX newLicensePane (AppRoot stage app, emptyTimerRef)
        scene <- newSceneWithoutSize licensePane
        stage <.> setScene scene
        stage <.> showStage


makePrefsMenu :: JavaFX a Menu
makePrefsMenu = do
  openPrefsWindow <- makeOpenPrefsWindow
  liftJ $ do
    prefsItem <- newMenuItem "Config"
    prefsItem <.> setOnMenuItemAction openPrefsWindow
    self <- newMenu "Preference"
    self <.> getMenuItems >- addChild prefsItem
    return self
  where
    makeOpenPrefsWindow :: JavaFX a (ActionEvent -> Java (EventHandler ActionEvent) ())
    makeOpenPrefsWindow = do
      timerRef <- snd <$> ask
      return $ \_ -> do
        prefsScene <- newPreferencesPane timerRef >>= newSceneWithoutSize
        stage <- newStage
        stage <.> setScene prefsScene
        stage <.> showStageAndWait
