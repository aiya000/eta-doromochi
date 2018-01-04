{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}

module Doromochi.DoromochiApp
  ( DoromochiApp (..)
  , start
  ) where

import Data.IORef (newIORef)
import Doromochi.Types
import Doromochi.View.DoromochiPane (newDoromochiPane)
import Java
import JavaFX

data {-# CLASS "io.github.aiya000.DoromochiApp extends javafx.application.Application" #-}
  DoromochiApp = DoromochiApp (Object# DoromochiApp)
  deriving (Class)

type instance Inherits DoromochiApp = '[Application]


-- | This is needed to run this application
foreign export java "start" start ::
  Stage -> Java DoromochiApp ()

-- | Show the window of 'DoromochiApp'
start :: Stage -> Java DoromochiApp ()
start stage = do
  stage <.> setTitle "ドロもち"
  --TODO: Create a pane or a window to make pomodoro prefs, Don't use `def :: PomodoroTimer`, Read prefs from the config
  timerRef <- newDefaultTimer >>= io . newIORef
  doromochiPane <- withThis $ runJavaFX newDoromochiPane . (,timerRef) . AppRoot stage . superCast
  scene <- newScene doromochiPane 256 256
  stage <.> do
    setTitle "ドロもち"
    setScene scene
    showStage
