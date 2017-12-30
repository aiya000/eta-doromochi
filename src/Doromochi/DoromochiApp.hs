{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}

module Doromochi.DoromochiApp
  ( DoromochiApp (..)
  , start
  ) where

import Doromochi.View.DoromochiPane (newDoromochiPane)
import Doromochi.JavaFX
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
  doromochiPane <- withThis $ runJavaFX newDoromochiPane . AppCore stage . superCast
  scene <- newScene doromochiPane 512 512
  stage <.> do
    setTitle "ドロもち"
    setScene scene
    showStage
