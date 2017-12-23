{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}

module Doromochi.DoromochiApp
  ( DoromochiApp (..)
  , start
  ) where

import Doromochi.View.LicensePane (newLicensePane)
import Java
import Java.Doromochi
import JavaFX
import System.Environment (getEnv)

data {-# CLASS "io.github.aiya000.DoromochiApp extends javafx.application.Application" #-}
  DoromochiApp = DoromochiApp (Object# DoromochiApp)
  deriving (Class)


-- | This is needed to run this application
foreign export java "start"
  start :: Stage -> Java DoromochiApp ()


--TODO: Bundle fxml s (in jar ?), don't depend upon install.sh
-- | Show the window of 'DoromochiApp'
start :: Stage -> Java DoromochiApp ()
start stage = do
  stage <.> setTitle "ドロもち"
  root <- newGroup
  scene <- newScene root 512 512
  menuBar <- makeMenuBar stage
  imageView <- makeRestTimeImageView
  root <.> getChildren >- addChild imageView
  root <.> getChildren >- addChild menuBar
  stage <.> do
    setTitle "ドロもち"
    setScene scene
    showStage
  where
    makeMenuBar :: Stage -> Java a MenuBar
    makeMenuBar stage = do
      menuBar     <- newMenuBar
      licenseMenu <- newMenu "Library"
      licenseItem <- newMenuItem "License"
      licenseItem <.> setOnMenuItemAction (intentToLicenseApp stage)
      licenseMenu <.> getMenuItems >- addChild licenseItem
      menuBar <.> getMenus >- addChild licenseMenu
      return menuBar

    intentToLicenseApp :: Stage -> ActionEvent -> Java (EventHandler ActionEvent) ()
    intentToLicenseApp stage = \_ -> do
      root <- newGroup
      licensePane <- newLicensePane
      root <.> getChildren >- addChild licensePane
      scene <-  newSceneWithoutSize root
      stage <.> setScene scene


-- | Make 'ImageView' with `~/.config/doromochi/images/rest_time.png`
makeRestTimeImageView :: Java a ImageView
makeRestTimeImageView = do
  configDir <- getAppConfigDir
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


--TODO: Make this exception safety with 'Control.Exception.Safe.MonadCatch'
-- | Read the fully path of ~/.config/doromochi
getAppConfigDir :: Java a FilePath
getAppConfigDir = do
  homeDir <- io $ getEnv "HOME"
  return $ homeDir ++ "/.config/doromochi"
