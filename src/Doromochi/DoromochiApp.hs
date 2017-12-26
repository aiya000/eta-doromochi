module Doromochi.DoromochiApp
  ( start
  ) where

import Doromochi.Types (DoromochiApp(..))
import Doromochi.View.LicensePane (newLicensePane)
import Java
import Java.Doromochi
import JavaFX
import System.Environment (getEnv)


-- | This is needed to run this application
foreign export java "start" start ::
  Stage -> Java DoromochiApp ()

-- | Show the window of 'DoromochiApp'
start :: Stage -> Java DoromochiApp ()
start stage = do
  stage <.> setTitle "ドロもち"
  root <- newGroup
  scene <- newScene root 512 512
  menuBar <- withThis (makeMenuBar stage)
  imageView <- makeRestTimeImageView
  root <.> getChildren >- addChild imageView
  root <.> getChildren >- addChild menuBar
  stage <.> do
    setTitle "ドロもち"
    setScene scene
    showStage
  where
    makeMenuBar :: Stage -> DoromochiApp -> Java a MenuBar
    makeMenuBar stage app = do
      menuBar <- newMenuBar
      licenseMenu <- newMenu "Library"
      licenseItem <- newMenuItem "License"
      licenseItem <.> setOnMenuItemAction (intentToLicensePane stage app)
      licenseMenu <.> getMenuItems >- addChild licenseItem
      menuBar <.> getMenus >- addChild licenseMenu
      return menuBar

    intentToLicensePane :: Stage -> DoromochiApp -> (ActionEvent -> Java (EventHandler ActionEvent) ())
    intentToLicensePane stage app = \_ -> do
      root <- newGroup
      licensePane <- newLicensePane app
      root <.> getChildren >- addChild licensePane
      scene <-  newSceneWithoutSize root
      stage <.> setScene scene


-- | Make 'ImageView' with `~/.config/doromochi/images/rest_time.png`
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
