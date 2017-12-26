module Doromochi.DoromochiPane
  ( DoromochiPane
  , newDoromochiPane
  ) where

import Doromochi.View.LicensePane (newLicensePane)
import Java
import Java.Doromochi
import JavaFX
import System.Environment (getEnv)

-- |
-- The primary (main) content of this app,
-- also this is shown when this app starts
type DoromochiPane = BorderPane
--TODO: ^ Use newtype or data


--TODO: Use 'StateT' for passing 'Application' and 'Stage'
-- | Make a 'DoromochiPane'
newDoromochiPane :: Application -> Stage -> Java a DoromochiPane
newDoromochiPane app stage = do
  menuBar <- makeMenuBar app stage
  imageView <- makeRestTimeImageView
  contents <- newFlowPane verticalOrient [ superCast menuBar
                                         , superCast imageView
                                         ]
  newBorderPane (Just contents) (Just menuBar) nil nil nil
  where
    nil :: Maybe Node
    nil = Nothing


-- | Make a menu bar for 'DoromochiPane'
makeMenuBar :: Application -> Stage -> Java a MenuBar
makeMenuBar app stage = do
  menuBar <- newMenuBar
  licenseMenu <- newMenu "Library"
  licenseItem <- newMenuItem "License"
  licenseItem <.> setOnMenuItemAction (intentToLicensePane app stage)
  licenseMenu <.> getMenuItems >- addChild licenseItem
  menuBar <.> getMenus >- addChild licenseMenu
  return menuBar


-- | Make an intent action for 'DoromochiPane'
intentToLicensePane :: Application -> Stage -> (ActionEvent -> Java (EventHandler ActionEvent) ())
intentToLicensePane app stage = \_ -> do
  licensePane <- newLicensePane app
  scene <- newSceneWithoutSize licensePane
  stage <.> setScene scene


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
