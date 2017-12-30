module Doromochi.DoromochiPane
  ( DoromochiPane
  , newDoromochiPane
  ) where

import Control.Monad.Reader (ask)
import Doromochi.JavaFX (JavaFX, liftJ, AppCore(..))
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


-- | Make a 'DoromochiPane'
newDoromochiPane :: JavaFX a DoromochiPane
newDoromochiPane = do
  menuBar <- makeMenuBar
  liftJ $ do
    imageView <- makeRestTimeImageView
    contents <- newFlowPane verticalOrient [ superCast menuBar
                                           , superCast imageView
                                           ]
    newBorderPane (Just contents) (Just menuBar) nil nil nil
  where
    nil :: Maybe Node
    nil = Nothing


-- | Make a menu bar for 'DoromochiPane'
makeMenuBar :: JavaFX a MenuBar
makeMenuBar = do
  openLisenceApp <- makeOpenLisenceApp
  liftJ $ do
    menuBar <- newMenuBar
    licenseMenu <- newMenu "Library"
    licenseItem <- newMenuItem "License"
    licenseItem <.> setOnMenuItemAction openLisenceApp
    licenseMenu <.> getMenuItems >- addChild licenseItem
    menuBar <.> getMenus >- addChild licenseMenu
    return menuBar


-- | Make an action to open a new window for 'licensePane'
makeOpenLisenceApp :: JavaFX a (ActionEvent -> Java (EventHandler ActionEvent) ())
makeOpenLisenceApp = do
  AppCore stage _ <- ask
  licensePane <- newLicensePane
  liftJ $ do
    scene <- newSceneWithoutSize licensePane
    return $ \_ -> stage <.> setScene scene


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
