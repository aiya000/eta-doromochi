-- | Present 'LicensePane' by 'newLicensePane'
module Doromochi.View.LicensePane where

import Control.Monad.Reader (ask)
import Doromochi.JavaFX (JavaFX, liftJ, AppCore(..))
import Java
import JavaFX

-- |
-- A subclass of 'StackPane',
-- this view shows the libraries
-- that are used by doromochi
type LicensePane = FlowPane
--TODO: ^ Use newtype or data

--TODO: Improve appearance
-- | Make a view of 'LicensePane'
newLicensePane :: JavaFX a LicensePane
newLicensePane = do
  doromochi <- newLabelJ "ドロもち"
  imageAuthor <- newLabelJ "The copyright for the zunko images in this software is owned by @HassakuTb on （ず・ω・きょ）"
  imageLink <- newHyperlinkWithOpening "https://github.com/aiya000/eta-doromochi/blob/master/images"
  hassakuLink <- newHyperlinkWithOpening "https://twitter.com/HassakuTb"
  zunkyoLink <- newHyperlinkWithOpening "https://zunko.jp/guideline.html"
  aboutLicense <- newLabelJ "Also this software includes the work that is distributed in the Apache License 2.0."
  bar <- newLabelJ "- - -"
  thisAppDepends <- newLabelJ "This software depends below softwares"
  emptyLine <- newLabelJ ""
  etaExamples <- newLabelJ "typelead/eta-examples"
  etaExamplesLink <- newHyperlinkWithOpening "https://github.com/typelead/eta-examples"
  liftJ $ newFlowPane verticalOrient [ superCast doromochi
                                     , superCast imageAuthor
                                     , superCast imageLink
                                     , superCast hassakuLink
                                     , superCast zunkyoLink
                                     , superCast bar
                                     , superCast aboutLicense
                                     , superCast thisAppDepends
                                     , superCast emptyLine
                                     , superCast etaExamples
                                     , superCast etaExamplesLink
                                     ]
  where
    newLabelJ :: String -> JavaFX a Label
    newLabelJ = liftJ . newLabel

    newHyperlinkWithOpening :: String -> JavaFX a Hyperlink
    newHyperlinkWithOpening urlLike = do
     openURL <- makeOpenURL urlLike
     liftJ $ do
       x <- newHyperlink urlLike
       x <.> setOnButtonAction openURL
       return x

    --TODO: OpenJDK8 + OpenJFX on my ArchLinux occures 'java.lang.ClassNotFoundException: com.sun.deploy.uitoolkit.impl.fx.HostServicesFactory', please see https://bugs.openjdk.java.net/browse/JDK-8160464
    makeOpenURL :: String -> JavaFX a (ActionEvent -> Java (EventHandler ActionEvent) ())
    makeOpenURL urlLike = do
      AppCore _ app <- ask
      return $ \_ -> app <.> getHostServices >- showDocument urlLike
