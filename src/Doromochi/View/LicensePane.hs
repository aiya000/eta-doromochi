-- | Present 'LicensePane' by 'newLicensePane'
module Doromochi.View.LicensePane where

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
newLicensePane :: Application -> Java a LicensePane
newLicensePane app = do
  doromochi <- newLabel "ドロもち"
  imageAuthor <- newLabel "The copyright for the zunko images in this software is owned by @HassakuTb on （ず・ω・きょ）"
  imageLink <- newHyperlinkWithOpening app "https://github.com/aiya000/eta-doromochi/blob/master/images"
  hassakuLink <- newHyperlinkWithOpening app "https://twitter.com/HassakuTb"
  zunkyoLink <- newHyperlinkWithOpening app "https://zunko.jp/guideline.html"
  aboutLicense <- newLabel "Also this software includes the work that is distributed in the Apache License 2.0."
  bar <- newLabel "- - -"
  thisAppDepends <- newLabel "This software depends below softwares"
  emptyLine <- newLabel ""
  etaExamples <- newLabel "typelead/eta-examples"
  etaExamplesLink <- newHyperlinkWithOpening app "https://github.com/typelead/eta-examples"
  newFlowPane verticalOrient [ superCast doromochi
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
    newHyperlinkWithOpening :: Application -> String -> Java a Hyperlink
    newHyperlinkWithOpening app urlLike = do
      x <- newHyperlink urlLike
      x <.> setOnButtonAction (openURL urlLike app)
      return x

    --TODO: OpenJDK8 + OpenJFX on my ArchLinux occures 'java.lang.ClassNotFoundException: com.sun.deploy.uitoolkit.impl.fx.HostServicesFactory', please see https://bugs.openjdk.java.net/browse/JDK-8160464
    openURL :: String -> Application -> (ActionEvent -> Java (EventHandler ActionEvent) ())
    openURL urlLike app = \_ -> app <.> getHostServices >- showDocument urlLike
