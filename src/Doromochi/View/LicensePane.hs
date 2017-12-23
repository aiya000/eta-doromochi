-- | Present 'LicensePane' by 'newLicensePane'
module Doromochi.View.LicensePane where

import Control.Monad (forM_)
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
newLicensePane :: Java a LicensePane
newLicensePane = do
  doromochi <- newLabel "ドロもち"
  bar <- newLabel "- - -"
  aboutLicense <- newLabel "This software includes the work that is distributed in the Apache License 2.0."
  bar' <- newLabel "- - -"
  thisAppDepends <- newLabel "This software depends below softwares"
  etaExamples <- newLabel "typelead/eta-examples"
  self <- newFlowPane
  self <.> setOrientation verticalOrient
  let nodes = [ doromochi
              , bar
              , aboutLicense
              , bar'
              , thisAppDepends
              , etaExamples
              ]
  forM_ nodes $ \node ->
    self <.> getChildren >- addChild node
  return self
