{-# LANGUAGE FlexibleContexts #-}

module JavaFX.Methods
  ( newScene
  , newCircle
  , newGroup
  , launch
  , getChildren
  , addChild
  , showStage
  , setTitle
  , setScene
  , action
  , newButton
  , setOnAction
  ) where

import Java
import JavaFX.Types

foreign import java unsafe "@new"
  newScene :: Extends a Parent => a -> Double -> Double -> Java b Scene

foreign import java unsafe "@new"
  newCircle :: Double -> Double -> Double -> Java c Circle

foreign import java unsafe "@new"
  newGroup :: Java c Group

-- launch eventually calls back into Haskell land so it should be marked 'safe'
foreign import java safe "@static javafx.application.Application.launch"
  launch :: JClass a -> JStringArray -> IO ()

foreign import java unsafe "getChildren"
  getChildren :: Extends c Parent => Java c ObservableList

foreign import java unsafe "@interface add"
  addChild :: Extends a Object => a -> Java ObservableList Bool

foreign import java unsafe "show"
  showStage :: Java Stage ()

foreign import java unsafe "setTitle"
  setTitle :: String -> Java Stage ()

foreign import java unsafe "setScene"
  setScene :: Scene -> Java Stage ()

-- Button-related stuff
foreign import java unsafe "@wrapper handle"
  action :: Extends a Event => (a -> Java (EventHandler a) ()) -> EventHandler a

foreign import java unsafe "@new"
  newButton :: String -> Java c Button

foreign import java unsafe "setOnAction"
  setOnAction' :: EventHandler ActionEvent -> Java Button ()

setOnAction :: (ActionEvent -> Java (EventHandler ActionEvent) ()) -> Java Button ()
setOnAction = setOnAction' . action
