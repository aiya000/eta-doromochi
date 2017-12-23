{-# LANGUAGE FlexibleContexts #-}

--TODO: Integrate this module and JavaFX.Methods

module JavaFX.Methods where

import Java
import Java.Doromochi
import JavaFX.Types

foreign import java unsafe "@new" newScene ::
  Extends a Parent => a -> Double -> Double -> Java b Scene

-- | Similar to 'newScene' but the size of 'Scene' is not specified
foreign import java unsafe "@new" newSceneWithoutSize ::
  Extends a Parent => a -> Java b Scene

foreign import java unsafe "@new" newStackPane ::
  Java a StackPane

-- | Similar to 'newScene', with the contents
foreign import java unsafe "@new" newStackPane' ::
  [Node] -> Java a StackPane

foreign import java unsafe "setAlignment" setAlignment ::
  Pos -> Java StackPane ()

foreign import java unsafe "@static @field javafx.geometry.Pos.BASELINE_CENTER" baseLineCenter
  :: Pos

foreign import java unsafe "@static @field javafx.geometry.Pos.BASELINE_LEFT" baseLineLeft
  :: Pos

foreign import java unsafe "@static @field javafx.geometry.Pos.BASELINE_RIGHT" baseLineRight
  :: Pos

foreign import java unsafe "@static @field javafx.geometry.Pos.BOTTOM_CENTER" bottomCenter
  :: Pos

foreign import java unsafe "@static @field javafx.geometry.Pos.BOTTOM_LEFT" bottomLeft
  :: Pos

foreign import java unsafe "@static @field javafx.geometry.Pos.BOTTOM_RIGHT" bottomRight
  :: Pos

foreign import java unsafe "@static @field javafx.geometry.Pos.CENTER" center
  :: Pos

foreign import java unsafe "@static @field javafx.geometry.Pos.CENTER_LEFT" centerLeft
  :: Pos

foreign import java unsafe "@static @field javafx.geometry.Pos.CENTER_RIGHT" centerRight
  :: Pos

foreign import java unsafe "@static @field javafx.geometry.Pos.TOP_CENTER" topCenter
  :: Pos

foreign import java unsafe "@static @field javafx.geometry.Pos.TOP_LEFT" topLeft
  :: Pos

foreign import java unsafe "@static @field javafx.geometry.Pos.TOP_RIGHT" topRight
  :: Pos

foreign import java unsafe "@new" newCircle ::
  Double -> Double -> Double -> Java c Circle

foreign import java unsafe "@new" newGroup ::
  Java c Group

-- launch eventually calls back into Haskell land so it should be marked 'safe'
foreign import java safe "@static javafx.application.Application.launch" launch ::
  JClass a -> JStringArray -> IO ()

foreign import java unsafe "getChildren" getChildren ::
  Extends c Parent => Java c (ObservableList a)

foreign import java unsafe "@interface add" addChild ::
  Extends a Object => a -> Java (ObservableList a) Bool

foreign import java unsafe "show" showStage ::
  Java Stage ()

foreign import java unsafe "setTitle" setTitle ::
  String -> Java Stage ()

foreign import java unsafe "setScene" setScene ::
  Scene -> Java Stage ()

foreign import java unsafe "@wrapper handle" action ::
  Extends a Event => (a -> Java (EventHandler a) ()) -> EventHandler a

foreign import java unsafe "@new" newEventType ::
  String -> Java a EventType

foreign import java unsafe "@new" newButton ::
  String -> Java c Button

foreign import java unsafe "setOnAction" setOnButtonAction' ::
  EventHandler ActionEvent -> Java Button ()

setOnButtonAction :: (ActionEvent -> Java (EventHandler ActionEvent) ()) -> Java Button ()
setOnButtonAction = setOnButtonAction' . action

foreign import java unsafe "@new" newImageView ::
  Image -> Java a ImageView

foreign import java unsafe "setFitHeight" setFitHeight ::
  Double -> Java ImageView ()

foreign import java unsafe "setFitWidth" setFitWidth ::
  Double -> Java ImageView ()

foreign import java unsafe "@new" newImage ::
  FilePath -> Java a Image

foreign import java unsafe "@new" newMenuBar ::
  Java a MenuBar

foreign import java unsafe "@new" newMenu ::
  String -> Java a Menu

foreign import java unsafe "getItems" getMenuItems ::
  Java Menu (ObservableList MenuItem)

foreign import java unsafe "@new" newMenuItem ::
  String -> Java a MenuItem

foreign import java unsafe "setOnAction" setOnMenuItemAction' ::
  EventHandler ActionEvent -> Java MenuItem ()

setOnMenuItemAction :: (ActionEvent -> Java (EventHandler ActionEvent) ()) -> Java MenuItem ()
setOnMenuItemAction = setOnMenuItemAction' . action

foreign import java unsafe "getMenus" getMenus ::
  Java MenuBar (ObservableList Menu)

foreign import java unsafe "@new" newText ::
  String -> Java a Text

-- | With the size of 'Font'
foreign import java unsafe "@new" newFont ::
  Double -> Java a Font

foreign import java unsafe "setFont" setFont ::
  Font -> Java Text ()

foreign import java unsafe "@static javafx.fxml.FXMLLoader.load" fXMLLoad ::
  Extends c Object => URL -> Java a c

foreign import java unsafe "getTop" getTop ::
  Java BorderPane Node

foreign import java unsafe "getLeft" getLeft ::
  Java BorderPane Node

foreign import java unsafe "getRight" getRight ::
  Java BorderPane Node

foreign import java unsafe "getBottom" getBottom ::
  Java BorderPane Node

foreign import java unsafe "getCenter" getCenter ::
  Java BorderPane Node

foreign import java unsafe "getImage" getImage ::
  Java ImageView Image

foreign import java unsafe "@new" newLabel ::
  String -> Java a Label

foreign import java unsafe "@new" newFlowPane ::
  Java a FlowPane

foreign import java unsafe "setOrientation" setOrientation ::
  Orientation -> Java FlowPane ()

foreign import java unsafe "@static @field javafx.geometry.Orientation.VERTICAL" verticalOrient ::
  Orientation

foreign import java unsafe "@static @field javafx.geometry.Orientation.HORIZONTAL" horizontalOrient ::
  Orientation
