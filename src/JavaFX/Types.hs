{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeFamilies #-}

module JavaFX.Types where

import Java
import Java.Utils

data {-# CLASS "javafx.application.Application" #-} Application =
  Application (Object# Application)
  deriving (Class)

data {-# CLASS "javafx.application.HostServices" #-} HostServices =
  HostServices (Object# HostServices)
  deriving (Class)

data {-# CLASS "javafx.scene.Group" #-} Group =
  Group (Object# Group)
  deriving (Class)

type instance Inherits Group = '[Parent]

data {-# CLASS "javafx.scene.Scene" #-} Scene =
  Scene (Object# Scene)
  deriving (Class)

data {-# CLASS "javafx.scene.shape.Circle" #-} Circle =
  Circle (Object# Circle)
  deriving (Class)

type instance Inherits Circle = '[Shape]

data {-# CLASS "javafx.stage.Stage" #-} Stage =
  Stage (Object# Stage)
  deriving (Class)

type instance Inherits Stage = '[Window]

data {-# CLASS "javafx.stage.Window" #-} Window =
  Window (Object# Window)
  deriving (Class)

data {-# CLASS "javafx.scene.Parent" #-} Parent =
  Parent (Object# Parent)
  deriving (Class)

type instance Inherits Parent = '[Node]

data {-# CLASS "javafx.scene.layout.Pane" #-} Pane =
  Pane (Object# Pane)
  deriving (Class)

type instance Inherits Pane = '[Region]

data {-# CLASS "javafx.scene.layout.Region" #-} Region =
  Region (Object# Region)
  deriving (Class)

type instance Inherits Region = '[Parent]

data {-# CLASS "javafx.scene.layout.StackPane" #-} StackPane =
  StackPane (Object# StackPane)
  deriving (Class)

type instance Inherits StackPane = '[Pane]

data {-# CLASS "javafx.geometry.Pos" #-} Pos =
  Pos (Object# Pos)
  deriving (Class)

type instance Inherits Pos = '[Enum Pos]

data {-# CLASS "javafx.scene.layout.BorderPane" #-} BorderPane =
  BorderPane (Object# BorderPane)
  deriving (Class)

type instance Inherits BorderPane = '[Pane]

data {-# CLASS "javafx.scene.Node" #-} Node =
  Node (Object# Node)
  deriving (Class)

data {-# CLASS "javafx.scene.Node[]" #-} JNodeArray =
  JNodeArray (Object# JNodeArray)
  deriving (Class)

instance JArray Node JNodeArray

data {-# CLASS "javafx.scene.shape.Shape" #-} Shape =
  Shape (Object# Shape)
  deriving (Class)

type instance Inherits Shape = '[Node]

data {-# CLASS "javafx.scene.control.Button" #-} Button =
  Button (Object# Button)
  deriving (Class)

type instance Inherits Button = '[ButtonBase]

data {-# CLASS "javafx.collections.ObservableList" #-} ObservableList a =
  ObservableList (Object# (ObservableList a))
  deriving (Class)

type instance Inherits (ObservableList a) = '[List a, Observable]

data {-# CLASS "javafx.beans.Observable" #-} Observable =
  Observable (Object# Observable)
  deriving (Class)

data {-# CLASS "javafx.event.Event" #-} Event =
  Event (Object# Event)
  deriving (Class)

data {-# CLASS "javafx.event.ActionEvent" #-} ActionEvent =
  ActionEvent (Object# ActionEvent)
  deriving (Class)

type instance Inherits ActionEvent = '[Event]

data {-# CLASS "javafx.event.EventHandler" #-} EventHandler a =
  EventHandler (Object# (EventHandler a))
  deriving (Class)

data {-# CLASS "javafx.event.EventType" #-} EventType =
  EventType (Object# EventType)
  deriving (Class)

data {-# CLASS "javafx.scene.image.ImageView" #-} ImageView =
  ImageView (Object# ImageView)
  deriving (Class)

type instance Inherits ImageView = '[Node]

data {-# CLASS "javafx.scene.image.Image" #-} Image =
  Image (Object# Image)
  deriving (Class)

data {-# CLASS "javafx.scene.control.MenuBar" #-} MenuBar =
  MenuBar (Object# MenuBar)
  deriving (Class)

type instance Inherits MenuBar = '[Control]

data {-# CLASS "javafx.scene.control.Control" #-} Control =
  Control (Object# Control)
  deriving (Class)

type instance Inherits Control = '[Region]

data {-# CLASS "javafx.scene.control.Menu" #-} Menu =
  Menu (Object# Menu)
  deriving (Class)

type instance Inherits Menu = '[MenuItem]

data {-# CLASS "javafx.scene.control.MenuItem" #-} MenuItem =
  MenuItem (Object# MenuItem)
  deriving (Class)

data {-# CLASS "javafx.scene.text.Text" #-} Text =
  Text (Object# Text)
  deriving (Class)

type instance Inherits Text = '[Shape]

data {-# CLASS "javafx.scene.text.Font" #-} Font =
  Font (Object# Font)
  deriving (Class)

data {-# CLASS "javafx.fxml.FXMLLoader" #-} FXMLLoader =
  FXMLLoader (Object# FXMLLoader)
  deriving (Class)

data {-# CLASS "javafx.scene.control.Label" #-} Label =
  Label (Object# Label)
  deriving (Class)

type instance Inherits Label = '[Labeled]

data {-# CLASS "javafx.scene.control.Labeled" #-} Labeled =
  Labeled (Object# Labeled)
  deriving (Class)

type instance Inherits Labeled = '[Control]

data {-# CLASS "javafx.scene.layout.FlowPane" #-} FlowPane =
  FlowPane (Object# FlowPane)
  deriving (Class)

type instance Inherits FlowPane = '[Pane]

data {-# CLASS "javafx.geometry.Orientation" #-} Orientation =
  Orientation (Object# Orientation)
  deriving (Class)

type instance Inherits Orientation = '[Enum Orientation]

data {-# CLASS "javafx.scene.control.Hyperlink" #-} Hyperlink =
  Hyperlink (Object# Hyperlink)
  deriving (Class)

type instance Inherits Hyperlink = '[ButtonBase]

data {-# CLASS "javafx.scene.control.ButtonBase" #-} ButtonBase =
  ButtonBase (Object# ButtonBase)
  deriving (Class)

type instance Inherits ButtonBase = '[Labeled]

data {-# CLASS "javafx.animation.Timeline" #-} Timeline =
  Timeline (Object# Timeline)
  deriving (Class)

type instance Inherits Timeline = '[Animation]

data {-# CLASS "javafx.animation.Animation" #-} Animation =
  Animation (Object# Animation)
  deriving (Class)

data {-# CLASS "javafx.animation.KeyFrame" #-} KeyFrame =
  KeyFrame (Object# KeyFrame)
  deriving (Class)

data {-# CLASS "javafx.animation.KeyFrame[]" #-} JKeyFrameArray =
  JKeyFrameArray (Object# JKeyFrameArray)
  deriving (Class)

instance JArray KeyFrame JKeyFrameArray

data {-# CLASS "javafx.animation.KeyValue" #-} KeyValue =
  KeyValue (Object# KeyValue)
  deriving (Class)

data {-# CLASS "javafx.animation.KeyValue[]" #-} JKeyValueArray =
  JKeyValueArray (Object# JKeyValueArray)
  deriving (Class)

instance JArray KeyValue JKeyValueArray

data {-# CLASS "javafx.util.Duration" #-} Duration =
  Duration (Object# Duration)
  deriving (Class)

data {-# CLASS "javafx.scene.control.TextField" #-} TextField =
  TextField (Object# TextField)
  deriving (Class)

type instance Inherits TextField = '[TextInputControl]

data {-# CLASS "javafx.scene.control.TextInputControl" #-} TextInputControl =
  TextInputControl (Object# TextInputControl)
  deriving (Class)

type instance Inherits TextInputControl = '[Control]

data {-# CLASS "javafx.scene.control.Spinner" #-} Spinner a =
  Spinner (Object# (Spinner a))
  deriving (Class)

type instance Inherits (Spinner a) = '[Control]

data {-# CLASS "javafx.scene.control.SpinnerValueFactory" #-} SpinnerValueFactory a =
  SpinnerValueFactory (Object# (SpinnerValueFactory a))
  deriving (Class)
