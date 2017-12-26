{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}

module Doromochi.Types
  ( DoromochiApp (..)
  ) where

import Java
import JavaFX

data {-# CLASS "io.github.aiya000.DoromochiApp extends javafx.application.Application" #-}
  DoromochiApp = DoromochiApp (Object# DoromochiApp)
  deriving (Class)

type instance Inherits DoromochiApp = '[Application]
