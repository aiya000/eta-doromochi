{-# LANGUAGE MagicHash #-}

module Doromochi.Main
  ( defaultMain
  ) where

import Doromochi.Types (DoromochiApp)
import Java (Proxy(..))
import JavaFX (javafx)

defaultMain :: IO ()
defaultMain = javafx (Proxy :: Proxy DoromochiApp)
