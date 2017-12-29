{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Doromochi.JavaFX
  ( AppCore (..)
  , JavaFX (..)
  , runJavaFX
  , liftJ
  ) where

import Control.Monad.Reader (ReaderT(..), MonadReader)
import Java
import JavaFX

-- | These are needed in the javafx application
data AppCore = AppCore
  { primStage   :: Stage       -- ^ This is the only existing in this app
  , application :: Application -- ^ This is the only existing too
  }

-- | Store resources that depends the javafx application
newtype JavaFX a b = JavaFX
  { unJavaFX :: ReaderT AppCore (Java a) b
  } deriving ( Functor
             , Applicative
             , Monad
             , MonadReader AppCore
             )

-- | Extract `'JavaFX' a`
runJavaFX :: JavaFX a b -> AppCore -> Java a b
runJavaFX context x = flip runReaderT x $ unJavaFX context

-- | Lift up a `'Java a'` as the `'JavaFX a'`
liftJ :: Java a b -> JavaFX a b
liftJ x = JavaFX . ReaderT $ const x
