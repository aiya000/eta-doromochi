{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Define the classes of Java that is used in doromochi
module Java.Doromochi where

import Java

data {-# CLASS "java.io.File" #-} File =
  File (Object# File)
  deriving (Class)

foreign import java unsafe "@new" newFile ::
  FilePath -> Java a File

foreign import java unsafe "toURI" toURI ::
  Java File URI


data {-# CLASS "java.net.URI" #-} URI =
  URI (Object# URI)
  deriving (Class)

foreign import java unsafe "toURL" toURL ::
  Java URI URL


data {-# CLASS "java.net.URL" #-} URL =
  URL (Object# URL)
  deriving (Class)


foreign import java unsafe "getResource" getResource ::
  Class a => String -> Java (JClass a) URL


data {-# CLASS "java.awt.Desktop" #-} Desktop =
  Desktop (Object# Desktop)
  deriving (Class)

foreign import java unsafe "@static java.awt.Desktop.getDesktop" getDesktop ::
  Java a Desktop

foreign import java unsafe "browse" browse ::
  URI -> Java Desktop ()

foreign import java unsafe "open" open ::
  File -> Java Desktop ()


data {-# CLASS "java.lang.reflect.Method" #-} Method =
  Method (Object# Method)
  deriving (Class)

data {-# CLASS "java.lang.reflect.Method[]" #-} JMethodArray =
  JMethodArray (Object# JMethodArray)
  deriving (Class)

instance JArray Method JMethodArray

foreign import java unsafe "getClass" getClass' ::
  Extends a Object => Java a (JClass a)

foreign import java unsafe "getDeclaredMethods" getDeclaredMethods' ::
  Java (JClass a) JMethodArray

getDeclaredMethods :: Java (JClass a) [Method]
getDeclaredMethods = do
  methods <- getDeclaredMethods'
  methods <.> arrayToList

foreign import java unsafe "getName" getMethodName ::
  Java Method String
