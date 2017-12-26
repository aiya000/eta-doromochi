{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MagicHash #-}

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
