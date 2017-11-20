module JavaFX.Core
  ( javafx
  ) where

import Java
import JavaFX.Methods (launch)
import System.Environment (getJavaArgs)

javafx :: Class a => Proxy a -> IO ()
javafx p = do
  jargs <- getJavaArgs
  launch (getClass p) jargs
