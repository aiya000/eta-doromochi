-- | General things for monads
module Control.Monad.Doromochi where

-- |
-- Share an argument between two functions
--
-- `
-- startButton <.> setOnButtonAction (intentToPomodoroPane .>. startClock')
-- `
(.>.) :: Monad m => (a -> m b) -> (a -> m c) -> a -> m c
(.>.) f g x = f x >> g x
