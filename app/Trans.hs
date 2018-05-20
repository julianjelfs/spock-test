module Trans where

import           Control.Exception
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader

-- maybeList :: ReaderT Int (ExceptT String IO) Int
-- maybeList = do
--   n   <- ask
--   inp <- try . div n . read <$> getLine
--   pure inp


run = runExceptT (runReaderT mightThrow 10)


--trying to understand nested monad transformers

mightThrow :: ReaderT Int (ExceptT String IO) Int
mightThrow = do
  d <- ask
  liftIO $ print d
  a <- liftIO $ read <$> getLine
  b <- liftIO $ read <$> getLine
  if a == 0 then lift $ throwE "zero" else pure $ a `div` b
