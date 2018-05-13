{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Spock
import Web.Spock.Config
import Web.Spock.Lucid (lucid)

import Api (fetchWebConfig)
import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Data.IORef
import Data.Monoid
import qualified Data.Text as T
import Env (defaultEnv)
import Views.Home
import WebConfig (WebConfig)

data MySession =
  EmptySession

newtype MyAppState =
  DummyAppState (IORef Int)

--loadConfig :: IO String
loadConfig = do
  config <- readFile "./app/config/config.json"
  putStrLn "loaded some data"
  --pure $ decode config
  pure ""

getWebConfig :: IO (Maybe WebConfig)
getWebConfig = runReaderT fetchWebConfig defaultEnv

main :: IO ()
main = do
  ref <- newIORef 0
  spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
  runSpock 8080 (spock spockCfg app)

app :: SpockM () MySession MyAppState ()
app = do
  get root $ lucid homeView
  get ("hello" <//> var) $ \name -> do
    (DummyAppState ref) <- getState
    visitorNumber <- liftIO $ atomicModifyIORef' ref $ \i -> (i + 1, i + 1)
    text
      ("Hello " <> name <> ", you are visitor number " <>
       T.pack (show visitorNumber))
