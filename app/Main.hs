{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Web.Spock
import Web.Spock.Config
import Web.Spock.Lucid (lucid)

import Api (fetchWebConfig)
import Control.Concurrent
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

data Config = Config
  { x :: Int
  , y :: Int
  } deriving (Show)

config :: IO (MVar Config)
config = newMVar (Config 0 0)

increment :: Config -> Config
increment Config {..} = Config (x + 1) (y + 1)

poll :: IO ()
poll = do
  mv <- newIORef (Config 0 0)
  threadId <- forkIO (checkStuff mv)
  pure ()

checkStuff :: IORef Config -> IO ()
checkStuff mv = do
  putStrLn "polling..."
  threadDelay 1000000
  cfg@Config {..} <- readIORef mv
  if x < 10
    then do
      print (show cfg)
      modifyIORef mv increment
      checkStuff mv
    else print "We have finished"

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
