{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson
import Lucid
import Web.Spock
import Web.Spock.Config
import Web.Spock.Lucid (lucid)

import Control.Monad.Trans
import Data.IORef
import Data.Monoid
import qualified Data.Text as T

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

main :: IO ()
main = do
  ref <- newIORef 0
  spockCfg <- defaultSpockCfg EmptySession PCNoDatabase (DummyAppState ref)
  runSpock 8080 (spock spockCfg app)

app :: SpockM () MySession MyAppState ()
app = do
  get root homeView
  get ("hello" <//> var) $ \name -> do
    (DummyAppState ref) <- getState
    visitorNumber <- liftIO $ atomicModifyIORef' ref $ \i -> (i + 1, i + 1)
    text
      ("Hello " <> name <> ", you are visitor number " <>
       T.pack (show visitorNumber))

homeView =
  lucid $ do
    h1_ "Hello!"
    p_ "How are you today?"
