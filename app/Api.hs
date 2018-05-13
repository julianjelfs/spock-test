{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Api where

import Control.Exception as E
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.Aeson.Types ((.:))
import qualified Data.ByteString.Lazy as BS
import Data.Map as Map
import qualified Data.Text as T
import qualified Network.Wreq as W

type Resp = W.Response (Map String A.Value)

data Endpoints = Endpoints
  { _config :: String
  , _someOther :: String
  } deriving (Show)

data UserContext = UserContext
  { _tenantId :: Int
  , _domainId :: Int
  , _posId :: Int
  } deriving (Show)

data Env = Env
  { _userContext :: UserContext
  , _endpoints :: Endpoints
  } deriving (Show)

makeLenses ''Env

makeLenses ''UserContext

makeLenses ''Endpoints

type App a = ReaderT Env IO a

opts :: App W.Options
opts = do
  (d, p) <- extract <$> ask
  let domain = W.param "domainId" .~ [parse d]
      pos = W.param "posId" .~ [parse p]
      labels = W.param "labels" .~ ["GraphQL"]
  pure $ W.defaults & (domain . pos . labels)
  where
    parse = T.pack . show
    extract e = (e ^. userContext . domainId, e ^. userContext . posId)

fetchConfig :: App (Maybe Config)
fetchConfig = do
  url <- configUrl <$> ask
  o <- opts
  resp <- liftIO (W.getWith o url `E.catch` onError)
  pure $ A.decode $ resp ^. W.responseBody
  where
    configUrl e = e ^. endpoints . config
    onError e@(SomeException s) = do
      putStrLn "Not sure how we would go about handling this"
      throwIO e

getConfig = runReaderT fetchConfig defaultEnv

defaultEnv =
  Env
    { _userContext = UserContext 1 1 1
    , _endpoints =
        Endpoints
          { _config =
              "https://m.travelrepublic.co.uk/api2/webconfig/public/getallbyscope"
          , _someOther = ""
          }
    }

--data structure for actually holding the config
data Config = Config
  { publishedVersion :: Integer
  , version :: Integer
  } deriving (Show)

instance A.FromJSON Config where
  parseJSON = configParser

configParser :: A.Value -> A.Parser Config
configParser =
  A.withObject "config" $ \obj -> do
    version <- obj .: "Version"
    publishedVersion <- obj .: "PublishedVersion"
    pure Config {publishedVersion = publishedVersion, version = version}
