{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Api where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Aeson (Value)

--import Data.Aeson.Lens (_String, key)
import Data.Map as Map
import qualified Data.Text as T
import Network.Wreq

type Resp = Response (Map String Value)

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

opts :: App Options
opts = do
  (d, p) <- extract <$> ask
  let domain = param "domainId" .~ [parse d]
      pos = param "posId" .~ [parse p]
      labels = param "labels" .~ ["GraphQL"]
  pure $ defaults & (domain . pos . labels)
  where
    parse = T.pack . show
    extract e = (e ^. userContext . domainId, e ^. userContext . posId)

fetchConfig :: App (Map String Value)
fetchConfig = do
  url <- configUrl <$> ask
  o <- opts
  resp <- liftIO $ asJSON =<< getWith o url
  pure $ resp ^. responseBody
  where
    configUrl e = e ^. endpoints . config

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
