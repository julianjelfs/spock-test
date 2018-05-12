{-# LANGUAGE OverloadedStrings #-}

module Api where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Aeson.Lens (_String, key)
import Data.ByteString.Lazy
import qualified Data.Text as T
import Network.Wreq

data Endpoints = Endpoints
  { config :: String
  , someOther :: String
  }

data UserContext = UserContext
  { tenantId :: Int
  , domainId :: Int
  , posId :: Int
  }

data Env = Env
  { userContext :: UserContext
  , endpoints :: Endpoints
  }

opts :: ReaderT Env IO Options
opts = do
  (d, p) <- extract <$> ask
  let domain = param "domainId" .~ [parse d]
      pos = param "posId" .~ [parse p]
      labels = param "labels" .~ ["GraphQL"]
  pure $ defaults & (domain . pos . labels)
  where
    parse = T.pack . show
    extract Env {userContext = UserContext {domainId = d, posId = p}} = (d, p)

fetchConfig :: ReaderT Env IO ByteString
fetchConfig = do
  url <- extractUrl <$> ask
  o <- opts
  resp <- liftIO $ getWith o url
  pure $ resp ^. responseBody
  where
    extractUrl Env {endpoints = Endpoints {config = url}} = url

getConfig = runReaderT fetchConfig defaultEnv

--use lenses to get stuff out of this structure
defaultEnv =
  Env
    { userContext = UserContext 1 1 1
    , endpoints =
        Endpoints
          { config =
              "https://m.travelrepublic.co.uk/api2/webconfig/public/getallbyscope"
          , someOther = ""
          }
    }
