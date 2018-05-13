{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}

module Env where

import Control.Lens

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
