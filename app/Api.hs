{-# LANGUAGE OverloadedStrings #-}

module Api where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Data.Aeson.Lens (_String, key)
import Data.ByteString.Lazy
import qualified Data.Text as T
import Network.Wreq

type Tenant = Int

type Domain = Int

type Pos = Int

type Env = (Tenant, Domain, Pos)

url = "https://m.travelrepublic.co.uk/api2/webconfig/public/getallbyscope"

opts :: ReaderT Env IO Options
opts = do
  (t, d, p) <- ask
  let domain = param "domainId" .~ [parse d]
      pos = param "posId" .~ [parse p]
      labels = param "labels" .~ ["GraphQL"]
  pure $ defaults & (domain . pos . labels)
  where
    parse = T.pack . show

fetchConfig :: ReaderT Env IO ByteString
fetchConfig = do
  o <- opts
  resp <- liftIO $ getWith o url
  pure $ resp ^. responseBody

getConfig = runReaderT fetchConfig (1, 1, 1)
