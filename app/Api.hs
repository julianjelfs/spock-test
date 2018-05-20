{-# LANGUAGE OverloadedStrings #-}

module Api
  ( fetchWebConfig
  )
where

import           Control.Exception             as E
import           Control.Lens
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import qualified Data.Aeson                    as A
import qualified Data.Text                     as T
import           Env
import qualified Network.Wreq                  as W
import           WebConfig

type App a = ReaderT Env IO a

opts :: App W.Options
opts = do
  (d, p) <- extract <$> ask
  let domain = W.param "domainId" .~ [parse d]
      pos    = W.param "posId" .~ [parse p]
      labels = W.param "labels" .~ ["GraphQL"]
  pure $ W.defaults & (domain . pos . labels)
 where
  parse = T.pack . show
  extract e = (e ^. userContext . domainId, e ^. userContext . posId)

fetchWebConfig :: App (Maybe WebConfig)
fetchWebConfig = do
  url  <- configUrl <$> ask
  o    <- opts
  resp <- liftIO (W.getWith o url `E.catch` onError)
  pure $ A.decode $ resp ^. W.responseBody
 where
  configUrl e = e ^. endpoints . config
  onError e@(SomeException s) = do
    putStrLn "Not sure how we would go about handling this"
    throwIO e
--testJson = BS.readFile "./app/test.json"
