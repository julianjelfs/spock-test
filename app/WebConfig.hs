{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module WebConfig where

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import Data.Aeson.Types ((.:))
import Data.Foldable (asum)

data WebConfig = WebConfig
  { publishedVersion :: Integer
  , version :: Integer
  , settings :: [WebConfigSetting]
  } deriving (Show)

data WebConfigSetting = WebConfigSetting
  { name :: String
  , value :: WebConfigValue
  } deriving (Show)

data WebConfigValue
  = StringWebConfigValue String
  | IntWebConfigValue Int
  | BoolWebConfigValue Bool
  deriving (Show)

instance A.FromJSON WebConfig where
  parseJSON = webConfigParser

instance A.FromJSON WebConfigSetting where
  parseJSON = webConfigSettingParser

instance A.FromJSON WebConfigValue where
  parseJSON = webConfigValueParser

webConfigParser :: A.Value -> A.Parser WebConfig
webConfigParser =
  A.withObject "config" $ \obj -> do
    version <- obj .: "Version"
    publishedVersion <- obj .: "PublishedVersion"
    settings <- obj .: "Settings"
    pure WebConfig {..}

webConfigSettingParser :: A.Value -> A.Parser WebConfigSetting
webConfigSettingParser =
  A.withObject "config setting" $ \obj -> do
    name <- obj .: "Name"
    value <- obj .: "Value"
    pure WebConfigSetting {..}

webConfigValueParser :: A.Value -> A.Parser WebConfigValue
webConfigValueParser v =
  asum
    [ StringWebConfigValue <$> A.parseJSON v
    , IntWebConfigValue <$> A.parseJSON v
    , BoolWebConfigValue <$> A.parseJSON v
    ]
