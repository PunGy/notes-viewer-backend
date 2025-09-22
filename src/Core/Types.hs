{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.Types (File (..), LogLevel (..), AppConfig (..)) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics

-- Configuration

data LogLevel = Debug | Info | Warning | Error
  deriving (Show, Eq, Ord, Read)

data AppConfig = AppConfig
  { githubToken :: Text
  , webhookSecret :: Text
  , repoOwner :: Text
  , repoName :: Text
  , outputPath :: FilePath
  , logLevel :: LogLevel
  , port :: Int
  }
  deriving (Show)

data File = File
  { url :: Text
  , filename :: Text
  , size :: Maybe Int
  }
  deriving (Show, Generic)

instance ToJSON File
