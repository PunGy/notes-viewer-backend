{-# LANGUAGE OverloadedStrings, DuplicateRecordFields #-}

module Core.Types (LogLevel(..), AppConfig(..)) where

import Data.Text (Text)

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
  } deriving (Show)
