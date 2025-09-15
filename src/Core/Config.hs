{-# LANGUAGE OverloadedStrings #-}

module Core.Config (configParser, loadConfig) where

import Core.Types (AppConfig (..), LogLevel (..))
import Env
import Utils.Env (loadEnv)

configParser :: Parser Error AppConfig
configParser =
  AppConfig
    <$> var (str) "GITHUB_TOKEN" (help "GitHub Personal Access Token")
    <*> var (str) "GITHUB_WEBHOOK_SECRET" (help "Secret for validating GitHub webhooks")
    <*> var (str) "GITHUB_REPO_OWNER" (help "Owner of the GitHub repository")
    <*> var (str) "GITHUB_REPO_NAME" (help "Name of the GitHub repository")
    <*> var (str) "JSON_OUTPUT_PATH" (help "File path for JSON output")
    <*> var (auto) "LOG_LEVEL" (help "Logging level (Debug, Info, Warning, Error)" <> def Info)
    <*> var (auto) "PORT" (help "Port for the web server to listen on")

loadConfig :: IO (AppConfig)
loadConfig = do
  _ <- loadEnv ".env"
  parse (header "AppConfig") configParser
