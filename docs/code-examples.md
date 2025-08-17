# Code Examples for GitHub Sync Implementation

## Core.Types Module

```haskell
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Core.Types where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)

-- GitHub Webhook Payload Types
data WebhookPayload = WebhookPayload
  { ref :: Text
  , repository :: Repository
  , pusher :: Pusher
  } deriving (Show, Generic)

instance FromJSON WebhookPayload

data Repository = Repository
  { name :: Text
  , fullName :: Text
  , private :: Bool
  } deriving (Show, Generic)

instance FromJSON Repository

data Pusher = Pusher
  { pusherName :: Text
  , email :: Text
  } deriving (Show, Generic)

instance FromJSON Pusher

-- GitHub API Types
data TreeItem = TreeItem
  { path :: Text
  , mode :: Text
  , type_ :: Text
  , size :: Maybe Int
  , sha :: Text
  } deriving (Show, Generic)

instance FromJSON TreeItem where
  parseJSON = withObject "TreeItem" $ \v -> TreeItem
    <$> v .: "path"
    <*> v .: "mode"
    <*> v .: "type"
    <*> v .:? "size"
    <*> v .: "sha"

-- Output JSON Types
data RepositoryData = RepositoryData
  { repoInfo :: RepoInfo
  , files :: [FileInfo]
  , totalFiles :: Int
  , generatedAt :: UTCTime
  } deriving (Show, Generic)

instance ToJSON RepositoryData

data RepoInfo = RepoInfo
  { repoName :: Text
  , owner :: Text
  , updatedAt :: UTCTime
  } deriving (Show, Generic)

instance ToJSON RepoInfo

data FileInfo = FileInfo
  { filePath :: Text
  , fileName :: Text
  , fileSize :: Int
  , fileSha :: Text
  , fileType :: Text
  , lastModified :: UTCTime
  } deriving (Show, Generic)

instance ToJSON FileInfo

-- Configuration Types
data AppConfig = AppConfig
  { githubToken :: Text
  , webhookSecret :: Text
  , repoOwner :: Text
  , repoName :: Text
  , outputPath :: FilePath
  , logLevel :: LogLevel
  , port :: Int
  } deriving (Show)

data LogLevel = Debug | Info | Warning | Error
  deriving (Show, Read, Eq, Ord)
```

## Api.Webhook Module

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Api.Webhook where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Network.HTTP.Types.Status
import Web.Scotty

import Core.Types
import Security.GitHub (verifySignature)
import Client.GitHub (fetchRepositoryTree)
import Generator.Repository (generateRepositoryJSON)
import Utils.FileSystem (saveJSON)
import Utils.Logger (logInfo, logError)

webhookHandler :: AppConfig -> ActionM ()
webhookHandler config = do
  -- Get headers and body
  signature <- header "X-Hub-Signature-256"
  event <- header "X-GitHub-Event"
  body <- body
  
  case (signature, event) of
    (Just sig, Just "push") -> do
      -- Verify signature
      let isValid = verifySignature (webhookSecret config) body sig
      
      if isValid
        then processWebhook config body
        else do
          liftIO $ logError "Invalid webhook signature"
          status unauthorized401
          json $ object ["error" .= ("Invalid signature" :: Text)]
    
    (_, Just evt) -> do
      liftIO $ logInfo $ "Ignoring event: " <> evt
      status ok200
      json $ object ["status" .= ("ignored" :: Text)]
    
    _ -> do
      status badRequest400
      json $ object ["error" .= ("Missing required headers" :: Text)]

processWebhook :: AppConfig -> LBS.ByteString -> ActionM ()
processWebhook config body = do
  case decode body :: Maybe WebhookPayload of
    Just payload -> do
      -- Check if push is to main branch
      if ref payload == "refs/heads/main"
        then do
          liftIO $ logInfo "Processing push to main branch"
          result <- liftIO $ syncRepository config
          
          case result of
            Right _ -> do
              status ok200
              json $ object ["status" .= ("success" :: Text)]
            Left err -> do
              liftIO $ logError $ "Sync failed: " <> err
              status internalServerError500
              json $ object ["error" .= err]
        else do
          liftIO $ logInfo $ "Ignoring push to: " <> ref payload
          status ok200
          json $ object ["status" .= ("ignored" :: Text)]
    
    Nothing -> do
      status badRequest400
      json $ object ["error" .= ("Invalid payload" :: Text)]

syncRepository :: AppConfig -> IO (Either Text ())
syncRepository config = do
  -- Fetch repository tree from GitHub
  treeResult <- fetchRepositoryTree config
  
  case treeResult of
    Left err -> return $ Left err
    Right tree -> do
      -- Generate JSON from tree
      let jsonData = generateRepositoryJSON config tree
      
      -- Save to file system
      saveResult <- saveJSON config jsonData
      
      case saveResult of
        Left err -> return $ Left err
        Right _ -> do
          logInfo "Repository sync completed successfully"
          return $ Right ()
```

## Security.GitHub Module

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Security.GitHub where

import Crypto.Hash.Algorithms (SHA256)
import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

verifySignature :: Text -> LBS.ByteString -> Text -> Bool
verifySignature secret body signature = 
  let secretBS = TE.encodeUtf8 secret
      bodyBS = LBS.toStrict body
      
      -- Remove "sha256=" prefix from signature
      sigWithoutPrefix = T.drop 7 signature
      
      -- Decode hex signature
      (decodedSig, _) = Base16.decode $ TE.encodeUtf8 sigWithoutPrefix
      
      -- Calculate HMAC
      calculatedHmac :: HMAC SHA256
      calculatedHmac = hmac secretBS bodyBS
      
      -- Get digest as ByteString
      calculatedDigest = hmacGetDigest calculatedHmac
      
  in constantTimeEq decodedSig calculatedDigest

-- Constant time comparison to prevent timing attacks
constantTimeEq :: ByteString -> ByteString -> Bool
constantTimeEq a b =
  BS.length a == BS.length b &&
  BS.foldl' (\acc (x, y) -> acc && x == y) True (BS.zip a b)
```

## Client.GitHub Module

```haskell
{-# LANGUAGE OverloadedStrings #-}

module Client.GitHub where

import Control.Exception (try)
import Data.Aeson (decode)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status

import Core.Types
import Utils.Logger (logDebug, logError)

fetchRepositoryTree :: AppConfig -> IO (Either Text [TreeItem])
fetchRepositoryTree config = do
  manager <- newManager tlsManagerSettings
  
  let url = T.unpack $ T.concat
        [ "https://api.github.com/repos/"
        , repoOwner config
        , "/"
        , repoName config
        , "/git/trees/main?recursive=1"
        ]
  
  initialRequest <- parseRequest url
  
  let request = initialRequest
        { requestHeaders = 
            [ (hAuthorization, "Bearer " <> TE.encodeUtf8 (githubToken config))
            , (hUserAgent, "notes-viewer-backend")
            , ("Accept", "application/vnd.github.v3+json")
            ]
        }
  
  logDebug $ "Fetching repository tree from: " <> T.pack url
  
  result <- try $ httpLbs request manager
  
  case result of
    Left (e :: HttpException) -> do
      logError $ "GitHub API request failed: " <> T.pack (show e)
      return $ Left "Failed to fetch repository data"
    
    Right response -> do
      let status = responseStatus response
      
      if statusIsSuccessful status
        then case decode (responseBody response) of
          Just treeData -> do
            logDebug $ "Fetched " <> T.pack (show $ length $ tree treeData) <> " items"
            return $ Right $ filter (\item -> type_ item == "blob") $ tree treeData
          Nothing -> do
            logError "Failed to parse GitHub API response"
            return $ Left "Invalid response format"
        else do
          logError $ "GitHub API returned status: " <> T.pack (show $ statusCode status)
          return $ Left $ "GitHub API error: " <> T.pack (show $ statusCode status)

-- Helper type for parsing tree response
data TreeResponse = TreeResponse
  { tree :: [TreeItem]
  } deriving (Generic)

instance FromJSON TreeResponse
```

## Example Scotty Route Setup

```haskell
-- In Api.Router.hs
module Api.Router where

import Web.Scotty
import Api.Webhook (webhookHandler)
import Core.Types (AppConfig)

setupRouter :: AppConfig -> ScottyM ()
setupRouter config = do
  -- Health check endpoint
  get "/health" $ do
    json $ object ["status" .= ("healthy" :: Text)]
  
  -- GitHub webhook endpoint
  post "/webhook/github" $ webhookHandler config
  
  -- Existing routes
  get "/notes/all" $ do
    -- This will read from the generated JSON file
    jsonFile <- liftIO $ readJSON (outputPath config </> "repository-data.json")
    case jsonFile of
      Just content -> json content
      Nothing -> do
        status notFound404
        json $ object ["error" .= ("No data available" :: Text)]
```

## Usage Example

```haskell
-- In Main.hs
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Core.Config (loadConfig)
import Api.Router (setupRouter)
import Utils.Logger (initLogger, logInfo)

main :: IO ()
main = do
  -- Load configuration
  config <- loadConfig
  
  -- Initialize logger
  initLogger (logLevel config)
  
  logInfo $ "Starting server on port " <> T.pack (show $ port config)
  
  -- Start Scotty with configured routes
  scotty (port config) $ setupRouter config
```

## Testing the Webhook Locally

```bash
# 1. Start ngrok
ngrok http 3000

# 2. Update GitHub webhook URL with ngrok URL
# https://xxxxx.ngrok.io/webhook/github

# 3. Make a test push to your repository
git commit --allow-empty -m "Test webhook"
git push origin main

# 4. Check logs and output file
cat ./data/json/repository-data.json | jq .