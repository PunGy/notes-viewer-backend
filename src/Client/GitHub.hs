{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Client.GitHub (fetchRepository) where

import Control.Exception
import Control.Monad.Reader
import Core.App (HandlerM)
import Core.Types
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Header
import Network.HTTP.Types.Status
import Utils.Logger (logDebug, logError)

data TreeItem = TreeItem
  { itemPath :: T.Text
  , itemMode :: T.Text
  , itemType :: T.Text
  , itemSha :: T.Text
  , itemSize :: Maybe Int
  , itemUrl :: T.Text
  }
  deriving (Show, Generic)

instance FromJSON TreeItem where
  parseJSON = withObject "TreeItem" $ \v ->
    TreeItem
      <$> v .: "path"
      <*> v .: "mode"
      <*> v .: "type"
      <*> v .: "sha"
      <*> v .:? "size"
      <*> v .: "url"

makeFile :: TreeItem -> File
makeFile item =
  File
    { url = (itemUrl item)
    , filename = (itemPath item)
    , size = (itemSize item)
    }

data TreeResponse = TreeResponse
  { tree :: [TreeItem]
  }
  deriving (Show, Generic)
instance FromJSON TreeResponse

fetchRepository :: HandlerM (Either T.Text [File])
fetchRepository = do
  manager <- liftIO $ newManager tlsManagerSettings
  token <- asks githubToken
  owner <- asks repoOwner
  repo <- asks repoName

  let requestUrl =
        T.unpack $
          T.concat
            [ "https://api.github.com/repos/"
            , owner
            , "/"
            , repo
            , "/git/trees/main?recursive=1"
            ]

  initialRequest <- parseRequest requestUrl

  let request =
        initialRequest
          { requestHeaders =
              [ (hAuthorization, "Bearer " <> TE.encodeUtf8 (token))
              , (hUserAgent, "notes-viewer-backend")
              , ("Accept", "application/vnd.github.v3+json")
              ]
          }
  logDebug $ "Fetching repository tree from: " <> T.pack requestUrl

  result <- liftIO $ try $ httpLbs request manager

  case result of
    Left (e :: HttpException) -> do
      logError $ "GitHub API request failed: " <> T.pack (show e)
      return $ Left "Failed to fetch repository data"
    Right response -> do
      let status = responseStatus response

      if statusIsSuccessful status
        then do
          let decodedBody = decode (responseBody response)
          case decodedBody of
            Just treeData -> do
              return $ Right $ processItems (tree treeData)
            Nothing -> do
              return $ Left $ "GiHub API error: unable to decode body"
        else do
          return $ Left $ "GitHub API error: " <> T.pack (show $ statusCode status)
 where
  mdSuffix = T.pack ".md"
  zkPrefix = T.pack ".zk"

  isRelevantItem :: TreeItem -> Bool
  isRelevantItem TreeItem{itemType, itemPath} =
    itemType /= "tree"
      && mdSuffix `T.isSuffixOf` itemPath
      && not (zkPrefix `T.isPrefixOf` itemPath)

  processItems :: [TreeItem] -> [File]
  processItems = map makeFile . filter isRelevantItem
