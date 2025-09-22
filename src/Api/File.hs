{-# LANGUAGE OverloadedStrings #-}

module Api.File (fileHandler) where

import Client.GitHub (fetchRepository, fetchFile)
import Core.App (HandlerM)
import Network.HTTP.Types.Status
import qualified Data.Text as T
import Data.Aeson
import Web.Scotty.Trans

fileHandler :: HandlerM ()
fileHandler = do
  response <- fetchRepository
  case response of
    Left err  -> do
      status status500
      json $ object ["error" .= err]
    Right repo -> do
      case repo of
        (fileEntry:_) -> do
          req <- fetchFile fileEntry
          case req of
            Left err -> do
              status status500
              json $ object ["error" .= err]
            Right rawFile -> do
              status status200
              raw $ rawFile
        _ -> do
          status status404
          json $ object ["error" .= ("Empty repository" :: T.Text)]

