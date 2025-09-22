{-# LANGUAGE OverloadedStrings #-}

module Api.Router (setupRouter) where

import Api.Webhook (webhookHandler)
import Api.Files (filesHandler)
import Api.File (fileHandler)
import Core.App
import Data.Aeson (object, (.=))
import qualified Data.Text as T
import Utils.Logger (logInfo)
import Web.Scotty.Trans

setupRouter :: RouterM ()
setupRouter = do
  get "/health" $ do
    logInfo "Got health message"
    json $ object ["status" .= ("healthy" :: T.Text)]
  post "/webhook/github" $ webhookHandler
  get "/files" $ filesHandler
  get "/file" $ fileHandler
