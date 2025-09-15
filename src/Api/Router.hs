{-# LANGUAGE OverloadedStrings #-}

module Api.Router (setupRouter) where

import Api.Webhook (webhookHandler)
import Core.App (liftActionM, runAppContext)
import Core.Types (AppConfig)
import Data.Aeson (object, (.=))
import qualified Data.Text as T
import Utils.Logger (logInfo)
import Web.Scotty

setupRouter :: AppConfig -> ScottyM ()
setupRouter config = do
  let runner = runAppContext config

  get "/health" $ do
    runner $ logInfo "Got health message" >> (liftActionM $ json $ object ["status" .= ("healthy" :: T.Text)])

  post "/webhook/github" $ runner webhookHandler
