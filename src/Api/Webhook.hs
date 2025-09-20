{-# LANGUAGE OverloadedStrings #-}

module Api.Webhook (webhookHandler) where

import Control.Monad.Reader (asks)
import Core.App (HandlerM)
import Core.Types (AppConfig (..), webhookSecret)
import Data.Aeson (object, (.=))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Network.HTTP.Types.Status
import Security.GitHub (verifySignature)
import Utils.Logger (logWarning)
import Web.Scotty.Trans

webhookHandler :: HandlerM ()
webhookHandler = do
  signature <- header "X-Hub-Signature-256"
  event <- header "X-GitHub-Event"
  _body <- body

  case (signature, event) of
    (Just sig, Just "push") -> do
      secret <- asks webhookSecret
      let isValid = verifySignature secret _body (TL.toStrict sig)

      if isValid
        then processWebhook
        else do
          status status401
          json $ object ["error" .= ("Invalid signature" :: T.Text)]
    (_, Just evt) -> do
      logWarning $ "Ignoring Github event: " <> TL.toStrict evt
      status ok200
      json $ object ["error " .= ("ignored" :: T.Text)]
    _ -> do
      status status404
      json $ object ["error" .= ("Missing signatrue or event header" :: T.Text)]

processWebhook :: HandlerM ()
processWebhook = do
  json $ object ["status" .= ("success" :: T.Text)]
