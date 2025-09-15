{-# LANGUAGE OverloadedStrings #-}

module Api.Webhook (webhookHandler) where

import Control.Monad.Reader (asks)
import Core.App (AppM, liftActionM)
import Core.Types (AppConfig (..), webhookSecret)
import Data.Aeson (object, (.=))
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8Lenient)
import qualified Data.Text.Lazy as TL
import Network.HTTP.Types.Status
import Security.GitHub (verifySignature)
import Utils.Logger (logDebug, logWarning)
import Web.Scotty

webhookHandler :: AppM ()
webhookHandler = do
  signature <- liftActionM $ header "X-Hub-Signature-256"
  event <- liftActionM $ header "X-GitHub-Event"
  _body <- liftActionM body

  case (signature, event) of
    (Just sig, Just "push") -> do
      secret <- asks webhookSecret
      let isValid = verifySignature secret _body (TL.toStrict sig)

      if isValid
        then processWebhook _body
        else do
          liftActionM $ status status401
          liftActionM $ json $ object ["error" .= ("Invalid signature" :: T.Text)]
    (_, Just evt) -> do
      logWarning $ "Ignoring Github event: " <> TL.toStrict evt
      liftActionM $ status ok200
      liftActionM $ json $ object ["error " .= ("ignored" :: T.Text)]
    _ -> do
      liftActionM $ status status404
      liftActionM $ json $ object ["error" .= ("Missing signatrue or event header" :: T.Text)]

processWebhook :: LBS.ByteString -> AppM ()
processWebhook body = do
  let a = (decodeUtf8Lenient $ LBS.toStrict body)
  _ <- logDebug $ "body: " <> a
  liftActionM $ status ok200
  liftActionM $ json $ object ["status" .= ("success" :: T.Text)]
