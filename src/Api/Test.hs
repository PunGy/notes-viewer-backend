{-# LANGUAGE OverloadedStrings #-}

module Api.Test (testHandler) where

import Client.GitHub (fetchRepository)
import Core.App (HandlerM)
import Network.HTTP.Types.Status
import Data.Aeson
import Web.Scotty.Trans

testHandler :: HandlerM ()
testHandler = do
  response <- fetchRepository
  case response of
    Left err  -> do
      status status500
      json $ object ["error" .= err]
    Right repo -> do
      status status200
      json $ repo

