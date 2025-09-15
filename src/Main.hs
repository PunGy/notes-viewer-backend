{-# LANGUAGE OverloadedStrings #-}

module Main where

import Web.Scotty
import Core.Config (loadConfig)
import Api.Router (setupRouter)
import Core.Types (port)

main :: IO ()
main = do
  config <- loadConfig
  scotty (port config) $ setupRouter config
