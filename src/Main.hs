{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

import Core.Config (loadConfig)
import Api.Router (setupRouter)
import Core.Types (port)
import Web.Scotty.Trans
import Control.Monad.Reader

main :: IO ()
main = do
  config <- loadConfig
  scottyT (port config) (`runReaderT` config) setupRouter
