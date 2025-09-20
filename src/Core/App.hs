{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Core.App (HandlerM, RouterM, AppM) where

import Control.Monad.Reader (ReaderT)
import Core.Types (AppConfig)
import Web.Scotty.Trans

type AppM = ReaderT AppConfig IO
type HandlerM = ActionT AppM
type RouterM = ScottyT AppM

