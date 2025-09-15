{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Core.App (AppM, runAppContext, liftActionM) where

import Control.Monad.Reader (ReaderT, MonadReader, runReaderT)
import Control.Monad.IO.Class (MonadIO)
import Core.Types (AppConfig)
import Web.Scotty (ActionM)
import Control.Monad.Trans.Class (lift)

newtype AppM a = AppM { unAppM :: ReaderT AppConfig ActionM a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadReader AppConfig
    )

liftActionM :: ActionM a -> AppM a
liftActionM action = AppM (lift action)

runAppContext :: AppConfig -> AppM a -> ActionM a
runAppContext config app = runReaderT (unAppM app) config
