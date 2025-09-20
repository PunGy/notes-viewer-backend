{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module Utils.Logger (logDebug, logInfo, logWarning, logError) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (MonadReader, asks)
import Core.Types (AppConfig (logLevel), LogLevel (..))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

makeLog :: (MonadReader AppConfig m, MonadIO m) => LogLevel -> T.Text -> m ()
makeLog loggerLevel msg = do
  currentLevel <- asks logLevel
  when (currentLevel <= loggerLevel) $
    liftIO $
      TIO.putStrLn $
        T.concat ["[" <> T.pack (show loggerLevel) <> "]: " <> msg]

logDebug :: (MonadReader AppConfig m, MonadIO m) => T.Text -> m ()
logDebug = makeLog Debug

logInfo :: (MonadReader AppConfig m, MonadIO m) => T.Text -> m ()
logInfo = makeLog Info

logWarning :: (MonadReader AppConfig m, MonadIO m) => T.Text -> m ()
logWarning = makeLog Warning

logError :: (MonadReader AppConfig m, MonadIO m) => T.Text -> m ()
logError = makeLog Error
