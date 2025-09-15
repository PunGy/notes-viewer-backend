{-# LANGUAGE OverloadedStrings #-}
module Utils.Logger (logDebug, logInfo, logWarning, logError) where

import Control.Monad (when)
import Control.Monad.Reader (asks, liftIO)
import Core.App (AppM)
import Core.Types (AppConfig (logLevel), LogLevel (..))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

makeLog :: LogLevel -> T.Text -> AppM ()
makeLog loggerLevel msg = do
  currentLevel <- asks logLevel
  when (currentLevel <= loggerLevel) $
    liftIO $
      TIO.putStrLn $
        T.concat ["[" <> T.pack (show loggerLevel) <> "]: " <> msg]

logDebug :: T.Text -> AppM ()
logDebug = makeLog Debug

logInfo :: T.Text -> AppM ()
logInfo = makeLog Info

logWarning :: T.Text -> AppM ()
logWarning = makeLog Warning

logError :: T.Text -> AppM ()
logError = makeLog Error
