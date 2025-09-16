{-# LANGUAGE OverloadedStrings #-}
module Utils.Logger (logDebug, logInfo, logWarning, logError) where

import Control.Monad (when)
import Control.Monad.Reader (asks, liftIO)
import Core.App (HandlerM)
import Core.Types (AppConfig (logLevel), LogLevel (..))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

makeLog :: LogLevel -> T.Text -> HandlerM ()
makeLog loggerLevel msg = do
  currentLevel <- asks logLevel
  when (currentLevel <= loggerLevel) $
    liftIO $
      TIO.putStrLn $
        T.concat ["[" <> T.pack (show loggerLevel) <> "]: " <> msg]

logDebug :: T.Text -> HandlerM ()
logDebug = makeLog Debug

logInfo :: T.Text -> HandlerM ()
logInfo = makeLog Info

logWarning :: T.Text -> HandlerM ()
logWarning = makeLog Warning

logError :: T.Text -> HandlerM ()
logError = makeLog Error
