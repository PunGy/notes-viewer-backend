module Utils.Env (loadEnv) where

import Data.Maybe (mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Environment (setEnv)

comment :: T.Text
comment = T.pack "#"

eqq :: T.Text
eqq = T.pack "="

parseEnvFile :: T.Text -> [(String, String)]
parseEnvFile = mapMaybe parseLine . T.lines
  where
    parseLine :: T.Text -> Maybe (String, String)
    parseLine line
      | T.null line = Nothing
      | comment `T.isPrefixOf` line = Nothing
      | otherwise = case T.breakOn eqq line of
          (key, value)
            | not (T.null value) ->
                Just (T.unpack key, T.unpack $ T.drop 1 value)
          _ -> Nothing

loadEnv :: FilePath -> IO ()
loadEnv path = do
  content <- T.readFile path
  let vars = parseEnvFile content
  mapM_ (uncurry setEnv) vars
