{-# LANGUAGE OverloadedStrings #-}

module Security.GitHub (verifySignature) where

import Crypto.Hash.Algorithms (SHA256)
import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import qualified Data.Text as T
import qualified Data.ByteString.Base16 as Base16
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBS
import Crypto.Hash (digestFromByteString)

verifySignature :: T.Text -> LBS.ByteString -> T.Text -> Bool
verifySignature secret body signatureHeader =
  case T.stripPrefix "sha256=" signatureHeader of
    Nothing ->
      False
    Just signature ->
      case Base16.decode $ TE.encodeUtf8 signature of
        Left _ ->
          False
        Right decodedSig ->
          case digestFromByteString decodedSig of
            Nothing ->
              False
            Just sig ->
              let
                secretBS = TE.encodeUtf8 secret
                bodyBS   = LBS.toStrict body -- Note: loads the whole body into memory.

                -- Calculate HMAC
                calculatedHmac :: HMAC SHA256
                calculatedHmac = hmac secretBS bodyBS

              -- possible time attack but meh
              in sig == (hmacGetDigest calculatedHmac)
