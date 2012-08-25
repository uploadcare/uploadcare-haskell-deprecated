{-# LANGUAGE OverloadedStrings #-}

module Web.Uploadcare
(
  signature
) where

import qualified Crypto.Hash.MD5 as MD5
import qualified Crypto.Hash.SHA1 as SHA1
import Crypto.MAC.HMAC (hmac)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Char (toLower)
import Data.Hex (hex)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)
import System.Locale (defaultTimeLocale, rfc822DateFormat)

lowerHex :: ByteString -> ByteString
lowerHex = BS.map toLower . hex

signature :: ByteString -> ByteString -> ByteString -> ByteString
          -> IO ByteString
signature secret verb content uri = do
    time <- getCurrentTime
    return . lowerHex . sign $ BS.intercalate "\n" [
        verb
      , lowerHex . MD5.hash $ content
      , "application/json"
      , BS.pack $ formatTime defaultTimeLocale rfc822DateFormat time
      , uri
      ]
  where
    sign = hmac SHA1.hash 512 secret
