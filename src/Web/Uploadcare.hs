{-# LANGUAGE OverloadedStrings #-}

module Web.Uploadcare
(
  makeSignature
, authHeader
, request
) where

import qualified Crypto.Hash.MD5 as MD5
import qualified Crypto.Hash.SHA1 as SHA1
import Crypto.MAC.HMAC (hmac)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (toLower)
import Data.Hex (hex)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)
import Network.HTTP.Conduit
import Network.HTTP.Types.Header (Header)
import Network.HTTP.Types.Method (Method)
import System.Locale (defaultTimeLocale, rfc822DateFormat)

lowerHex :: ByteString -> ByteString
lowerHex = BS.map toLower . hex

makeSignature :: ByteString -> Method -> ByteString -> ByteString
              -> IO ByteString
makeSignature secretKey rMethod rPath rBody = do
    time <- getCurrentTime
    return . lowerHex . sign $ BS.intercalate "\n" [
        rMethod
      , lowerHex . MD5.hash $ rBody
      , "application/json"
      , BS.pack $ formatTime defaultTimeLocale rfc822DateFormat time
      , rPath
      ]
  where
    sign = hmac SHA1.hash 512 secretKey

authHeader :: ByteString -> ByteString -> Header
authHeader publicKey signature = ("Authentication", auth)
  where
    auth = BS.concat ["UploadCare ", publicKey, ":", signature]

request :: ByteString -> ByteString -> Method -> ByteString
        -> IO (Response LBS.ByteString)
request publicKey secretKey rMethod rPath = do
    signature <- makeSignature secretKey rMethod rPath ""
    let req = def {
        method = rMethod
      , host = "api.uploadcare.com"
      , path = rPath
      , requestHeaders = [authHeader publicKey signature]
    }
    res <- withManager $ httpLbs req
    return res
