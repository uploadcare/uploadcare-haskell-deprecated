{-# LANGUAGE OverloadedStrings #-}

module Web.Uploadcare
(
  makeSignature
, apiHeaders
, request
) where

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as CBS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (toLower)
import qualified Data.Digest.MD5 as MD5
import Data.HMAC (hmac_sha1)
import Data.Hex (hex)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)
import Network.HTTP.Conduit
import Network.HTTP.Types.Header (RequestHeaders, hContentType, hDate)
import Network.HTTP.Types.Method (Method)
import System.Locale (defaultTimeLocale)

jsonContentType :: ByteString
jsonContentType = "application/json"

makeSignature :: ByteString -> Method -> ByteString -> ByteString
              -> ByteString -> ByteString
makeSignature secretKey rMethod rPath rBody timestamp =
    lowerHex . sign $ BS.intercalate "\n" [
        rMethod
      , lowerHex . MD5.hash . BS.unpack $ rBody
      , jsonContentType
      , timestamp
      , rPath
      ]
  where
    lowerHex = CBS.map toLower . hex . BS.pack
    sign = hmac_sha1 (BS.unpack secretKey) . BS.unpack

apiHeaders :: ByteString -> ByteString -> ByteString -> RequestHeaders
apiHeaders publicKey signature timestamp = [
        ("Authentication", auth)
      , (hDate, timestamp)
      , (hContentType, jsonContentType)
      ]
  where
    auth = BS.concat ["UploadCare ", publicKey, ":", signature]

request :: ByteString -> ByteString -> Method -> ByteString
        -> IO (Response LBS.ByteString)
request publicKey secretKey rMethod rPath = do
    time <- getCurrentTime
    let timestamp = toTimestamp time
    let signature = makeSignature secretKey rMethod rPath "" timestamp
    let req = def {
        method = rMethod
      , host = "api.uploadcare.com"
      , path = rPath
      , requestHeaders = apiHeaders publicKey signature timestamp
    }
    res <- withManager $ httpLbs req
    return res
  where
    toTimestamp = CBS.pack . formatTime defaultTimeLocale httpDateFormat
    httpDateFormat = "%a, %d %b %Y %H:%M:%S GMT"
