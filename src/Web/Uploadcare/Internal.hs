{-# LANGUAGE OverloadedStrings #-}

module Web.Uploadcare.Internal
(
  makeSignature
, apiHeaders
, request
, parseResponse
) where

import qualified Crypto.Hash.MD5 as MD5
import qualified Crypto.Hash.SHA1 as SHA1
import Crypto.MAC.HMAC (hmac)
import Data.Aeson
import qualified Data.Aeson.Types as T
import Data.Attoparsec.Lazy (parse, Result(..))
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Char (toLower)
import Data.Hex (hex)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime)
import Network.HTTP.Conduit
import Network.HTTP.Types.Header (RequestHeaders, hAccept, hContentType, hDate)
import Network.HTTP.Types.Method (Method)
import System.Locale (defaultTimeLocale)
import Web.Uploadcare.Client

jsonContentType :: ByteString
jsonContentType = "application/json"

makeSignature :: Client -> Method -> ByteString -> ByteString -> ByteString
              -> ByteString
makeSignature client rMethod rPath rBody timestamp =
    lowerHex . sign $ BS.intercalate "\n" [
        rMethod
      , lowerHex . MD5.hash $ rBody
      , jsonContentType
      , timestamp
      , rPath
      ]
  where
    lowerHex = BS.map toLower . hex
    sign = hmac SHA1.hash 64 $ secretKey client

apiHeaders :: Client -> ByteString -> ByteString -> RequestHeaders
apiHeaders client signature timestamp = [
        ("Authentication", auth)
      , (hAccept, "application/vnd.uploadcare-v0.2+json")
      , (hDate, timestamp)
      , (hContentType, jsonContentType)
      ]
  where
    auth = BS.concat ["UploadCare ", publicKey client, ":", signature]

request :: Client -> Method -> ByteString -> IO (Response LBS.ByteString)
request client rMethod rPath = do
    time <- getCurrentTime
    let timestamp = toTimestamp time
    let signature = makeSignature client rMethod rPath "" timestamp
    let req = def {
        method = rMethod
      , host = "api.uploadcare.com"
      , path = rPath
      , requestHeaders = apiHeaders client signature timestamp
    }
    res <- withManager $ httpLbs req
    return res
  where
    toTimestamp = BS.pack . formatTime defaultTimeLocale httpDateFormat
    httpDateFormat = "%a, %d %b %Y %H:%M:%S GMT"

parseResponse :: FromJSON a => Response LBS.ByteString -> Maybe a
parseResponse res = case parse json body of
    (Done _ r) -> T.parseMaybe parseJSON r
    _          -> Nothing
  where
    body = responseBody res
