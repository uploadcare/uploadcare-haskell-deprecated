{-# LANGUAGE OverloadedStrings #-}

module Web.Uploadcare.Internal
(
  makeSignature
, apiHeaders
, request
, queryRequest
, commandRequest
, parseResponse
) where

import Control.Exception (try, throw)
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
import Network.HTTP.Types (Status(..))
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

request :: Client -> Method -> ByteString
        -> IO (Either String (Response LBS.ByteString))
request client rMethod rPath = do
    time <- getCurrentTime
    let timestamp = toTimestamp time
        signature = makeSignature client rMethod rPath "" timestamp
        req = def {
            method = rMethod
          , host = "api.uploadcare.com"
          , path = rPath
          , requestHeaders = apiHeaders client signature timestamp
        }
    eres <- try $ withManager $ httpLbs req
    return $ case eres of
        Right res                           -> Right res
        Left (StatusCodeException status _) -> Left $ statusString status
        Left err                            -> throw err
  where
    toTimestamp = BS.pack . formatTime defaultTimeLocale httpDateFormat
    httpDateFormat = "%a, %d %b %Y %H:%M:%S GMT"
    statusString s = unwords [
          show $ statusCode s
        , BS.unpack $ statusMessage s
        ]

queryRequest :: FromJSON a => Client -> Method -> ByteString
             -> IO (Either String a)
queryRequest client rMethod rPath = do
    eres <- request client rMethod rPath
    return $ case eres of
        Right res -> parseResponse res
        Left err  -> Left err

commandRequest :: Client -> Method -> ByteString -> IO (Either String ())
commandRequest client rMethod rPath = do
    eres <- request client rMethod rPath
    return $ case eres of
        Right _  -> Right ()
        Left err -> Left err

parseResponse :: FromJSON a => Response LBS.ByteString -> Either String a
parseResponse res = case parse json body of
    Done _ r     -> T.parseEither parseJSON r
    Fail _ _ err -> Left err
  where
    body = responseBody res
