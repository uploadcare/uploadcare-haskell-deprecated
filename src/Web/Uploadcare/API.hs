{-# LANGUAGE OverloadedStrings #-}

module Web.Uploadcare.API
(
  File(..)
, file
) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.Aeson
import qualified Data.Aeson.Types as T
import Data.Attoparsec.Lazy (parse, Result(..))
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy as LBS
import Network.HTTP.Conduit (Response(..))
import Web.Uploadcare.Internal

data File = File {
    file_id :: String
  , last_keep_claim :: String
  , made_public :: Bool
  , mime_type :: String
  , on_s3 :: Bool
  , original_file_url :: String
  , original_filename :: String
  , removed :: Maybe String
  , size :: Integer
  , upload_date :: String
  , url :: String
  } deriving (Show)

instance FromJSON File where
    parseJSON (Object v) =
        File <$> v .: "file_id"
             <*> v .: "last_keep_claim"
             <*> v .: "made_public"
             <*> v .: "mime_type"
             <*> v .: "on_s3"
             <*> v .: "original_file_url"
             <*> v .: "original_filename"
             <*> v .: "removed"
             <*> v .: "size"
             <*> v .: "upload_date"
             <*> v .: "url"
    parseJSON _ = mzero

file :: ByteString -> ByteString -> ByteString -> IO (Maybe File)
file publicKey secretKey fileId = do
    res <- request publicKey secretKey "GET" path
    return $ parseResponse res
  where
    path = BS.concat ["/files/", fileId, "/"]

parseResponse :: Response LBS.ByteString -> Maybe File
parseResponse res = case parse json body of
    (Done _ r) -> T.parseMaybe parseJSON r :: Maybe File
    _          -> Nothing
  where
    body = responseBody res
