{-# LANGUAGE OverloadedStrings #-}

module Web.Uploadcare.API
(
  File(..)
, getFile
, deleteFile
, saveFile
) where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Web.Uploadcare.Client (Client)
import Web.Uploadcare.Internal

data File = File {
    fileId :: String
  , lastKeepClaim :: String
  , madePublic :: Bool
  , mimeType :: String
  , onS3 :: Bool
  , originalFileUrl :: String
  , originalFilename :: String
  , removed :: Maybe String
  , size :: Integer
  , uploadDate :: String
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

getFile :: Client -> ByteString -> IO (Either String File)
getFile client fid = do
    queryRequest client "GET" path
  where
    path = BS.concat ["/files/", fid, "/"]

deleteFile :: Client -> File -> IO (Either String ())
deleteFile client file = do
    commandRequest client "DELETE" path
  where
    path = BS.concat ["/files/", fid, "/"]
    fid = BS.pack $ fileId file

saveFile :: Client -> File -> IO (Either String ())
saveFile client file = do
    commandRequest client "POST" path
  where
    path = BS.concat ["/files/", fid, "/storage/"]
    fid = BS.pack $ fileId file
