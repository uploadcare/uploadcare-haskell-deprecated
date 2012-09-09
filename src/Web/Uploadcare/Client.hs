module Web.Uploadcare.Client
(
  Client(..)
, newClient
, closeClient
) where

import Data.ByteString.Char8 (ByteString)
import Network.HTTP.Conduit (Manager, def, newManager, closeManager)

data Client = Client {
    manager :: Manager
  , publicKey :: ByteString
  , secretKey :: ByteString
  }

newClient :: ByteString -> ByteString -> IO Client
newClient connPublicKey connSecretKey = do
    connManager <- newManager def
    return $ Client {
        manager = connManager
      , publicKey = connPublicKey
      , secretKey = connSecretKey
      }

closeClient :: Client -> IO ()
closeClient = closeManager . manager
