{-# LANGUAGE OverloadedStrings #-}

module Web.Uploadcare.Client
(
  Client(..)
, newClient
, newDemoClient
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

newDemoClient :: IO Client
newDemoClient = do
    putStrLn "Warning! You are using the demo account."
    newClient "demopublickey" "demoprivatekey"

closeClient :: Client -> IO ()
closeClient = closeManager . manager
