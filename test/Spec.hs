{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Web.Uploadcare.Client
import Web.Uploadcare.Internal

main :: IO ()
main = hspec $ do
    describe "Client" $ do
        it "can create a client with demo keys" $ do
            client <- newDemoClient
            publicKey client `shouldBe` "demopublickey"
            secretKey client `shouldBe` "demoprivatekey"
            closeClient client

    describe "Internal" $ do
        it "can create a secure signature" $ do
            client <- newClient "public" "secret"
            let timestamp = "Fri, 09 Nov 2001 01:08:47 GMT"
                signature = makeSignature client "GET" "/files/" "" timestamp
            signature `shouldBe` "283c0160f1dc06cfe800dad1db1c98e4d1453a75"
