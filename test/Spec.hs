{-# LANGUAGE OverloadedStrings #-}

import Test.Hspec
import Web.Uploadcare.Client

main :: IO ()
main = hspec $ do
    describe "Client" $ do
        it "can create a client with demo keys" $ do
            client <- newDemoClient
            publicKey client `shouldBe` "demopublickey"
            secretKey client `shouldBe` "demoprivatekey"
