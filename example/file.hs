{-# LANGUAGE OverloadedStrings #-}

import Web.Uploadcare

main :: IO ()
main = do
    client <- newDemoClient
    file <- getFile client "1d4d470b-8048-4c00-8ae6-9be332e7d2b1"
    putStrLn $ maybe "" originalFileUrl file
