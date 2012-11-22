{-# LANGUAGE OverloadedStrings #-}

import Web.Uploadcare

main :: IO ()
main = do
    client <- newDemoClient
    efile <- getFile client "1d4d470b-8048-4c00-8ae6-9be332e7d2b1"
    putStrLn $ case efile of
        Right file -> originalFileUrl file
        Left err   -> err
