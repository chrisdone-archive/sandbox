#!/usr/bin/env stack
-- stack --resolver lts-12.12 script

{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Simple
import qualified Data.ByteString.Char8 as B8

main :: IO ()
main = do
  response <- httpBS "http://example.com"
  B8.putStrLn (getResponseBody response)
