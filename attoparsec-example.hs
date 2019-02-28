#!/usr/bin/env stack
-- stack --resolver lts-12.12 script
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString as S
import qualified Data.Attoparsec.ByteString as P

main =
  case P.parseOnly myparser (S.pack [2, 97, 98]) of
    Right result -> print result
    Left err -> putStrLn err
  where
    myparser = do
      len <- P.anyWord8
      bytes <- P.take (fromIntegral len)
      return bytes
