#!/usr/bin/env stack
-- stack --resolver lts-12.12 script
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString as S
import qualified Data.Attoparsec.ByteString as P
import Conduit
import Data.Conduit.List
import Data.Conduit.Attoparsec

main = do
  result <- runConduit (sourceList chunks .| sinkParserEither myparser)
  case result of
    Left err -> print err
    Right val -> print val
  where
    chunks = [S.pack [2], S.pack [97], S.pack [98]]
    myparser = do
      len <- P.anyWord8
      bytes <- P.take (fromIntegral len)
      return bytes
