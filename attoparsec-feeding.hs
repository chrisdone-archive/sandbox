#!/usr/bin/env stack
-- stack --resolver lts-12.12 script
{-# LANGUAGE OverloadedStrings #-}
import           Control.Applicative
import qualified Data.ByteString as S
import qualified Data.Attoparsec.ByteString as P

main = loop (P.parse myparser) (S.pack [1,3])
  where
    loop eater input =
      do putStrLn ("Chunk " ++ show (S.unpack chunk))
         case eater chunk of
           P.Done _ value -> print value
           P.Fail _ _ err -> putStrLn err
           P.Partial next -> do
             putStrLn "Waiting for more ..."
             loop next remaining
      where (chunk, remaining) = S.splitAt 1 input
    myparser = do
      len <- (P.word8 1 *> P.word8 2) <|> (P.word8 1 *> P.word8 3)
      return len
