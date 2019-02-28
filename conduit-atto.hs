#!/usr/bin/env stack
-- stack --resolver lts-12.12 script
{-# LANGUAGE OverloadedStrings #-}
import           Data.Conduit.Network
import qualified Data.ByteString as S
import qualified Data.Attoparsec.ByteString as P
import           Conduit
import           Data.Conduit.Attoparsec

main =
  runTCPServer
    (serverSettings 2019 "*")
    (\app -> do
       putStrLn "Someone connected!"
       runConduit (appSource app .| conduitParserEither parser .| handlerSink app))
  where


handlerSink output = do
  mnext <- await
  case mnext of
    Nothing -> liftIO (putStrLn "Connection closed.")
    Just eithermessage ->
      case eithermessage of
        Left err -> liftIO (print err)
        Right (position, message) -> do
          liftIO (print message)
          liftIO (runConduit (yield "Thanks!\n" .| appSink app))
          handlerSink app

parser = do
    len <- P.anyWord8
    bytes <- P.take (fromIntegral len)
    return bytes
