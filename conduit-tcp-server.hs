#!/usr/bin/env stack
-- stack --resolver lts-12.12 script
{-# LANGUAGE OverloadedStrings #-}
import Data.Conduit.Network
import Conduit

main =
  runTCPServer
    (serverSettings 2019 "*")
    (\app -> do
       putStrLn "Someone connected!"
       let loop = do
             oneMessageMaybe <-
               runConduit (appSource app .| mapMC print .| await)
             case oneMessageMaybe of
               Nothing -> putStrLn "No more messages in upstream, done!"
               Just message -> do
                 print message
                 loop
       loop)
  where
    myparser = do
      len <- P.anyWord8
      bytes <- P.take (fromIntegral len)
      return bytes
