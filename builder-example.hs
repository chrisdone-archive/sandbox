#!/usr/bin/env stack
-- stack --resolver lts-12.12 script
{-# LANGUAGE OverloadedStrings #-}
import           System.IO
import           Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
import qualified Data.ByteString.Lazy as L
import qualified Data.Conduit.ByteString.Builder as CB
import qualified Data.ByteString.Lazy.Builder as L
main =
  runConduitRes
    (CL.sourceList [L.word8 2 <> L.byteString "ab"] .| CB.builderToByteString .|
     CB.sinkHandle stdout)
