{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import           Data.ByteString (ByteString)
import qualified Data.Conduit.List as CL
import           Data.CSV.Conduit
import qualified Data.Conduit.Binary as CB
import           Data.Conduit.Network
import qualified Data.Attoparsec.ByteString as P
import           Conduit
import           Data.Conduit.Attoparsec

main =
  runTCPServer
    (serverSettings 2019 "*")
    (\app -> do
       putStrLn "Someone connected!"
       runConduit
         (appSource app .| conduitParserEither parser .| handlerSink app))
  where
    handlerSink app = do
      mnext <- await
      case mnext of
        Nothing -> liftIO (putStrLn "Connection closed.")
        Just eithermessage ->
          case eithermessage of
            Left err -> liftIO (print err)
            Right (position, lineCount) -> do
              liftIO (print lineCount)
              liftIO
                (runConduitRes
                   (CB.sourceFile "fake-db.csv" .| intoCSV defCSVSettings .|
                    CL.mapMaybe
                      (M.lookup "Name" :: Map ByteString ByteString -> Maybe ByteString) .|
                    CL.map (<> "\n") .|
                    CL.isolate lineCount .|
                    appSink app))
              handlerSink app
    parser = do
      len <- P.anyWord8
      return (fromIntegral len * 1000)
