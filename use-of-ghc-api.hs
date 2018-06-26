{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE CPP #-}
import GHC
import GHC.Paths
import DynFlags
import Language.Haskell.TH.LanguageExtensions

targetFile :: [Char]
targetFile = "B.hs"

main :: IO ()
main = do
  defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      let dflags' = foldl xopt_set dflags [ImplicitPrelude]
      _ <- setSessionDynFlags dflags'
      target <- guessTarget targetFile Nothing
      setTargets [target]
      _ <- load LoadAllTargets
      modSum <- getModSummary $ mkModuleName "B"
      p <- parseModule modSum
      t <- typecheckModule p
      pure ()
