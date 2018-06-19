{-# LANGUAGE FlexibleInstances, TypeApplications, UndecidableInstances, ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Displays a type error, using context information given by the reflection Reifies.

import           Data.Tagged
import qualified Data.ByteString.Lazy as L
import           Data.String
import           Data.Proxy
import qualified Data.Map.Strict as M
import           Data.Map.Strict (Map)
import           Data.ByteString.Builder
import           Data.List
import           Data.Monoid
import           Data.Reflection

class Display a where display :: a -> Builder

data Type = IntType | StringType

instance Display Type where
  display IntType = "Int"
  display StringType = "String"

data TypeError = TypeMismatch Type Type [TypeError]

instance Reifies s (Map String Type) => Display (Tagged s TypeError) where
  display (Tagged (TypeMismatch t1 t2 otherErrors)) =
    "Couldn't match " <> display t1 <> " against " <> display t2 <>
    "\nContext:\n" <>
    mconcat
      (intersperse
         "\n"
         (map
            (\(k, v) -> fromString k <> " :: " <> display v)
            (M.toList (reflect (Proxy @s))))) <>
    "\nOther errors:\n" <>
    mconcat (intersperse "\n" (map (display . tagWith (Proxy @s)) otherErrors))

main :: IO ()
main =
  L.putStr
    (toLazyByteString
       (reify
          (M.fromList [("x", IntType)])
          (\s ->
             display
               (tagWith
                  s
                  (TypeMismatch
                     IntType
                     StringType
                     [TypeMismatch StringType IntType []])))))
