{-# LANGUAGE LambdaCase #-}

-- Simple exmaple of a terminal type as an ADT.

import Prelude hiding (print)
import Control.Monad

--------------------------------------------------------------------------------
-- Trivial

data Terminal a
  = Print String (Terminal a)
  | GetLine (String -> Terminal a)
  | Return a

interpret :: Terminal a -> IO a
interpret =
  \case
    Return a -> return a
    Print str next -> do
      putStrLn str
      interpret next
    GetLine f -> do
      line <- getLine
      interpret (f line)

main :: IO ()
main =
  interpret
    (Print
       "Please enter your name: "
       (GetLine (\name -> Print ("Hello, " ++ name ++ "!") (Return ()))))

--------------------------------------------------------------------------------
-- Monadic

main2 :: IO ()
main2 =
  interpret (do printline "Enter your name"
                line <- getline
                line2 <- getline
                printline ("Hi " ++ line ++ ", " ++ line2)
                return ())

printline :: String -> Terminal ()
printline str = Print str (Return ())

getline :: Terminal String
getline = GetLine (\str -> Return str)

instance Functor Terminal where
  fmap f =
    \case
      Return x -> Return (f x)
      Print str x -> Print str (fmap f x)
      GetLine g -> GetLine (\line -> fmap f (g line))

instance Applicative Terminal where
  (<*>) = ap
  pure = return

instance Monad Terminal where
  return = Return
  m >>= f =
    case m of
      Return a -> f a
      GetLine g -> GetLine (\line -> g line >>= f)
      Print str m' -> Print str (m' >>= f)
