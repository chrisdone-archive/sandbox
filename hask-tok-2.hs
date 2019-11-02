{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import           Data.Word

data S =
  S
    { byteString :: {-# UNPACK #-}!ByteString
    , position :: !Int
    , line :: !Int
    , column :: !Int
    }
  deriving (Show, Eq)
newtype Lex a = Lex { runLex :: forall r.  S -> (a, S) }
instance Functor Lex where fmap = liftM
instance Applicative Lex where (<*>) = ap; pure = return
instance Monad Lex where
  return a = Lex (\s -> (a, s))
  m >>= f =
    Lex
      (\s ->
         let (a, !s') = runLex m s
          in runLex (f a) s')

{-

Exercise:

1. Consume input, keeping the line, column and positions up to date.
2. Do this in a way that is fast.

-}
