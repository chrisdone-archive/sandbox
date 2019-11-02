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
newtype Lex a = Lex { runLex :: forall r.  S -> (S -> a -> r) -> r }
instance Functor Lex where fmap = liftM
instance Applicative Lex where (<*>) = ap; pure = return
instance Monad Lex where
  return a = Lex (\s k -> k s a)
  m >>= f = Lex (\s k -> runLex m s (\s' a -> runLex (f a) s' k))

{-

Exercise:

1. Consume input, keeping the line, column and positions up to date.
2. Do this in a way that is fast.
3. Is the continuation style faster than merely returning a new S? Likely not?

-}
