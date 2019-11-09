{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import           Control.Applicative
import           Control.DeepSeq
import           Control.Exception
import           Control.Monad.State.Strict
import           Control.Monad.Trans
import           Data.ByteString (ByteString)
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import           Data.Functor
import           Data.Functor.Identity
import           Data.Maybe
import           Data.Word
import           GHC.Generics
import           Prelude hiding (dropWhile)
import           System.Environment
import           Zepto

data Point =
  Point
    { line :: !Int
    , column :: !Int
    , indentation :: !Int
    }
  deriving (Show, Eq, Generic)
instance NFData Point
newtype P m a = P { runP :: ZeptoT (StateT Point m) a }
  deriving (Functor, Applicative, Monad, Alternative, MonadIO)

instance MonadTrans P where
   lift m = P (lift (lift m))
   {-# INLINE lift #-}

data Token =
  Token
    { byteString :: {-# UNPACK  #-}!ByteString
    , start :: {-# UNPACK #-}!Point
    , end :: {-# UNPACK #-}!Point
    } deriving (Show, Generic)
instance NFData Token
takeToken :: Monad m => (Word8 -> Bool) -> P m Token
takeToken p = do
  start@(Point {line}) <- P (lift get)
  byteString <- P (Zepto.takeWhile p)
  let !newlines = S8.count '\n' byteString
      !lastLine =
        if newlines == 0
          then byteString
          else fromMaybe
                 byteString
                 (fmap
                    (flip S.drop byteString . (+ 1))
                    (S8.elemIndexEnd '\n' byteString))
      !indentation = S.length (S.takeWhile (== 32) lastLine)
      !column = S.length lastLine
      !end = start {line = line + newlines, column, indentation}
  P (lift (put end))
  pure (Token {start, end, byteString})
{-# INLINE takeToken #-}

dropWhile :: Monad m => (Word8 -> Bool) -> P m ()
dropWhile p = void (P (Zepto.takeWhile p))
{-# INLINE dropWhile #-}

run :: Monad m =>  P m a -> ByteString -> m (Either String a)
run (P m) i =
  evalStateT (parseT m i) (Point {line = 1, column = 1, indentation = 0})
{-# INLINE run #-}

simple :: P IO ()
simple = do
  word *> spaces
  end <- P atEnd
  unless end simple

simple_ :: Monad m => P m ()
simple_ = do
  (_, end)<- couple
  unless end simple_

simple_count :: P (State Int) ()
simple_count = do
  (Token { byteString
         , start = Point {line, column, indentation}
         , end = Point { line = line1
                       , column = column2
                       , indentation = indentation2
                       }
         }, end) <- couple
  lift
    (modify
       (+ (line + column + indentation + line1 + column2 +indentation2
          )))
  unless end simple_count
{-# INLINE simple_count #-}

couple :: Monad m => P m (Token, Bool)
couple = do
  token<- word' <* spaces
  end <- P atEnd
  pure (token, end)
{-# INLINE couple #-}

word :: P IO ()
word = do
  token <- (takeToken (not . isSpace8))
  liftIO (print token)

word_ :: Monad m => P m ()
word_ = do
  !w <- fmap force (takeToken (not . isSpace8))
  pure ()

word' :: Monad m => P m Token
word' = do
  !w <- takeToken (not . isSpace8)
  pure w

spaces :: Monad m => P m ()
spaces = dropWhile isSpace8
{-# INLINE spaces #-}

isSpace8 c = c==13 || c==32 || c==10

main = do
  fp:mode:_ <- getArgs
  case mode of
    "print" -> do
      S.readFile fp >>= void . run simple
    "silent" -> do
      void (S.readFile fp >>= evaluate . runIdentity . run simple_)
    "count" -> do
      void (S.readFile fp >>= print . flip execState 0 . run simple_count)

-- With only evaluate

  -- 6,655,456 bytes allocated in the heap
  --    11,184 bytes copied during GC
  --    46,720 bytes maximum residency (1 sample(s))
  --    31,104 bytes maximum slop
  --         9 MB total memory in use (0 MB lost due to fragmentation)

-- With simple_

  --      6,655,592 bytes allocated in the heap
  --         11,184 bytes copied during GC
  --         46,720 bytes maximum residency (1 sample(s))
  --         31,104 bytes maximum slop
  --              9 MB total memory in use (0 MB lost due to fragmentation)

  --                                    Tot time (elapsed)  Avg pause  Max pause
  -- Gen  0         1 colls,     0 par    0.000s   0.000s     0.0000s    0.0000s
  -- Gen  1         1 colls,     0 par    0.000s   0.000s     0.0001s    0.0001s

  -- INIT    time    0.000s  (  0.000s elapsed)
  -- MUT     time    0.027s  (  0.030s elapsed)
  -- GC      time    0.000s  (  0.000s elapsed)
  -- EXIT    time    0.000s  (  0.000s elapsed)
  -- Total   time    0.027s  (  0.031s elapsed)

  -- %GC     time       0.3%  (0.3% elapsed)

  -- Alloc rate    250,210,225 bytes per MUT second

  -- Productivity  99.4% of total user, 99.5% of total elapsed
