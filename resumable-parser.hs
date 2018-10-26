{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

{-

A very simple, "core" resumable parser with backtracking-by-default.

> parseOnly (letters <> digits) "abc" :: Either [Text] String
Right "abc"
> parseOnly (letters <> digits) "123" :: Either [Text] String
Right "123"
> parseOnly (letters <> digits) "!!!" :: Either [Text] String
Left ["non-letter","non-digit"]
> parseOnly (letters <> digits) "abc!" :: Either [Text] String
Right "abc"
> parseOnly ((letters <> digits) <* endOfInput) "abc!" :: Either [Text] String
Left ["Expected end of input"]
> parseOnly ((letters <> digits) <* endOfInput) "abc" :: Either [Text] String
Right "abc"
-}

module Resumable where

import           Control.Monad
import           Data.Char
import           Data.Maybe
import           Data.Semigroup
import           Data.Text (Text)

--------------------------------------------------------------------------------
-- Parser type

data Result i e r
  = Done (Maybe i) r
  | Failed (Maybe i) e
  | Partial (Maybe i -> Result i e r)

newtype Parser i e a = Parser
  { runParser :: forall r.
            Maybe i
         -> (Maybe i -> a -> Result i e r)
         -> (Maybe i -> e -> Result i e r)
         -> Result i e r
  }

instance Monad (Parser i e) where
  return x = Parser (\mi done _failed -> done mi x)
  {-# INLINABLE return #-}
  m >>= f =
    Parser
      (\mi done failed ->
         runParser m mi (\mi' v -> runParser (f v) mi' done failed) failed)
  {-# INLINABLE (>>=) #-}

instance Semigroup e => Semigroup (Parser [i] e a) where
  left <> right =
    Parser
      (\mi done failed ->
         runParser
           left
           mi
           done
           (\_mi e -> runParser right mi done (\mi' e' -> failed mi' (e <> e'))))
  {-# INLINABLE (<>) #-}

instance Applicative (Parser i e) where
  (<*>) = ap
  {-# INLINABLE (<*>) #-}
  pure = return
  {-# INLINABLE pure #-}

instance Functor (Parser i e) where
  fmap = liftM
  {-# INLINABLE fmap #-}

--------------------------------------------------------------------------------
-- API

class NoMoreInput e where noMoreInput :: e
instance NoMoreInput e => NoMoreInput [e] where noMoreInput = pure noMoreInput
class ExpectedEof e where expectedEof :: e
instance ExpectedEof e => ExpectedEof [e] where expectedEof = pure expectedEof
class SomeError e where someError :: Text -> e
instance SomeError e => SomeError [e] where someError = pure . someError

instance NoMoreInput Text where noMoreInput = "No more input"
instance ExpectedEof Text where expectedEof = "Expected end of input"
instance SomeError Text where someError = id

parseOnly :: NoMoreInput e => Parser i e a -> i -> Either e a
parseOnly p i =
  case terminate (runParser p (Just i) Done Failed) of
    Partial _ -> Left noMoreInput
    Done _ d -> pure d
    Failed _ e -> Left e
  where
    terminate r =
      case r of
        Partial f -> terminate (f Nothing)
        x -> x

nextElement :: NoMoreInput e => Parser [a] e a
nextElement =
  Parser (\mi0 done failed ->
       let go mi =
             case mi of
               Nothing -> failed Nothing noMoreInput
               Just (x:xs) -> done (Just xs) x
               Just [] -> Partial go
        in go mi0)
{-# INLINABLE nextElement #-}

endOfInput :: ExpectedEof e => Parser [a] e ()
endOfInput =
  Parser (\mi0 done failed ->
       let go mi =
             case mi of
               Just [] -> Partial go
               Just (_:_) -> failed mi expectedEof
               Nothing -> done Nothing ()
        in go mi0)
{-# INLINABLE endOfInput #-}

--------------------------------------------------------------------------------
-- Combinators

digit :: (SomeError e, NoMoreInput e) =>  Parser [Char] e Char
digit = do
  c <- nextElement
  if isDigit c
    then pure c
    else Parser (\mi _done failed -> failed mi (someError "non-digit"))

letter :: (SomeError e, NoMoreInput e) =>  Parser [Char] e Char
letter = do
  c <- nextElement
  if isLetter c
    then pure c
    else Parser (\mi _done failed -> failed mi (someError "non-letter"))

letters :: (SomeError e, NoMoreInput e) => Parser [Char] [e] [Char]
letters = do
  c <- letter
  d <- fmap Just letters <> pure Nothing
  pure (c : fromMaybe [] d)

digits :: (NoMoreInput e, SomeError e) => Parser [Char] [e] [Char]
digits = do
  c <- digit
  d <- fmap Just digits <> pure Nothing
  pure (c : fromMaybe [] d)
