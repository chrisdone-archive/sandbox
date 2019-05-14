{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

-- | Lists that are of finite length.

module Data.List.Finite
  ( FiniteList(Empty, (:%))
  , maxed
  , cons
  , empty
  ) where

-- | A list of finite length.
data FiniteList a =
  FiniteList
    { finiteListMaxLength :: !Int
    , finiteListLength :: !Int
    , finiteList :: ![a]
    }
  deriving (Functor, Foldable, Traversable)

-- | Make a finite list.
empty :: Int -> FiniteList a
empty size =
  FiniteList {finiteListMaxLength = size, finiteListLength = 0, finiteList = []}

-- | Is the list maxed out?
maxed :: FiniteList a -> Bool
maxed (FiniteList {finiteListMaxLength, finiteListLength}) =
  finiteListLength == finiteListMaxLength

-- | Cons onto the list. Ignores if we reached the max already.
cons :: a -> FiniteList a -> FiniteList a
cons a list =
  if maxed list
    then list
    else list
           { finiteListLength = finiteListLength list + 1
           , finiteList = a : finiteList list
           }

-- | Uncons from the list.
uncons :: FiniteList a -> Maybe (a, FiniteList a)
uncons list =
  case finiteList list of
    (x:xs) ->
      let !len = finiteListLength list - 1
       in Just (x, list {finiteList = xs, finiteListLength = len})
    _ -> Nothing

-- | A bidirectional pattern synonym matching an empty sequence.
pattern Empty :: Int -> FiniteList a
pattern Empty a =
  FiniteList {finiteListMaxLength = a, finiteListLength = 0, finiteList = []}

-- | A bidirectional pattern synonym viewing the front of a finite list.
pattern (:%) :: a -> FiniteList a -> FiniteList a
pattern x :% xs <- (uncons -> Just (x, xs))
