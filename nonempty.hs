{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A trivial non-empty text value.

-- import qualified Base.NonEmptyText as NET

module Base.NonEmptyText
  (NonEmptyText
  ,KnownNonEmptyString
  ,get
  ,append
  ,mk
  ,static) where

import           Data.Proxy
import           Data.String
import           Data.Text (Text)
import qualified Data.Text as S
import           GHC.TypeLits

-- | Handy constraint kind.
type KnownNonEmptyString string = (KnownSymbol string, CmpSymbol string "" ~ 'GT)

-- | A text that is not empty.
newtype NonEmptyText =
  NonEmptyText
    { get :: Text
    } deriving (Eq, Show, Ord, Semigroup)

-- | Append regular text to a non-empty.
append :: NonEmptyText -> Text -> NonEmptyText
append (NonEmptyText x) y = NonEmptyText (x <> y)

-- | Runtime smart constructor.
mk :: Text -> Maybe NonEmptyText
mk x =
  if S.null x
    then Nothing
    else Just (NonEmptyText x)

-- | Statically checked string construction: NET.static (Proxy @"foo")
static ::
     forall string. KnownNonEmptyString string
  => Proxy string
  -> NonEmptyText
static p = NonEmptyText (fromString (symbolVal p))
