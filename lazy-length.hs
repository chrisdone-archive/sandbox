-- Lazy natural numbers for an efficient length comparisong.

{-
Equal-sized lists:

> genericLength [1..3] == (genericLength [1..3] :: Nat)
True

With infinite lists:

> genericLength [1..3] < (genericLength [1..] :: Nat)
True
> genericLength [1..3] == (genericLength [1..] :: Nat)
False
> genericLength [1..3] > (genericLength [1..] :: Nat)
False

-}

import Data.List

-- A natural number in peano style.
--
-- 0 = Zero
-- 1 = Add1 Zero
-- 2 = Add1 (Add1 Zero)
-- 3 = Add1 (Add1 (Add1 Zero))
-- ..
data Nat = Zero | Add1 Nat

instance Num Nat where
  Zero + y = y
  Add1 x + y = x + (Add1 y)

  fromInteger 0 = Zero
  fromInteger n = Add1 (fromInteger (n - 1))

-- This instance can be derived automatically, but we include a manual
-- implementation to demonstrate the idea:
instance Ord Nat where
  compare Zero Zero = EQ
  compare Zero (Add1 _) = LT
  compare (Add1 l) (Add1 r) = compare l r

-- This instance can be derived automatically, too.
instance Eq Nat where
  Zero == Zero = True
  Add1 x == Add1 y = x == y
  _ == _ = False
