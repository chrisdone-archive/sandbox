{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

-- Lets you write functions that accept all types but this specific
-- one.

import Data.Word

type family a /~ b where
  a /~ a = 'False
  _ /~ _ = 'True

foo :: (Integral i, i /~ Word8 ~ 'True) => i -> ()
foo = undefined

bar = foo (undefined ::  Int)

-- As expected, the below is not allowed:
-- bad = foo (undefined :: Word8)
