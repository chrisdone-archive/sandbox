{-# LANGUAGE BangPatterns #-}

-- | Simple implementation of zeckendorf's fibonacci numbers.

import Data.Bits
import Data.List
import Numeric.Natural

fibs :: [Natural]
fibs = scanl (+) 1 (1 : fibs)

-- | Maps a natural number to non-consecutive fibonacci numbers.
zeck :: Natural -> [Natural]
zeck n | n <= 2 = [n]
zeck n = go (reverse (takeWhile (<= n) fibs)) 0 []
  where
    go [] total xs = xs
    go (inf:infs) !total !outfs =
      case compare (total + inf) n of
        EQ -> inf : outfs
        LT -> go (drop 1 infs) (total + inf) (inf : outfs)
        GT -> go infs total outfs
