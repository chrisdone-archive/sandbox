-- This module demonstrates boxed vectors. That means they
-- contain values which are thunks, aka values of kind *, aka
-- values which may contain _|_. You can write any Haskell
-- value in here.
--
-- Optimizations:
--
-- 1) Use unsafeFreeze to avoid copying. See its haddocks.

-- 2) Use unsafeRead/unsafeWrite. These are, clearly, unsafe
-- operations. Use your discretion. And write tests! Or better yet,
-- use Liquid Haskell so that you have a proof of bounds checks.

module Main where
import           Control.Monad.ST
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
main :: IO ()
main = do
  -- Using the IO monad:
  vec <-
    do v <- MV.new 1
       MV.write v 0 (1 :: Int)
       V.freeze v
  print vec
  -- Using the ST monad:
  print
    (runST
       (do v <- MV.new 1
           MV.write v 0 (1 :: Int)
           V.freeze v))

{-
Output:

> main
[1]
[1]

-}
