-- This module demonstrates unboxed vectors. That means that they can
-- contain only unboxable values. That's basically Int, Char, Bool,
-- (Int,Int), etc.
--
-- Use this one for efficiency when you are just using unboxable
-- types.

-- These vectors are stored as a byte array underneath:
-- <https://hackage.haskell.org/package/ghc-prim-0.5.0.0/docs/GHC-Prim.html#g:10>
-- Each element is not garbage collected (just the whole array), and
-- thus are very efficient. Your vector will probably fit in your CPU
-- cache, meaning you don't have to talk to mainline memory to do your
-- work.
--
-- There's a list of values that are unboxed here:
--
-- <https://hackage.haskell.org/package/ghc-prim-0.5.0.0/docs/GHC-Prim.html>
--
-- See Int#, Char#, etc.
--
--
-- 1) Use unsafeFreeze to avoid copying. See its haddocks.
--
-- 2) Use unsafeRead/unsafeWrite. These are, clearly, unsafe
-- operations. Use your discretion. And write tests! Or better yet,
-- use Liquid Haskell so that you have a proof of bounds checks.
--
module Main where
import           Control.Monad.ST
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Unboxed.Mutable as MV
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

-- Output:
{-
> main
[1]
[1]
-}
