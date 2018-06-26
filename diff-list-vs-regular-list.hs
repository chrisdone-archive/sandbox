-- | Demonstration that dlists are not faster than cons and then reverse.

-- benchmarking list/100000
-- time                 5.444 ms   (5.393 ms .. 5.508 ms)
--                      0.998 R²   (0.996 R² .. 0.999 R²)
-- mean                 5.375 ms   (5.322 ms .. 5.445 ms)
-- std dev              187.9 μs   (137.4 μs .. 278.0 μs)
-- variance introduced by outliers: 17% (moderately inflated)

-- benchmarking dlist/100000
-- time                 5.325 ms   (5.274 ms .. 5.402 ms)
--                      0.998 R²   (0.997 R² .. 1.000 R²)
-- mean                 5.351 ms   (5.312 ms .. 5.400 ms)
-- std dev              142.2 μs   (99.77 μs .. 223.0 μs)

{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall #-}
import Criterion
import Criterion.Main

main :: IO ()
main =
  defaultMain
    [ bgroup
        "list"
        [ bench
            (show iters)
            (nf
               (\n0 ->
                  reverse
                    (let loop :: Int -> [Int] -> [Int]
                         loop 0 acc = acc
                         loop n acc = loop (n - 1) (n : acc)
                      in loop n0 []))
               iters)
        ]
    , bgroup
        "dlist"
        [ bench
            (show iters)
            (nf
               (\n0 ->
                  let loop :: Int -> ([Int] -> [Int]) -> [Int]
                      loop 0 f = f []
                      loop n f = loop (n - 1) (f . (n :))
                   in loop n0 id)
               iters)
        ]
    ]
  where
    iters :: Int
    iters = 100000
