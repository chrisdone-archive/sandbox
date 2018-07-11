-- | Demonstration that dlists are not faster than cons and then reverse.

-- benchmarking deepseq/list/100000
-- time                 5.194 ms   (5.129 ms .. 5.263 ms)
--                      0.999 R²   (0.999 R² .. 1.000 R²)
-- mean                 5.191 ms   (5.169 ms .. 5.227 ms)
-- std dev              86.42 μs   (57.85 μs .. 113.8 μs)

-- benchmarking deepseq/dlist/100000
-- time                 5.314 ms   (5.257 ms .. 5.398 ms)
--                      0.999 R²   (0.997 R² .. 1.000 R²)
-- mean                 5.298 ms   (5.265 ms .. 5.340 ms)
-- std dev              114.7 μs   (79.13 μs .. 185.2 μs)

-- benchmarking head/list/100000
-- time                 4.914 ms   (4.845 ms .. 4.980 ms)
--                      0.999 R²   (0.999 R² .. 1.000 R²)
-- mean                 4.943 ms   (4.923 ms .. 4.972 ms)
-- std dev              71.90 μs   (56.73 μs .. 100.1 μs)

-- benchmarking head/dlist/100000
-- time                 5.040 ms   (4.950 ms .. 5.153 ms)
--                      0.999 R²   (0.998 R² .. 1.000 R²)
-- mean                 4.980 ms   (4.950 ms .. 5.011 ms)
-- std dev              93.72 μs   (62.24 μs .. 143.6 μs)

{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall #-}
import Criterion
import Criterion.Main

main :: IO ()
main =
  defaultMain
    [ bgroup
        "deepseq"
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
    ,  bgroup
         "head"
         [ bgroup
             "list"
             [ bench
                 (show iters)
                 (nf
                    (\n0 ->
                       head (reverse
                               (let loop :: Int -> [Int] -> [Int]
                                    loop 0 acc = acc
                                    loop n acc = loop (n - 1) (n : acc)
                                 in loop n0 [])))
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
                        in head (loop n0 id))
                    iters)
             ]
         ]
    ]
  where
    iters :: Int
    iters = 100000
