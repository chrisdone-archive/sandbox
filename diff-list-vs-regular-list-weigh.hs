-- | Demonstration of dlist vs regular list+reverse memory use.
--
-- chris@precision:~/Work/chrisdone/sandbox$ ./diff-list-vs-regular-list-weigh

-- Case          Allocated  GCs
-- list 1000        64,000    0
-- list 10000      640,000    0
-- list 100000   6,400,000    6
-- dlist 1000       64,000    0
-- dlist 10000     640,000    0
-- dlist 100000  6,400,000    6

{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wall #-}
import Weigh

main :: IO ()
main =
  mainWith
    (sequence_
       (concat
          [ [ func
              ("list " ++ show i)
              (\n0 ->
                 reverse
                   (let loop :: Int -> [Int] -> [Int]
                        loop 0 acc = acc
                        loop n acc = loop (n - 1) (n : acc)
                     in loop n0 []))
              i
            | i <- iters'
            ]
          , [ func
              ("dlist " ++ show i)
              (\n0 ->
                 let loop :: Int -> ([Int] -> [Int]) -> [Int]
                     loop 0 f = f []
                     loop n f = loop (n - 1) (f . (n :))
                  in loop n0 id)
              i
            | i <- iters'
            ]
          ]))
  where
    iters' = [1000, 10000, 100000]
