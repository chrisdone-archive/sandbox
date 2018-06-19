-- This module demonstrates a few mutable cells in Haskell, using the
-- mutable-containers package which provides a general interface.
module Main where
import Data.Mutable
import Control.Monad.ST
main = do
  -- IORef contains any boxed value. Pretty fast. Only works in IO.
  do ioref <- fmap asIORef (newRef 123)
     writeRef ioref 456
     v <- readRef ioref
     print v
  -- STRef contains any boxed value. Pretty fast. Only works in ST.
  print (runST (do stref <- fmap asSTRef (newRef 123)
                   writeRef stref 456
                   v <- readRef stref
                   return v))
  -- Unboxed reference. This will shave off some performance overhead
  -- on very tight loops. Works in either IO or ST. You can only put
  -- instances of Unbox in here: http://hackage.haskell.org/package/mutable-containers-0.3.3/docs/Data-Mutable.html#t:Unbox
  do uref <- fmap asURef (newRef (123::Int))
     writeRef uref 456
     v <- readRef uref
     print v
  print (runST (do uref <- fmap asURef (newRef (123::Int))
                   writeRef uref 456
                   v <- readRef uref
                   return v))
