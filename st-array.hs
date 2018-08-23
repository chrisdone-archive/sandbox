import Data.Array.ST
import Data.Array.Unboxed

someArray :: UArray Int Int
someArray =
  runSTUArray
    (do arr <- newArray (0, 10) 0
        writeArray arr 0 666
        writeArray arr 1 42
        return arr)

value :: Int
value = (someArray ! 0) + (someArray ! 1)
