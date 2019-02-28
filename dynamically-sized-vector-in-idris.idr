-- This program does the following steps:
--
-- 1) Accept a line of input, parses it as an integer.
-- 2) Creates a vector of that size (populated with numbers up to n).
--
-- In a loop:
--
-- 1) Accepts an integer, attempts to cast it to a "Fin n" (as in
-- finite) number, which is within the bounds of the vector.
-- 2) Looks-up the value of the vector at index n and prints it.

main : IO ()
main = do
  putStrLn "How big should the vector be?"
  sizeString <- getLine
  case parseInt sizeString of
    Nothing =>
      putStrLn "Couldn't parse an integer."
    Just int =>
      do let size = fromInteger (cast int)
         let myvector = the (Vect size Integer) -- This is the magic part.
                            (enumVector size)
         print size
         print myvector
         forever
           (do putStrLn "Enter an index to look at: "
               indexString <- getLine
               case parseInt indexString of
                 Nothing => putStrLn "Couldn't parse an integer."
                 Just int =>
                   case integerToFin (the Integer (cast int)) size of
                     Nothing => putStrLn "Couldn't convert to number within bounds."
                     Just idx =>
                       do putStrLn "The value is:"
                          print (index idx myvector))

-- Things that Idris doesn't have out of the box:

charToInt : Char -> Maybe Int
charToInt c = let i = cast {to=Int} c in
              let zero = cast {to=Int} '0' in
              let nine = cast {to=Int} '9' in
              if i < zero || i > nine
                then Nothing
                else Just (i - zero)

total
parse' : Int -> List Int -> Maybe Int
parse' _   []      = Nothing
parse' acc [d]     = Just (10 * acc + d)
parse' acc (d::ds) = parse' (10 * acc + d) ds


total parseInt : String -> Maybe Int
parseInt str = (sequence (map charToInt (takeWhile isDigit (unpack str)))) >>= parse' 0

forever : IO () -> IO ()
forever m = m >>= \_ => forever m

enumVector : (n : Nat) -> Vect n Integer
enumVector x = reverse (go x) where
  go Z = []
  go (S n) = [toIntegerNat n] ++ go n
