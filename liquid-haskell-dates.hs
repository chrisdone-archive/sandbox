{-# LANGUAGE ScopedTypeVariables #-}

-- | Modelling a calendar date statically.

module Date where

-- We define invariants on dates, such as how many days per month, including leap years.

{-@ type Day =
  {v:Int | v > 0 && v <= 31 } @-}

{-@ type Month day =
  {v:Int | v > 0 && v <= 12 && (day < 31 || not (v = 04 || v = 06 || v = 09 || v = 11)) } @-}

{-@ type Year month day =
  {v:Int | v > 0 && (month /= 2 || (day < 30 && (day < 29 ||  v mod 400 = 0 || (v mod 4 = 0 && v mod 100 /= 0)))) } @-}

-- We define a date type with a shadow liquid type encoding our invariants.

data Date = Date
  { day :: !Int
  , month :: !Int
  , year :: !Int
  } deriving (Show)
{-@ data Date = Date
  { day   :: Day
  , month :: Month day
  , year  :: Year month day
  } @-}

-- In order to construct a valid `Date`, we need to do all the proper runtime tests, or 
-- else Liquid Haskell complains at compile time that they're not satisfied.

main :: IO ()
main = do
  year :: Int <- readLn
  month :: Int <- readLn
  day :: Int <- readLn
  if year > 0
    then if month > 0 && month <= 12 && (day < 31 || not (month == 04 || month == 06 || month == 09 || month == 11))
           then if day > 0 && day <= 31 && valid_leap_days day month year
                  then print (Date day month year)
                  else error "Day is out of range!"
           else error "Month is out of range."
    else error "Year is out of range."
  where
    valid_leap_days day month year =
      (month /= 2 ||
       (day < 30 && (day < 29 || mod year 400 == 0 || (mod year 4 == 0 && mod year 100 /= 0))))


-- Examples:

works :: Date
works = Date 12 03 2017

works2 :: Date
works2 = Date 31 03 2017

works3 :: Date
works3 = Date 30 04 2017

works_leap_day :: Date
works_leap_day = Date 29 02 2016

-- Does not compile:
-- invalid_nov_day = Date 11 31 2017
-- invalid_month = Date 12 15 2017
-- invalid_leap_day = Date 29 02 2017
-- invalid_days d m y = Date 30 2 2000
-- invalid_bound = Date 31 04 2017
