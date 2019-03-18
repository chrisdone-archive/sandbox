{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Represent money as pennies.

module Money
  ( Pennies
  , Pounds
  , addPennies
  , multPennies
  , divPennies
  , penniesToPounds
  ) where

import Text.Printf

newtype Pennies =
  Pennies
    { penniesCount :: Int
    }
  deriving (Eq, Ord, Enum)

instance Show Pennies where
  show (Pennies p) = show p ++ "p"

newtype Pounds =
  Pounds
    { poundsToPennies :: Pennies
    }
  deriving (Eq, Ord, Enum)

instance Show Pounds where
  show (Pounds (Pennies pennies)) =
    "£" ++ printf "%.2f" (fromIntegral pennies / 100 :: Double)

addPennies :: Pennies -> Pennies -> Pennies
addPennies (Pennies x) (Pennies y) = Pennies (x+y)

multPennies :: Int -> Pennies -> Pennies
multPennies y (Pennies x) = Pennies (x * y)

divPennies :: Int -> Pennies -> Pennies
divPennies y (Pennies x) = Pennies (div x y)

penniesToPounds :: Pennies -> Pounds
penniesToPounds = Pounds
