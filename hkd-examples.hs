{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, StandaloneDeriving, TypeFamilies #-}
import Control.Monad.Trans
import Control.Monad.State.Strict
import Data.Data
import Data.Functor.Identity
import Data.Maybe
type family HKD f a where
  HKD Identity a = a
  HKD f        a = f a

data Wibble f = Wibble { foo :: HKD f Int , bar :: HKD f Bool} deriving (Typeable)
deriving instance Data (Wibble Maybe)
deriving instance Data (Wibble Identity)
deriving instance Show (Wibble Identity)

-- | Produce all missing fields that are Nothing.
missingFields :: Data (t Maybe) => t Maybe -> [String]
missingFields v =
  map
    fst
    (filter
       ((== toConstr (Nothing :: Maybe ())) . snd)
       (zip fields (gmapQ toConstr v)))
  where
    fields = constrFields (toConstr v)

runMaybe :: (Data (t Identity), Data (t Maybe)) => t Maybe -> Maybe (t Identity)
runMaybe tMaybe =
  evalStateT
    (fromConstrM
       (do i <- get
           modify (+ 1)
           case gmapQi i cast tMaybe of
             Nothing -> lift Nothing
             Just v -> lift v)
       (toConstr tMaybe))
    0
