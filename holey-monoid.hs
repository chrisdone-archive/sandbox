-- | Monoids with holes. The 'HoleyMonoid' allows building monoidal values of which certain components are to be filled in later. For example:
--
-- > > let holey :: (Show a, Show b) => HoleyMonoid String r (a -> b -> r)
-- >       holey = now "x = " . later show . now ", y = " . later show
-- >
-- > > run holey 3 5
-- > "x = 3, y = 5"
--
-- This module is intended to be imported in qualified fashion, e.g.
--
-- > import qualified Data.HoleyMonoid as HM

import Prelude hiding (id, (.), map)
import Control.Applicative
import Control.Category
import Data.Monoid

-- | The type of a monoid with holes. The underlying monoid is represented by
-- type parameter @m@. The @r@ is the result type and stays polymorphic until the
-- very last moment when 'run' is called. The last argument @a@ is always a
-- function with zero or more arguments, finally resulting in @r@. Ordering the
-- arguments in this order allows holey monoids to be composed using `.`, stacking the
-- expected arguments. Note that the `Monoid` constraint is only used in the
-- identity 'HoleyMonoid' and in composing two 'HoleyMonoid's.
newtype HoleyMonoid m r a = HoleyMonoid { runHM :: (m -> r) -> a }

instance Monoid m => Category (HoleyMonoid m) where
  id    = now mempty
  f . g = f `bind` \a -> g `bind` \b -> now (a `mappend` b)

instance Functor (HoleyMonoid m r) where
  fmap f (HoleyMonoid g) = HoleyMonoid (f . g)

instance Applicative (HoleyMonoid m r) where
  pure x = HoleyMonoid (pure x)
  HoleyMonoid f <*> HoleyMonoid g = HoleyMonoid (f <*> g)

instance Monad (HoleyMonoid m r) where
  return x = HoleyMonoid (return x)
  HoleyMonoid f >>= g = HoleyMonoid (f >>= \x -> runHM (g x))

-- | Insert a constant monoidal value.
now :: m -> HoleyMonoid m r r
now a = HoleyMonoid ($ a)

-- | Insert a monoidal value that is not specified until the computation is
-- 'run'. The argument that is expected later is converted to the monoid type
-- using the given conversion function.
later :: (a -> m) -> HoleyMonoid m r (a -> r)
later f = HoleyMonoid (. f)

-- | Monadic indexed bind on the underlying 'Monoid' types.
bind :: HoleyMonoid m b c -> (m -> HoleyMonoid n a b) -> HoleyMonoid n a c
m `bind` f = HoleyMonoid $ \k -> runHM m (\a -> runHM (f a) k)

-- | Map between underlying 'Monoid' types.
map :: (m -> n) -> HoleyMonoid m r a -> HoleyMonoid n r a
map g m = HoleyMonoid (\k -> runHM m (k . g))

-- | Run the computation, resulting in a function that still expects some
-- arguments. The number of arguments that is still expected will be equal to
-- the number of 'later's the computation is built of.
run :: HoleyMonoid m m a -> a
run m = runHM m id
