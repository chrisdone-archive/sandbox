{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

{-

> execWriterT $ document api
PathDoc "users" (PathDoc "profile" (MappendDoc [GetCaptureDoc "id",GetParamDoc "private_or_public"]))

-}

module RestRequest where

import Control.Applicative
import Control.Monad.Trans.Writer
import Control.Monad.Trans

data Request m a where
  Path :: String -> Request m a -> Request m a
  GetCapture :: String -> Request m String
  GetHeader :: String -> Request m String
  GetParam :: String -> Request m (Maybe String)
  GetBody :: Request m String
  Lift :: m a -> Request m a
  Fmap :: (z -> a) -> Request m z -> Request m a
  LiftA2 :: (y -> z -> a) -> Request m y -> Request m z -> Request m a
  Pure :: a -> Request m a

instance Functor (Request f) where
  fmap = Fmap

instance Applicative (Request f) where
  pure = Pure
  liftA2 = LiftA2

data Doc
  = PathDoc String Doc
  | GetHeaderDoc String
  | GetParamDoc String
  | GetCaptureDoc String
  | WriteHeaderDoc String
  | WriteBodyDoc
  | GetBodyDoc
  | MappendDoc [Doc]
  deriving (Show)

instance Monoid Doc where
  mempty = MappendDoc []
  mappend (MappendDoc xs) (MappendDoc ys) = MappendDoc (xs <> ys)
  mappend (MappendDoc xs) (ys) = MappendDoc (xs <> [ys])
  mappend xs (MappendDoc ys) = MappendDoc ([xs] <> ys)
  mappend x y = MappendDoc [x,y]

instance Semigroup Doc where
  (<>) = mappend

document :: Monad m => Request m a -> WriterT Doc m a
document =
  \case
    Lift m -> lift m
    Pure a -> pure a
    Fmap f x -> fmap f (document x)
    LiftA2 f x y -> liftA2 f (document x) (document y)
    Path piece rest -> censor (PathDoc piece) (document rest)
    GetHeader key -> do
      tell (GetHeaderDoc key)
      pure mempty
    GetCapture key -> do
      tell (GetCaptureDoc key)
      pure mempty
    GetParam key -> do
      tell (GetParamDoc key)
      pure mempty
    GetBody -> do
      tell GetBodyDoc
      pure mempty

api :: Request m (String, Maybe String)
api =
  Path
    "users"
    (Path "profile" ((,) <$> GetCapture "id" <*> GetParam "private_or_public"))
