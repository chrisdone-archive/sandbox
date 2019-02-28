{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- | Indexed fields, for DB DSLs, validation DSLs and regular usage.

import Data.Functor.Identity

--------------------------------------------------------------------------------
-- A class for indexed types

class Indexed i a where
  type Index i (a :: *)

--------------------------------------------------------------------------------
-- Example data type

data Article i = Article { articleId :: Index i Int }

--------------------------------------------------------------------------------
-- Making values from Haskell

instance Indexed Identity a where
  type Index Identity a = a

identityArticle :: Article Identity
identityArticle = Article { articleId = 123 } -- Note the undecorated field value.

--------------------------------------------------------------------------------
-- Consuming values from forms

newtype Validate a = Validate (a -> Bool)

instance Indexed Validate a where
  type Index Validate a = Validate a

validateArticle :: Article Validate
validateArticle = Article { articleId = Validate (> 12) } -- A validation predicate.

--------------------------------------------------------------------------------
-- Querying values as database records

data Expr a where
   Null :: Expr a
   Val :: a -> Expr a

instance Indexed Expr a where
  type Index Expr a = Expr a

dbArticle :: Article Expr
dbArticle = Article { articleId = Null } -- A typical SQL value for a field.

--------------------------------------------------------------------------------
-- Optional fields

instance Indexed Maybe a where
  type Index Maybe a = Maybe a

optionalArticle :: Article Maybe
optionalArticle = Article { articleId = Just 1 }

--------------------------------------------------------------------------------
-- Meta

data Meta a = Meta String

instance Indexed Meta a where
  type Index Meta a = Meta a

metaArticle :: Article Meta
metaArticle = Article {articleId = Meta "Id"}
