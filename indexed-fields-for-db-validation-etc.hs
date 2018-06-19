{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- | Indexed fields, for DB DSLs, validation DSLs and regular usage.

import Data.Functor.Identity

data Article i = Article { articleId :: Index i Int }

class Indexed i a where
  type Index i (a :: *)

instance Indexed Validate a where
  type Index Validate a = Validate a
newtype Validate a = Validate (a -> Bool)

instance Indexed Identity a where
  type Index Identity a = a

instance Indexed DbExpr a where
  type Index DbExpr a = DbExpr a
data DbExpr a = Null

identityArticle :: Article Identity
identityArticle = Article { articleId = 123} -- Note the undecorated field value.

validateArticle :: Article Validate
validateArticle = Article { articleId = Validate (> 12)} -- A validation predicate.

dbArticle :: Article DbExpr
dbArticle = Article { articleId = Null} -- A typical SQL value for a field.
