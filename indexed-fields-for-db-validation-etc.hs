{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- | Indexed fields, for DB DSLs, validation DSLs and regular
-- usage. Minimal type magic needed. Just a regular old type family.

import           Control.Monad.State
import           Control.Monad.Trans.Reader
import           Data.Functor.Identity
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Validation
import           GHC.Generics (Generic)
import           Text.Read

--------------------------------------------------------------------------------
-- A class for indexed types

-- We begin with a trivial class called indexed with an associated
-- type function Index.
class Indexed i a where
  type Index i (a :: *)

--------------------------------------------------------------------------------
-- Making values from Haskell

instance Indexed Identity a where
  type Index Identity a = a

identityArticle :: Article Identity
identityArticle =
  Article {articleTitle = "Some article", articleId = ArticleId 123}
  -- Note the undecorated field value. Sweet!

reverseTitle :: Article Identity -> String
reverseTitle (Article{articleTitle=title}) = title

--------------------------------------------------------------------------------
-- Optional fields (a simple example)

instance Indexed Maybe a where
  type Index Maybe a = Maybe a

optionalArticle :: Article Maybe
optionalArticle =
  Article {articleTitle = Nothing, articleId = Just (ArticleId 1)}

--------------------------------------------------------------------------------
-- Consuming values from forms

-- | Imagine replacing this with an Applicative formlet.
newtype Formlet a =
  Formlet (ReaderT (Map String String) (Validation [String]) a)
  deriving (Functor, Applicative)

field :: String -> (String -> Either String a) -> Formlet a
field name parser =
  Formlet
    (ReaderT
       (\fields ->
          case M.lookup name fields of
            Nothing -> Failure ["Missing field"]
            Just str ->
              case parser str of
                Left e -> Failure [e]
                Right v -> Success v))

instance Indexed Formlet a where
  type Index Formlet a = Formlet a

-- Now we don't have to care about order! This is an alternative to
-- applicative-do + recordwildcards.
validateArticle :: Article Formlet
validateArticle =
  Article
    { articleTitle = field "title" pure
    , articleId = field "id" (fmap ArticleId . readEither)
    }

-- | Go from a record describing a formlet, to a formlet producing a record.
--
-- NOTE: We could generate this trivially with template-haskell (or
-- perhaps Generics).
articleValidation :: Article Formlet -> Formlet (Article Identity)
articleValidation (Article x y) = Article <$> x <*> y

-- This idea might be generalizable. Perhaps any @Article f -> f
-- (Article Identity)@?

--------------------------------------------------------------------------------
-- Meta

-- Record meta data that can be recovered at runtime by simply using a
-- record field. No type-level labels magic needed.

-- See database example below.

newtype Meta a = Meta { getMeta :: String }

instance Indexed Meta a where
  type Index Meta a = Meta a

--------------------------------------------------------------------------------
-- Querying values as database records

-- See use of this in DB example.

data Expr a where
   Null :: Expr (Maybe a)
   Val :: Render a => a -> Expr a
   Get :: (r Meta -> Meta f) -> Selected r -> Expr f
   Equal :: Render a => Expr a -> Expr a -> Expr Bool
   And :: Expr a -> Expr a -> Expr Bool

instance Indexed Expr a where
  type Index Expr a = Expr a

--------------------------------------------------------------------------------
-- Updating records

-- See use of this in DB example.

data Updating a = Default | Set a

instance Indexed Updating a where
  type Index Updating a = Updating a

-- Could be generated with TH (possibly Generics), like making lenses.
updatingArticle :: Article Updating
updatingArticle = Article {articleId = Default, articleTitle = Default}

-- A class also works:

class Updateable f where
  update :: f Updating

instance Updateable Article where
  update = Article {articleId = Default, articleTitle = Default}

-- See below for example use, but it's pretty predictable.

--------------------------------------------------------------------------------
-- DB library

-- Values declared at top-level scope for each database entity.
data Entity a = Entity { entityVal :: a Meta, entityMeta :: String }

-- Values are produced in the Relational DSL which have unique names.
data Selected a = Selected { selectedVal :: a Meta, selectedMeta :: (String, Int) }

-- Can render to SQL. We use String for simplicity, but this could be
-- another AST or a Builder.
class Render e where
  render :: e -> String

instance Render Int where render = show

renderExpr :: Expr a -> String
renderExpr =
  \case
    Null -> "NULL"
    (Val a) -> render a
    (Get key (Selected obj (name,idx))) -> name <> "_" <> show idx <> "." <> getMeta (key obj)
    (Equal a b) -> "(" <> renderExpr a <> " = " <> renderExpr b <> ")"
    (And a b) -> "(" <> renderExpr a <> " AND " <> renderExpr b <> ")"

-- A simple query DSL that supports joins and filtering.
data Relational a where
  SelectFrom :: Entity a -> Relational (Selected a)
  Filter :: Expr Bool -> Relational ()
  Bind :: Relational a -> (a -> Relational b) -> Relational b
  Pure :: a -> Relational a

-- Generate a plan from a query.
planRelational :: Relational (Projection a) -> Plan
planRelational r = let (proj,plan) = runState (go r) (Plan mempty [] [])
                   in plan {planProjection = collapseProjection proj}
  where
    go :: Relational a -> State Plan a
    go =
      \case
        SelectFrom entity -> do
          selects <- gets planSelects
          i <-
            case M.lookup (entityMeta entity) selects of
              Just i -> pure i
              Nothing -> do
                modify
                  (\plan ->
                     plan
                       { planSelects =
                           M.insert (entityMeta entity) (M.size selects) selects
                       })
                pure (M.size selects)
          pure (Selected (entityVal entity) (entityMeta entity, i))
        Filter bool ->
          modify (\plan -> plan {planFilters = bool : planFilters plan})
        Pure a -> return a
        Bind m f -> go m >>= go . f

-- The plan is simply a set of entities to select, filters on them and
-- a final projection.
data Plan =
  Plan
    { planSelects :: Map String Int
    , planFilters :: [Expr Bool]
    , planProjection :: [SomeProjection]
    }

-- Render the query to string. Dumb implementation.
renderPlan :: Plan -> String
renderPlan plan = "SELECT " <> project <> "\nFROM " <> select <> filters
  where
    project =
      intercalate
        ", "
        (map
           (\case
              SomeExpr e -> renderExpr e
              SomeSelected (Selected _ (name, idx)) -> name <> "_" <> show idx)
           (planProjection plan))
    select =
      intercalate
        ", "
        (map
           (\(t, index) -> t ++ " AS " ++ t ++ "_" ++ show index)
           (M.toList (planSelects plan)))
    filters =
      if null (planFilters plan)
        then ""
        else "\nWHERE " <>
             intercalate "\nAND " (map renderExpr (planFilters plan))

instance Applicative Relational where
  pure = return
  (<*>) = ap

instance Functor Relational where
  fmap = liftM

instance Monad Relational where
  (>>=) = Bind
  return = Pure

-- A final projection at the end of a query. We can return expressions
-- (i.e. constants or fields), or return whole records, and combine
-- them together as tuples.
data Projection a where
  ProjectExpr :: Expr a -> Projection a
  ConsProj :: Projection a -> Projection b -> Projection (a, b)
  ProjectSelected :: Selected e -> Projection (e Identity)

data SomeProjection
  = forall e. SomeExpr (Expr e)
  | forall e. SomeSelected (Selected e)

collapseProjection :: Projection a -> [SomeProjection]
collapseProjection =
  \case
    ProjectExpr e -> [SomeExpr e]
    ConsProj e es -> collapseProjection e <> collapseProjection es
    ProjectSelected e -> [SomeSelected e]

--------------------------------------------------------------------------------
-- Example

-- Declare our entity types

data Article i =
  Article
    { articleId :: Index (Defaulted i) ArticleId
    , articleTitle :: Index i String
    } deriving (Generic)
newtype ArticleId = ArticleId Int deriving (Show, Eq, Render)

-- Just because we can:

deriving instance Show (Article Identity)
deriving instance Eq (Article Identity)

data Author i =
  Author
    { authorId :: Index i AuthorId
    , authorName :: Index i String
    } deriving (Generic)
newtype AuthorId = AuthorId Int deriving (Show, Eq, Render)

data Authorship i =
  Authorship
    { authorshipArticle :: Index i ArticleId
    , authorshipAuthor  :: Index i AuthorId
    } deriving (Generic)

-- Declare value-level references for each entity (can be
-- TH-generated, or perhaps Generics) like deriving lenses, or
-- implemented manually for special cases.

entityArticle :: Entity Article
entityArticle =
  Entity
    { entityVal = Article {articleTitle = Meta "title", articleId = Meta "id"}
    , entityMeta = "article"
    }

entityAuthor :: Entity Author
entityAuthor =
  Entity
    { entityVal = Author {authorName = Meta "name", authorId = Meta "id"}
    , entityMeta = "author"
    }

entityAuthorship :: Entity Authorship
entityAuthorship =
  Entity
    { entityVal =
        Authorship
          {authorshipArticle = Meta "article", authorshipAuthor = Meta "author"}
    , entityMeta = "authorship"
    }

-- Or if you want to be really terse, just have a class. Example below of this too.

class Table a where table :: Entity a
instance Table Article where table = entityArticle
instance Table Author where table = entityAuthor
instance Table Authorship where table = entityAuthorship


-- Write a simple query that joins on several tables.
-- Imagine we have e.g.
--
-- >>> filter (article ! articleId ==. authorship ! authorshipArticle)
---
-- Handy operators to make the code look more concise. Same for the projection.

articleQuery :: Relational (Projection (Article Identity, String))
             -- ^ Note how easy this type could be converted to a
             -- FromRow instance (postgresql-simple, mysql-simple, etc).
articleQuery = do
  article <- SelectFrom entityArticle
  author <- SelectFrom table -- The class with method 'table' also works fine.
  authorship <- SelectFrom entityAuthorship
  Filter (Equal (Get articleId article) (Get authorshipArticle authorship))
  Filter (Equal (Get authorId author) (Get authorshipAuthor authorship))
  pure
    (ConsProj (ProjectSelected article) (ProjectExpr (Get authorName author)))

-- Plan the query

planArticle :: Plan
planArticle = planRelational articleQuery

-- SQL Output

-- > putStrLn $ renderPlan planArticle
-- SELECT article_0, author_1.name
-- FROM article AS article_0, author AS author_1, authorship AS authorship_2
-- WHERE (author_1.id = authorship_2.author)
-- AND (article_0.id = authorship_2.article)

-- I haven't fleshed out an API (time not permitting), but you can
-- imagine making a simple UPDATE API using the Updating type:

updateArticleExample :: Article Updating
updateArticleExample = update { articleTitle = Set "Article!"}

-- And you could do WHERE .. as with did above.

--------------------------------------------------------------------------------
-- Some playing around with defaultable fields

data Insertable a

type family Defaulted x where
  Defaulted Insertable = InsertOrDefault
  Defaulted x = x

instance Indexed Insertable a where
  type Index Insertable a = InsertOnly a

data InsertOnly a where
  InsertOnly :: a -> InsertOnly a

data InsertOrDefault a where
  Insert :: a -> InsertOrDefault a
  Defaulting :: InsertOrDefault a

instance Indexed InsertOrDefault a where
  type Index InsertOrDefault a = InsertOrDefault a

insertArticle :: Article Insertable
insertArticle = Article
  { articleId = Defaulting                 -- Here I can use defaulting.
  , articleTitle = InsertOnly ""           -- Here I cannot.
  }
