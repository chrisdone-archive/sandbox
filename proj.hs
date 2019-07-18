{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall #-}

import Data.List.NonEmpty (NonEmpty(..))
import Data.String

newtype Title =
  Title String
  deriving (IsString, Show)

data CompletionKey
  = VariableName
  | OperatorName
  deriving (Show)

newtype Delim =
  Delim String
  deriving (Show, IsString)

data Tree
  = Keyword String
  | IntLit (Maybe Int)
  | ArbitraryText (Maybe String)
  | Variable CompletionKey (Maybe String)
  | Choice Title (NonEmpty Tree) (Maybe Tree)
  | List
      Title
      Tree
      Delim
      (Maybe (NonEmpty Tree))
  | Composite
      Title
      (NonEmpty Tree)
      (Maybe (NonEmpty Tree))
  deriving (Show)

data Cursor
  = InList Int Cursor
  | InComposite Int Cursor
  | InChoice Cursor
  | Here
  deriving (Show)

data Preview
  = KeywordPreview String
  | CompletionType String
  | Choices Title
  | ListOf Title
  | CompositePreview Title
  | IntLitPreview
  | ArbitraryTextPreview
  deriving (Show)

grammar :: Tree
grammar = expr
  where
    expr = Choice "expression" [let', app, op, list, tuple, parens, lit] Nothing
    parens = Composite "(..)" [Keyword "(", expr, Keyword ")"] Nothing
    let' =
      Composite
        "let"
        [Keyword "let", List "definitions" def ";" Nothing]
        Nothing
    def =
      Composite
        "definition"
        [Variable VariableName Nothing, Keyword "=", expr, Keyword "in", expr]
        Nothing
    app = Composite "application" [expr, expr] Nothing
    op = Composite "infix" [expr, Variable OperatorName Nothing, expr] Nothing
    list =
      Composite
        "list"
        [Keyword "[", List "list" expr "," Nothing, Keyword "]"]
        Nothing
    tuple =
      Composite
        "tuple"
        [Keyword "(", List "tuple" expr "," Nothing, Keyword ")"]
        Nothing
    lit = Choice "literal" [int, string] Nothing
    int = IntLit Nothing
    string =
      Composite
        "string"
        [Keyword "\"", ArbitraryText Nothing, Keyword "\""]
        Nothing

previewTree :: Tree -> Preview
previewTree =
  \case
    Keyword kw -> KeywordPreview kw
    Variable completionKey _ -> previewCompletionKey completionKey
    Choice title _ _ -> Choices title
    List title _ _ _ -> ListOf title
    Composite title _ _ -> CompositePreview title
    IntLit _ -> IntLitPreview
    ArbitraryText _ -> ArbitraryTextPreview

previewCompletionKey :: CompletionKey -> Preview
previewCompletionKey =
  \case
    VariableName -> CompletionType "variable-name"
    OperatorName -> CompletionType "operator"
