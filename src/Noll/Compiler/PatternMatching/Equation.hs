{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler.PatternMatching.Equation (
  HeadConstructorEquation (..),
  HeadLiteralEquation (..),
  HeadVariableEquation (..),
  PatternEquation (..),
  PatternEquationBody (..),
  PatternEquationSet (..),
  groupByHeadConstructor,
  patternEquation,
  patternEquationSet,
) where

import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import Lang.Label (Label (..))
import Lang.Utils (Name, groupByEq)
import Noll.Compiler.PatternMatching.Envelope (EnvelopeExpression (..), EnvelopePattern (..))

data PatternEquationBody e t = PatternEquationBody [EnvelopePattern e t] (EnvelopeExpression e t)
  deriving (Show, Eq, Ord, Read)

data HeadLiteralEquation e t = HeadLiteralEquation (e t) (PatternEquationBody e t)
  deriving (Show, Eq, Ord, Read)

data HeadVariableEquation e t = HeadVariableEquation (Label t) (PatternEquationBody e t)
  deriving (Show, Eq, Ord, Read)

data HeadConstructorEquation e t = HeadConstructorEquation (Label t) [EnvelopePattern e t] (PatternEquationBody e t)
  deriving (Show, Eq, Ord, Read)

{-# INLINE headConstructorName #-}
headConstructorName :: HeadConstructorEquation e t -> Name
headConstructorName (HeadConstructorEquation (Label _ name) _ _) = name

data PatternEquation e t
  = HeadLiteral (HeadLiteralEquation e t)
  | HeadVariable (HeadVariableEquation e t)
  | HeadConstructor (HeadConstructorEquation e t)
  | EmptyEquation (EnvelopeExpression e t)
  deriving (Show, Eq, Ord, Read)

patternEquation :: [EnvelopePattern e t] -> EnvelopeExpression e t -> PatternEquation e t
patternEquation [] ee = EmptyEquation ee
patternEquation (p : ps) ee =
  case p of
    MConstructor name qs ->
      HeadConstructor (HeadConstructorEquation name qs body)
    MVariable name ->
      HeadVariable (HeadVariableEquation name body)
    MLiteral _ e ->
      HeadLiteral (HeadLiteralEquation e body)
 where
  body = PatternEquationBody ps ee

maybeHeadLiteralEquation :: PatternEquation e t -> Maybe (HeadLiteralEquation e t)
maybeHeadLiteralEquation =
  \case
    HeadLiteral eq -> Just eq
    _ -> Nothing

maybeHeadVariableEquation :: PatternEquation e t -> Maybe (HeadVariableEquation e t)
maybeHeadVariableEquation =
  \case
    HeadVariable eq -> Just eq
    _ -> Nothing

maybeHeadConstructorEquation :: PatternEquation e t -> Maybe (HeadConstructorEquation e t)
maybeHeadConstructorEquation =
  \case
    HeadConstructor eq -> Just eq
    _ -> Nothing

maybeEmptyEquation :: PatternEquation e t -> Maybe (EnvelopeExpression e t)
maybeEmptyEquation =
  \case
    EmptyEquation ee -> Just ee
    _ -> Nothing

data PatternEquationType
  = HeadLiteralType
  | HeadVariableType
  | HeadConstructorType
  | EmptyEquationType
  deriving (Show, Eq, Ord, Read)

{-# INLINE equationType #-}
equationType :: PatternEquation e t -> PatternEquationType
equationType =
  \case
    HeadLiteral{} ->
      HeadLiteralType
    HeadVariable{} ->
      HeadVariableType
    HeadConstructor{} ->
      HeadConstructorType
    EmptyEquation{} ->
      EmptyEquationType

{-# INLINE equationGroups #-}
equationGroups :: [PatternEquation e t] -> [[PatternEquation e t]]
equationGroups = groupByEq equationType

data PatternEquationSet e t
  = AllHeadLiteral [HeadLiteralEquation e t]
  | AllHeadVariable [HeadVariableEquation e t]
  | AllHeadConstructor [HeadConstructorEquation e t]
  | AllEmpty [EnvelopeExpression e t]
  | Mixed [[PatternEquation e t]]
  deriving (Show, Eq, Ord, Read)

patternEquationSet :: [PatternEquation e t] -> PatternEquationSet e t
patternEquationSet equations =
  case equationGroups equations of
    [eqs] ->
      case ( mapMaybe maybeHeadLiteralEquation eqs
           , mapMaybe maybeHeadVariableEquation eqs
           , mapMaybe maybeHeadConstructorEquation eqs
           , mapMaybe maybeEmptyEquation eqs
           ) of
        (qs, [], [], []) ->
          AllHeadLiteral qs
        ([], qs, [], []) ->
          AllHeadVariable qs
        ([], [], qs, []) ->
          AllHeadConstructor qs
        ([], [], [], ees) ->
          AllEmpty ees
        _ ->
          error "Compiler error"
    eqss ->
      Mixed eqss

groupByHeadConstructor :: [HeadConstructorEquation e t] -> [[HeadConstructorEquation e t]]
groupByHeadConstructor = groupByEq headConstructorName . sortBy (compare `on` headConstructorName)
