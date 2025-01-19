{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler.PatternMatching where

import Control.Monad.State (MonadState, evalState)
import Data.Function (on)
import Data.List (sortBy)
import Data.Maybe (mapMaybe)
import Noll.Common.Supply (supplyN)
import Noll.Compiler.Transform.Tree (rename)
import Noll.Label (Label (..))
import Noll.Language (Expression (..), Type (..))
import Noll.Utils (Name, foldrM, groupByEq)

import qualified Data.Text as Text

data EnvelopeClause e t = EnvelopeClause (Label t) [Label t] (EnvelopeExpression e t)
  deriving (Show, Eq, Ord, Read)

data EnvelopePattern e t
  = MConstructor (Label t) [EnvelopePattern e t]
  | MVariable (Label t)
  | MLiteral t (e t)
  deriving (Show, Eq, Ord, Read)

data EnvelopeExpression e t
  = MFail
  | MExpression (e t)
  | MCase (Label t) [EnvelopeClause e t]
  | MConditional (Label t) (e t) (EnvelopeExpression e t) (EnvelopeExpression e t)
  deriving (Show, Eq, Ord, Read)

class EnvelopeHost e t where
  replace :: Name -> Name -> e t -> e t

instance (Ord t) => EnvelopeHost (Expression a) t where
  replace = rename

instance (Ord t) => EnvelopeHost (EnvelopeClause a) t where
  replace = undefined

instance (Ord t) => EnvelopeHost (EnvelopeExpression a) t where
  replace = undefined

--

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

--

-- TODO: newtype?
type MatchRule m p e t = [Label t] -> [p e t] -> EnvelopeExpression e t -> m (EnvelopeExpression e t)

{-# INLINE matchPatterns #-}
matchPatterns :: (Ord t, EnvelopeHost e t) => [Label t] -> [PatternEquation e t] -> EnvelopeExpression e t -> EnvelopeExpression e t
matchPatterns us qs e = evalState (matchPatternsM us qs e) 0

matchPatternsM :: (Ord t, MonadState Int m, EnvelopeHost e t) => MatchRule m PatternEquation e t
matchPatternsM us qs e =
  case patternEquationSet qs of
    AllEmpty ees ->
      emptyRule us ees e
    AllHeadLiteral eqs ->
      literalRule us eqs e
    AllHeadVariable eqs ->
      variableRule us eqs e
    AllHeadConstructor eqs ->
      constructorRule us eqs e
    Mixed eqss ->
      foldrM (matchPatternsM us) e eqss

emptyRule :: (MonadState Int m, EnvelopeHost e t) => MatchRule m EnvelopeExpression e t
emptyRule us eqs e =
  case eqs of
    (MFail : es) ->
      emptyRule us es e
    (e1 : _) ->
      pure e1
    [] ->
      pure e

literalRule :: (Ord t, MonadState Int m, EnvelopeHost e t) => MatchRule m HeadLiteralEquation e t
literalRule [] _ _ = error "Implementation error"
literalRule (u : us) eqs ex = foldrM go ex eqs
 where
  go (HeadLiteralEquation lit (PatternEquationBody qs e)) e1 = do
    e2 <- matchPatternsM us [patternEquation qs e] e1
    pure (MConditional u lit e2 e1)

variableRule :: (Ord t, MonadState Int m, EnvelopeHost e t) => MatchRule m HeadVariableEquation e t
variableRule [] _ _ = error "Implementation error"
variableRule (Label _ u : us) eqs ex = matchPatternsM us (updateEq <$> eqs) ex
 where
  updateEq (HeadVariableEquation (Label _ name) (PatternEquationBody ps e)) =
    patternEquation ps (replace name u e)

constructorRule :: (Ord t, MonadState Int m, EnvelopeHost e t) => MatchRule m HeadConstructorEquation e t
constructorRule [] _ _ = error "Implementation error"
constructorRule (u@(Label t _) : us) eqs ex = do
  cs <- traverse processGroup (groupByHeadConstructor eqs)
  pure (MCase u (cs <> [EnvelopeClause (Label t "_") [] ex]))
 where
  processGroup qs@(HeadConstructorEquation con ps _ : _) = do
    ns <- supplyN (length ps)
    let vs = uncurry freshVar <$> zip ns ps
    EnvelopeClause con vs <$> matchPatternsM (vs <> us) (shift <$> qs) ex
  shift (HeadConstructorEquation _ ps (PatternEquationBody qs e)) =
    patternEquation (ps <> qs) e

-- TODO: ??
freshVar :: Int -> EnvelopePattern e t -> Label t
freshVar n =
  \case
    MVariable (Label t name) ->
      Label t (prefix <> ":" <> name)
    MConstructor (Label t _) _ ->
      Label t prefix
    MLiteral t _ ->
      Label t prefix
 where
  prefix = Text.pack ("$p:" <> show n)

--

compileEnvelope :: (EnvelopeHost (Expression a) t, Eq t) => EnvelopeExpression (Expression a) t -> Expression a t
compileEnvelope =
  \case
    MFail ->
      error "Pattern matching failure"
    MExpression expr ->
      expr
    MCase ll cs ->
      undefined -- ESimplifiedMatch (fromJust (tag e)) (EVariable ll) (NonEmpty.fromList (clauseList cs))
    MConditional ll e1 e2 e3 ->
      EIf
        undefined
        undefined
        ( EApplication
            undefined
            undefined -- booleanTypeTag
            undefined -- (EBinaryOperator (equalToTypeTag (fromJust (tag e1)), OEqualTo))
            undefined -- (EVariable ll :| [e1])
        )
        (compileEnvelope e2)
        (compileEnvelope e3)
