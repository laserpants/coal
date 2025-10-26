{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.PatternMatching.Rule (matchPatterns) where

import Coal.Common.Label (Label (..))
import Coal.Common.Supply (freshName, supplied)
import Coal.Compiler.PatternMatching.Envelope (
  EnvelopeClause (..),
  EnvelopeExpression (..),
  EnvelopeHost (..),
  EnvelopePattern (..),
 )
import Coal.Compiler.PatternMatching.Equation
import Coal.Compiler.Stack
import Extras (foldrM)

type MatchRule a m p e t = [Label t] -> [p e t] -> EnvelopeExpression e t -> CompilerT a m (EnvelopeExpression e t)

matchPatterns :: (Ord t, EnvelopeHost e t, Monad m) => MatchRule a m PatternEquation e t
matchPatterns us qs e =
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
      foldrM (matchPatterns us) e eqss

emptyRule :: (EnvelopeHost e t, Monad m) => MatchRule a m EnvelopeExpression e t
emptyRule us eqs e =
  case eqs of
    (MFail : es) ->
      emptyRule us es e
    (e1 : _) ->
      pure e1
    [] ->
      pure e

literalRule :: (Ord t, EnvelopeHost e t, Monad m) => MatchRule a m HeadLiteralEquation e t
literalRule [] _ _ = error "Implementation error"
literalRule (u : us) eqs ex = foldrM go ex eqs
 where
  go (HeadLiteralEquation lit (PatternEquationBody qs e)) e1 = do
    e2 <- matchPatterns us [patternEquation qs e] e1
    pure (MConditional u lit e2 e1)

variableRule :: (Ord t, EnvelopeHost e t, Monad m) => MatchRule a m HeadVariableEquation e t
variableRule [] _ _ = error "Implementation error"
variableRule (Label _ u : us) eqs ex = matchPatterns us (updateEq <$> eqs) ex
 where
  updateEq (HeadVariableEquation (Label _ name) (PatternEquationBody ps e)) =
    patternEquation ps (replace name u e)
constructorRule :: (Ord t, EnvelopeHost e t, Monad m) => MatchRule a m HeadConstructorEquation e t
constructorRule [] _ _ = error "Implementation error"
constructorRule (u@(Label t _) : us) eqs ex = do
  cs <- traverse processGroup (groupByHeadConstructor eqs)
  pure (MCase u (cs <> [EnvelopeClause (Label t "_") [] ex]))
 where
  processGroup qs@(HeadConstructorEquation con ps _ : _) = do
    vs <- mapM suppliedLabel ps
    EnvelopeClause con vs <$> matchPatterns (vs <> us) (shift <$> qs) ex
  processGroup _ =
    error "Implementation error"
  shift (HeadConstructorEquation _ ps (PatternEquationBody qs e)) =
    patternEquation (ps <> qs) e

suppliedLabel :: (Monad m) => EnvelopePattern e t -> CompilerT a m (Label t)
suppliedLabel =
  \case
    MVariable (Label t name) -> do
      prefix <- supplied (freshName "match")
      pure (Label t (prefix <> "." <> name))
    MConstructor (Label t _) _ ->
      Label t <$> supplied (freshName "match")
    MLiteral t _ ->
      Label t <$> supplied (freshName "match")
