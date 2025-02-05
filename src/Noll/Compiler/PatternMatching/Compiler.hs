{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler.PatternMatching.Compiler (
  TypeProxy (..),
  compileEnvelope,
) where

import Noll.Common.List1 (List1, NonEmpty (..))
import Noll.Compiler.PatternMatching.Envelope (
  EnvelopeClause (..),
  EnvelopeExpression (..),
  EnvelopeHost (..),
  fails,
 )
import Noll.Language (
  BinaryOperator (..),
  CompiledClause (..),
  Expression (..),
  HasType (..),
  Intrinsic (..),
  Pattern (..),
  Type (..),
 )
import Noll.Utils (const2)

class TypeProxy t where
  expressionType :: Expression a t -> t
  patternType :: Pattern a t -> t
  envelopeExprType :: EnvelopeExpression (Expression a) t -> t
  arrow :: t -> t -> t
  boolean :: t

instance TypeProxy () where
  expressionType =
    const ()
  patternType =
    const ()
  envelopeExprType =
    const ()
  arrow =
    const2 ()
  boolean =
    ()

instance TypeProxy (Type o k) where
  expressionType =
    typeOf
  patternType =
    typeOf
  envelopeExprType =
    typeOf
  arrow =
    TArrow
  boolean =
    TIntrinsic IBool

compileEnvelope :: (TypeProxy t, Ord t, Monoid a) => EnvelopeExpression (Expression a) t -> Expression a t
compileEnvelope =
  \case
    MFail ->
      error "Pattern matching failure"
    MExpression expr ->
      expr
    e@(MCase ll cs) ->
      ECompiledMatch mempty (envelopeExprType e) (EVariable mempty ll) (clauseList cs)
    MConditional ll e1 e2 e3 ->
      EIf
        mempty
        (envelopeExprType e2)
        ( EApplication
            mempty
            boolean
            (EBinaryOperator mempty (expressionType e1 `arrow` expressionType e1 `arrow` boolean, OEqualTo))
            (EVariable mempty ll :| [e1])
        )
        (compileEnvelope e2)
        (compileEnvelope e3)

compileEnvelopeClause :: (TypeProxy t, Ord t, Monoid a) => EnvelopeClause (Expression a) t -> CompiledClause Expression a t
compileEnvelopeClause (EnvelopeClause l1 ls e) = ECompiledClause (l1 :| ls) (compileEnvelope e)

clauseList :: (TypeProxy t, Ord t, Monoid a) => [EnvelopeClause (Expression a) t] -> List1 (CompiledClause Expression a t)
clauseList ecs =
  case filter (not . fails) ecs of
    c : cs ->
      compileEnvelopeClause <$> (c :| cs)
    [] ->
      error "Implementation error"
