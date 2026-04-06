{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.PatternMatching.Compiler (
  TypeProxy (..),
  compileEnvelope,
) where

import Coal.Common.Label (Label (..))
import Coal.Compiler.PatternMatching.Envelope
import Coal.Language
import Data.Data (Data, Typeable)
import Data.List.NonEmpty (NonEmpty (..))
import Extras (const2)

class TypeProxy t where
  expressionType :: (Data a, Data k) => Expression a k t -> t
  patternType :: (Data a, Data k) => Pattern a k t -> t
  envelopeExprType :: (Data a, Data k) => EnvelopeExpression (Expression a k) t -> t
  arrow :: t -> t -> t
  folded :: t -> [Label t] -> t
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
  folded =
    const2 ()

instance (Data k, Data (o k), Typeable o) => TypeProxy (Type o k) where
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
  folded t1 lls =
    foldType t1 (labelTag <$> lls)

infixr 1 `arrow`

compileEnvelope :: (Eq a, Eq k, TypeProxy t, Ord t, Data a, Data k, Monoid a) => EnvelopeExpression (Expression a k) t -> Expression a k t
compileEnvelope =
  \case
    MFail ->
      error "Pattern matching failure"
    MExpression expr ->
      expr
    e@(MCase ll cs) ->
      ECompiledMatch mempty (envelopeExprType e) (EVariable mempty ll) (clauseList cs)
    MConditional _ _ e2 e3
      | MFail == e3 ->
          compileEnvelope e2
    MConditional ll e1 e2 e3 ->
      EIf
        mempty
        (envelopeExprType e2)
        ( EApplication
            mempty
            boolean
            (EVariable mempty (Label (expressionType e1 `arrow` expressionType e1 `arrow` boolean) "(==)"))
            (EVariable mempty ll :| [e1])
        )
        (compileEnvelope e2)
        (compileEnvelope e3)

compileEnvelopeClause :: (Eq a, Eq k, TypeProxy t, Ord t, Data a, Data k, Monoid a) => EnvelopeClause (Expression a k) t -> CompiledClause a k t
compileEnvelopeClause (EnvelopeClause (Label t name) ls e) =
  ECompiledClause mempty (Label (folded t ls) name :| ls) (compileEnvelope e)

clauseList :: (Eq a, Eq k, TypeProxy t, Ord t, Data a, Data k, Monoid a) => [EnvelopeClause (Expression a k) t] -> NonEmpty (CompiledClause a k t)
clauseList ecs =
  case filter (not . fails) ecs of
    c : cs ->
      compileEnvelopeClause <$> (c :| cs)
    [] ->
      error "Implementation error"
