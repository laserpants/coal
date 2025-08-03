{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.PatternMatching.Compiler (
  TypeProxy (..),
  compileEnvelope,
) where

import Coal.Common.Label (Label (..))
import Coal.Common.List1 (List1, NonEmpty (..))
import Coal.Compiler.PatternMatching.Envelope
import Coal.Language
import Data.Data (Data, Typeable)
import Extra (const2)

class TypeProxy t where
  expressionType :: (Data a) => Expression a t -> t
  patternType :: (Data a) => Pattern a t -> t
  envelopeExprType :: (Data a) => EnvelopeExpression (Expression a) t -> t
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

compileEnvelope :: (TypeProxy t, Ord t, Data a, Monoid a) => EnvelopeExpression (Expression a) t -> Expression a t
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
            (EBinaryOperator mempty (expressionType e1 `arrow` expressionType e1 `arrow` boolean) OEqualTo)
            (EVariable mempty ll :| [e1])
        )
        (compileEnvelope e2)
        (compileEnvelope e3)

compileEnvelopeClause :: (TypeProxy t, Ord t, Data a, Monoid a) => EnvelopeClause (Expression a) t -> CompiledClause a t
compileEnvelopeClause (EnvelopeClause (Label t name) ls e) =
  ECompiledClause (Label (folded t ls) name :| ls) (compileEnvelope e)

clauseList :: (TypeProxy t, Ord t, Data a, Monoid a) => [EnvelopeClause (Expression a) t] -> List1 (CompiledClause a t)
clauseList ecs =
  case filter (not . fails) ecs of
    c : cs ->
      compileEnvelopeClause <$> (c :| cs)
    [] ->
      error "Implementation error"
