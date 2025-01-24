{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler.PatternMatching (
  TypeProxy (..),
  compileEnvelope,
  compileMatchExprs,
) where

import Noll.Common.List1 (List1, NonEmpty (..), fromList1)
import Noll.Common.Supply (supplied, suppliedName)
import Noll.Compiler.PatternMatching.Compiler (MatchMonad (..), matchPatterns)
import Noll.Compiler.PatternMatching.Envelope (
  EnvelopeClause (..),
  EnvelopeExpression (..),
  EnvelopeHost (..),
  EnvelopePattern (..),
  fails,
 )
import Noll.Compiler.PatternMatching.Equation (patternEquation)
import Noll.Compiler.Transform.Expression (mapMOverExpression)
import Noll.Compiler.Transform.Tree (rename, replaceWith)
import Noll.Label (Label (..))
import Noll.Language (
  BinaryOperator (..),
  Binding (..),
  Choice (..),
  Clause (..),
  CompiledClause (..),
  Constant (..),
  Expression (..),
  Function (..),
  HasType (..),
  Intrinsic (..),
  Module (..),
  Object (..),
  Pattern (..),
  Type (..),
 )
import Noll.Utils (Dictionary (..), const2)

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

compileEnvelope :: (TypeProxy t, EnvelopeHost (Expression a) t, Monoid a, Eq t) => EnvelopeExpression (Expression a) t -> Expression a t
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

compileEnvelopeClause :: (TypeProxy t, EnvelopeHost (Expression a) t, Monoid a, Eq t) => EnvelopeClause (Expression a) t -> CompiledClause Expression a t
compileEnvelopeClause (EnvelopeClause l1 ls e) = ECompiledClause (l1 :| ls) (compileEnvelope e)

clauseList :: (TypeProxy t, EnvelopeHost (Expression a) t, Monoid a, Eq t) => [EnvelopeClause (Expression a) t] -> List1 (CompiledClause Expression a t)
clauseList ecs =
  case filter (not . fails) ecs of
    c : cs ->
      compileEnvelopeClause <$> (c :| cs)
    [] ->
      error "Implementation error"

--

class MatchExpressionContext a where
  compileMatchExprs :: a -> MatchMonad a

instance (MatchExpressionContext a) => MatchExpressionContext [a] where
  compileMatchExprs = traverse compileMatchExprs

instance (MatchExpressionContext a) => MatchExpressionContext (Maybe a) where
  compileMatchExprs = traverse compileMatchExprs

instance (MatchExpressionContext a) => MatchExpressionContext (List1 a) where
  compileMatchExprs = traverse compileMatchExprs

instance (MatchExpressionContext a) => MatchExpressionContext (Dictionary a) where
  compileMatchExprs = traverse compileMatchExprs

instance (Show a, Show t, TypeProxy t, Ord t, Monoid a) => MatchExpressionContext (Module a k t) where
  compileMatchExprs =
    \case
      Module p ns os ->
        Module p ns <$> compileMatchExprs os

instance (Show a, Show t, TypeProxy t, Ord t, Monoid a) => MatchExpressionContext (Object a k t) where
  compileMatchExprs =
    \case
      DFunction name f ->
        DFunction name <$> compileMatchExprs f
      DConstant name c ->
        DConstant name <$> compileMatchExprs c

instance (MatchExpressionContext (e a t)) => MatchExpressionContext (Function e a t) where
  compileMatchExprs =
    \case
      Function a u ps e ->
        Function a u ps <$> compileMatchExprs e

instance (MatchExpressionContext (e a t)) => MatchExpressionContext (Constant e a t) where
  compileMatchExprs =
    \case
      Constant a u e ->
        Constant a u <$> compileMatchExprs e

instance MatchExpressionContext (Clause Expression a t) where
  compileMatchExprs =
    \case
      EClause a p cs ->
        pure (EClause a p cs)

instance (Show a, Show t, TypeProxy t, Ord t, Monoid a) => MatchExpressionContext (Binding Expression a t) where
  compileMatchExprs =
    \case
      BPattern a p e ->
        BPattern a p <$> compileMatchExprs e

instance (Show a, Show t, TypeProxy t, Ord t, Monoid a) => MatchExpressionContext (Expression a t) where
  compileMatchExprs =
    \case
      EMatch a t e cs -> do
        cs1 <- compileMatchExprs cs
        name <- suppliedName
        replaceWith name <$> compileMatchExprs e <*> compileClauses (Label (expressionType e) name) cs1
      e ->
        mapMOverExpression compileMatchExprs e

compileClauses :: (Show a, Show t, TypeProxy t, Monoid a, Ord t) => Label t -> List1 (Clause Expression a t) -> MatchMonad (Expression a t)
compileClauses ll cs = compileEnvelope <$> matchPatterns [ll] eqs MFail
 where
  eqs = uncurry patternEquation . translateClause <$> fromList1 cs

translateClause :: (Show a, Show t, TypeProxy t) => Clause Expression a t -> ([EnvelopePattern (Expression a) t], EnvelopeExpression (Expression a) t)
translateClause (EClause _ p (CPlain _ _ e :| [])) =
  ([translatePattern p], MExpression e)
translateClause _ =
  error "TODO"

translatePattern :: (Show a, Show t, TypeProxy t) => Pattern a t -> EnvelopePattern (Expression a) t
translatePattern =
  \case
    PVariable _ ll ->
      MVariable ll
    PConstructor _ ll ps ->
      MConstructor ll (translatePattern <$> ps)
    p@(PLiteral a prim) ->
      MLiteral (patternType p) (ELiteral a prim)
