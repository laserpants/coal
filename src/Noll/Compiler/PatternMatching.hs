{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler.PatternMatching (
  TypeProxy (..),
  compileEnvelope,
  compileMatchExprs,
) where

import Data.Data (Data)
import Data.Generics.Uniplate.Data (transformM)
import Noll.Common.List1 (List1, NonEmpty (..), fromList1)
import Noll.Common.Supply (suppliedName)
import Noll.Compiler.PatternMatching.Compiler (TypeProxy (..), compileEnvelope)
import Noll.Compiler.PatternMatching.Envelope (
  EnvelopeExpression (..),
  EnvelopePattern (..),
 )
import Noll.Compiler.PatternMatching.Equation (patternEquation)
import Noll.Compiler.PatternMatching.Rule (MatchMonad (..), matchPatterns)
import Noll.Compiler.Transform.Tree (replaceWith)
import Noll.Label (Label (..))
import Noll.Language (
  Binding (..),
  Choice (..),
  Clause (..),
  Constant (..),
  Definition (..),
  Expression (..),
  Function (..),
  Module (..),
  Pattern (..),
 )
import Noll.Utils (Dictionary)

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

instance (Show a, Data a, Show t, Data t, TypeProxy t, Ord t, Monoid a) => MatchExpressionContext (Module a k t) where
  compileMatchExprs =
    \case
      Module p ns os ->
        Module p ns <$> compileMatchExprs os

instance (Show a, Data a, Data t, Show t, TypeProxy t, Ord t, Monoid a) => MatchExpressionContext (Definition a k t) where
  compileMatchExprs =
    \case
      DAnnotation u d ->
        DAnnotation u <$> compileMatchExprs d
      DFunction name f ->
        DFunction name <$> compileMatchExprs f
      DConstant name c ->
        DConstant name <$> compileMatchExprs c
      _ ->
        error "TODO"

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

instance (Show a, Data a, Show t, Data t, TypeProxy t, Ord t, Monoid a) => MatchExpressionContext (Binding Expression a t) where
  compileMatchExprs =
    \case
      BPattern a p e ->
        BPattern a p <$> compileMatchExprs e
      BFunction{} ->
        error "TODO"

instance (Show a, Show t, Data a, Data t, TypeProxy t, Ord t, Monoid a) => MatchExpressionContext (Expression a t) where
  compileMatchExprs =
    transformM $
      \case
        EMatch _ _ e cs -> do
          cs1 <- compileMatchExprs cs
          name <- suppliedName
          replaceWith name
            <$> compileMatchExprs e
            <*> compileClauses (Label (expressionType e) name) cs1
        e ->
          pure e

compileClauses :: (Show a, Data a, Show t, TypeProxy t, Monoid a, Ord t) => Label t -> List1 (Clause Expression a t) -> MatchMonad (Expression a t)
compileClauses ll cs = compileEnvelope <$> matchPatterns [ll] eqs MFail
 where
  eqs = uncurry patternEquation . translateClause <$> fromList1 cs

translateClause :: (Show a, Data a, Show t, TypeProxy t) => Clause Expression a t -> ([EnvelopePattern (Expression a) t], EnvelopeExpression (Expression a) t)
translateClause (EClause _ p (CPlain _ _ e :| [])) =
  ([translatePattern p], MExpression e)
translateClause _ =
  error "TODO"

translatePattern :: (Show a, Data a, Show t, TypeProxy t) => Pattern a t -> EnvelopePattern (Expression a) t
translatePattern =
  \case
    PVariable _ ll ->
      MVariable ll
    PConstructor _ ll ps ->
      MConstructor ll (translatePattern <$> ps)
    p@(PLiteral a prim) ->
      MLiteral (patternType p) (ELiteral a prim)
    PAnnotation{} ->
      error "TODO"
    PAny{} ->
      error "TODO"
    PRecord{} ->
      error "TODO"
    PListCons{} ->
      error "TODO"
    PListLiteral{} ->
      error "TODO"
    POr{} ->
      error "TODO"
    PShorthand{} ->
      error "TODO"
    PAtVariable{} ->
      error "TODO"
