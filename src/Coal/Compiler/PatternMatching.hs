{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.PatternMatching (
  MatchExpressionContext (..),
  TypeProxy (..),
  compileEnvelope,
) where

import Coal.AST.Transform (replaceWith)
import Coal.Common.Label (Label (..))
import Coal.Common.Supply (freshName, supplied)
import Coal.Compiler.PatternMatching.Compiler (TypeProxy (..), compileEnvelope)
import Coal.Compiler.PatternMatching.Envelope (EnvelopeExpression (..), EnvelopePattern (..))
import Coal.Compiler.PatternMatching.Equation (patternEquation)
import Coal.Compiler.PatternMatching.Rule (matchPatterns)
import Coal.Compiler.Stack (CompilerT (..))
import Coal.Language (Binding (..), Choice (..), Clause (..), Expression (..), IndexedType, Kind (..), Pattern (..), Primitive (..))
import Coal.Language.Module (Module (..))
import Data.Data (Data)
import Data.Generics.Uniplate.Data (transformBiM)
import Data.List.NonEmpty (NonEmpty (..), toList)
import Extras (Dictionary)
import TextShow (TextShow (showt))

class MatchExpressionContext a c where
  compileMatchExprs :: (Monad m) => c -> CompilerT a m c

instance (MatchExpressionContext a c) => MatchExpressionContext a [c] where
  compileMatchExprs = traverse compileMatchExprs

instance (MatchExpressionContext a c) => MatchExpressionContext a (Maybe c) where
  compileMatchExprs = traverse compileMatchExprs

instance (MatchExpressionContext a c) => MatchExpressionContext a (NonEmpty c) where
  compileMatchExprs = traverse compileMatchExprs

instance (MatchExpressionContext a c) => MatchExpressionContext a (Dictionary c) where
  compileMatchExprs = traverse compileMatchExprs

instance (Eq a, Data a, Monoid a) => MatchExpressionContext a (Module a Kind IndexedType) where
  compileMatchExprs = transformBiM (compileMatchExprsE :: (Monad m) => Expression a Kind IndexedType -> CompilerT a m (Expression a Kind IndexedType))

instance (Eq a, Data a, Monoid a) => MatchExpressionContext a (Clause a () IndexedType) where
  compileMatchExprs = transformBiM (compileMatchExprsE :: (Monad m) => Expression a () IndexedType -> CompilerT a m (Expression a () IndexedType))

instance (Eq a, Data a, Monoid a) => MatchExpressionContext a (Binding Expression a () IndexedType) where
  compileMatchExprs = transformBiM (compileMatchExprsE :: (Monad m) => Expression a () IndexedType -> CompilerT a m (Expression a () IndexedType))

instance (Eq a, Data a, Monoid a) => MatchExpressionContext a (Expression a () IndexedType) where
  compileMatchExprs = transformBiM (compileMatchExprsE :: (Monad m) => Expression a () IndexedType -> CompilerT a m (Expression a () IndexedType))

compileMatchExprsE :: (Eq a, Eq k, Data a, Data k, Monoid a, Monad m) => Expression a k IndexedType -> CompilerT a m (Expression a k IndexedType)
compileMatchExprsE =
  \case
    EMatch _ _ e cs -> do
      name <- supplied (freshName "match")
      replaceWith name e <$> compileClauses (Label (expressionType e) name) cs
    e ->
      pure e

compileClauses :: (Eq a, Eq k, Data a, Data k, Monoid a, Monad m) => Label IndexedType -> NonEmpty (Clause a k IndexedType) -> CompilerT a m (Expression a k IndexedType)
compileClauses ll cs = compileEnvelope <$> matchPatterns [ll] eqs MFail
 where
  eqs = uncurry patternEquation . translateClause <$> toList cs

type TranslatedClause e a k t = ([EnvelopePattern (e a k) t], EnvelopeExpression (e a k) t)

translateClause :: (Data a, Data k) => Clause a k IndexedType -> TranslatedClause Expression a k IndexedType
translateClause (EClause _ p (CPlain _ _ e :| [])) = ([translatePattern p], MExpression e)
translateClause _ = error "Implementation error"

translatePattern :: (Data a, Data k) => Pattern a k IndexedType -> EnvelopePattern (Expression a k) IndexedType
translatePattern =
  \case
    PVariable _ ll ->
      MVariable ll
    PConstructor _ ll ps ->
      MConstructor ll (translatePattern <$> ps)
    p@(PLiteral _ LUnit) ->
      MVariable (Label (patternType p) "_")
    p@(PLiteral a prim) ->
      MLiteral (patternType p) (ELiteral a prim)
    PAnnotation _ _ p ->
      translatePattern p
    PAny _ t ->
      MVariable (Label t "_")
    PListCons a t p1 p2 ->
      translatePattern (PConstructor a (Label t "$Cons") [p1, p2])
    PListLiteral a t ps ->
      translatePattern (translateListLiteral a t ps)
    PTuple a t (p :| ps) ->
      translatePattern (PConstructor a (Label t ("$Tuple" <> showt (length ps + 1))) (p : ps))
    _ ->
      error "Implementation error"

translateListLiteral :: a -> IndexedType -> [Pattern a k IndexedType] -> Pattern a k IndexedType
translateListLiteral a t [] = PConstructor a (Label t "$Nil") []
translateListLiteral a t (p : ps) = PConstructor a (Label t "$Cons") [p, translateListLiteral a t ps]
