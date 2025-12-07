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
import Coal.Compiler.Stack (CompilerT)
import Coal.Language (Binding (..), Choice (..), Clause (..), Expression (..), Pattern (..), Primitive (..))
import Coal.Language.Module (ConstantDefinition (..), Definition (..), FunctionDefinition (..), Module (..))
import Data.Data (Data)
import Data.Generics.Uniplate.Data (transformBiM, transformM)
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

type MatchClasses a t = (Show a, Data a, Monoid a, Show t, Data t, TypeProxy t, Ord t)

instance (Eq a, MatchClasses a t, Data k) => MatchExpressionContext a (Module a k t) where
  compileMatchExprs = transformBiM (compileMatchExprsE :: (Monad m) => Expression a t -> CompilerT a m (Expression a t))

instance (Eq a, MatchClasses a t, Data k) => MatchExpressionContext a (Definition a k t) where
  compileMatchExprs = transformBiM (compileMatchExprsE :: (Monad m) => Expression a t -> CompilerT a m (Expression a t))

instance (Eq a, MatchClasses a t) => MatchExpressionContext a (FunctionDefinition a t) where
  compileMatchExprs = transformBiM (compileMatchExprsE :: (Monad m) => Expression a t -> CompilerT a m (Expression a t))

instance (Eq a, MatchClasses a t) => MatchExpressionContext a (ConstantDefinition a t) where
  compileMatchExprs = transformBiM (compileMatchExprsE :: (Monad m) => Expression a t -> CompilerT a m (Expression a t))

instance (Eq a, MatchClasses a t) => MatchExpressionContext a (Clause a t) where
  compileMatchExprs = transformBiM (compileMatchExprsE :: (Monad m) => Expression a t -> CompilerT a m (Expression a t))

instance (Eq a, MatchClasses a t) => MatchExpressionContext a (Binding Expression a t) where
  compileMatchExprs = transformBiM (compileMatchExprsE :: (Monad m) => Expression a t -> CompilerT a m (Expression a t))

instance (Eq a, MatchClasses a t) => MatchExpressionContext a (Expression a t) where
  compileMatchExprs = transformM compileMatchExprsE

compileMatchExprsE :: (Eq a, MatchClasses a t, Monad m) => Expression a t -> CompilerT a m (Expression a t)
compileMatchExprsE =
  \case
    EMatch _ _ e cs -> do
      -- TODO
      name <- supplied (freshName "match")
      replaceWith name e <$> compileClauses (Label (expressionType e) name) cs
    e ->
      pure e

compileClauses :: (Eq a, MatchClasses a t, Monad m) => Label t -> NonEmpty (Clause a t) -> CompilerT a m (Expression a t)
compileClauses ll cs = compileEnvelope <$> matchPatterns [ll] eqs MFail
 where
  eqs = uncurry patternEquation . translateClause <$> toList cs

type TranslatedClause e a t = ([EnvelopePattern (e a) t], EnvelopeExpression (e a) t)

translateClause :: (MatchClasses a t) => Clause a t -> TranslatedClause Expression a t
translateClause (EClause _ p (CPlain _ _ e :| [])) =
  ([translatePattern p], MExpression e)
translateClause _ =
  error "TODO"

translatePattern :: (MatchClasses a t) => Pattern a t -> EnvelopePattern (Expression a) t
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

translateListLiteral :: (MatchClasses a t) => a -> t -> [Pattern a t] -> Pattern a t
translateListLiteral a t [] = PConstructor a (Label t "$Nil") []
translateListLiteral a t (p : ps) = PConstructor a (Label t "$Cons") [p, translateListLiteral a t ps]
