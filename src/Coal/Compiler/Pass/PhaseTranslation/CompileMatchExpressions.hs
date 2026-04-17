{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

{- |
Module: Coal.Compiler.Pass.PhaseTranslation.CompileMatchExpressions

Compile match expressions into decision trees for efficient pattern matching.

This pass transforms high-level match expressions with patterns into an
optimized compiled representation using decision trees. The pattern matching
compiler analyzes the patterns and generates an efficient sequence of tests
and branches that minimize redundant checks.

For example, a match expression:

@
match(value) {
  | Some(x) => x + 1
  | None => 0
}
@

is compiled into a decision tree that efficiently tests the constructor and
binds variables, avoiding redundant checks when multiple patterns share common
structure. The compilation process:

1. Translates patterns into an internal envelope representation
2. Generates pattern matching equations from clauses
3. Compiles equations into optimized decision trees using the pattern matching compiler
4. Produces @ECompiledMatch@ expressions with efficient branching

This transformation is essential for efficient pattern matching execution,
particularly for complex nested patterns where naive left-to-right matching
would perform redundant tests.
-}
module Coal.Compiler.Pass.PhaseTranslation.CompileMatchExpressions (
  passCompileMatchExpressions,
) where

import Coal.AST.Metadata (Metadata (..))
import Coal.AST.Rewrite (replaceWith)
import Coal.Common.Label (Label (..))
import Coal.Common.Supply (freshName, supplied)
import Coal.Compiler.Pass (Pass (..))
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

{- | Match expression compilation pass.

Transform match expressions into optimized decision trees using the pattern
matching compiler. Generate efficient branching code that minimizes redundant
constructor tests, particularly for complex nested patterns where multiple
patterns share common structure.
-}
passCompileMatchExpressions :: (Monad m) => Pass Metadata m (Module Metadata Kind IndexedType) (Module Metadata Kind IndexedType)
passCompileMatchExpressions = Pass{runPass = passImpl}

passImpl :: (Monad m) => Module Metadata Kind IndexedType -> CompilerT Metadata m (Module Metadata Kind IndexedType)
passImpl = compileMatchExprs

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
