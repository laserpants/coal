-- +
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

{- |
Module: Coal.Compiler.Pass.TypePhase.ExpandLambdaMatchExpressions

Expand lambda-match expressions into standard lambda expressions with embedded match.

This pass transforms lambda-match expressions, which combine lambda abstraction
with pattern matching in a compact syntax, into their desugared form: a regular
lambda expression containing a match expression.

Lambda-match expressions provide a convenient shorthand for pattern-matching
lambdas. For example:

@
match {
  | [] => 0
  | x :: xs => x + 1
}
@

translates to:

@
fn($lambda_match) => match($lambda_match) {
  | [] => 0
  | x :: xs => x + 1
}
@

This transformation makes the binding of the match scrutinee explicit,
simplifying later compiler passes that work with standard lambda and match
constructs.
-}
module Coal.Compiler.Pass.TypePhase.ExpandLambdaMatchExpressions (
  ExpandContext (..),
  passExpandLambdaMatchExpressions,
) where

import Coal.AST.Shorthand (lambdaE, matchE, varE, varP)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (CompilerT)
import Coal.Language.Definition
import Coal.Language.Expression (Expression (ELambdaMatch))
import Coal.Language.Module (Module (..))
import Coal.Language.Type.Kind
import Data.Data (Data)
import Data.Generics.Uniplate.Data (transformM)
import Data.List.NonEmpty (NonEmpty (..))
import Extras (Dictionary)

{- | Lambda-match expression expansion pass.

Expand lambda-match expressions into standard lambda expressions with embedded
match statements. This desugaring makes the binding of the match scrutinee
explicit, transforming the compact lambda-match syntax into standard lambda
and match constructs for subsequent compiler passes.
-}
passExpandLambdaMatchExpressions :: (Monad m, Monoid a, Data a) => Pass a m (Module a Kind ()) (Module a Kind ())
passExpandLambdaMatchExpressions = Pass{runPass = passImpl}

passImpl :: (Monad m, Monoid a, Data a) => Module a Kind () -> CompilerT a m (Module a Kind ())
passImpl = expandLambdaMatchExpressions

class ExpandContext t where
  expandLambdaMatchExpressions :: (Monad m) => t -> CompilerT a m t

instance (ExpandContext a) => ExpandContext [a] where
  expandLambdaMatchExpressions = traverse expandLambdaMatchExpressions

instance (ExpandContext a) => ExpandContext (NonEmpty a) where
  expandLambdaMatchExpressions = traverse expandLambdaMatchExpressions

instance (ExpandContext a) => ExpandContext (Dictionary a) where
  expandLambdaMatchExpressions = traverse expandLambdaMatchExpressions

instance (Monoid a, Data a) => ExpandContext (Module a Kind ()) where
  expandLambdaMatchExpressions =
    \case
      Module{..} ->
        Module modulePath moduleExportList
          <$> expandLambdaMatchExpressions moduleDefinitions

instance (Monoid a, Data a) => ExpandContext (Definition a Kind ()) where
  expandLambdaMatchExpressions =
    \case
      DFunction loc name def ->
        DFunction loc name <$> expandLambdaMatchExpressions def
      DLet loc name def ->
        DLet loc name <$> expandLambdaMatchExpressions def
      o ->
        return o

instance (Monoid a, Data a) => ExpandContext (LetDefinition a Kind ()) where
  expandLambdaMatchExpressions =
    \case
      LetDefinition{..} -> do
        newLetDefinitionExpression <- expandLambdaMatchExpressions letDefinitionExpression
        return $
          LetDefinition
            { letDefinitionExpression = newLetDefinitionExpression
            , ..
            }

instance (Monoid a, Data a) => ExpandContext (FunctionDefinition a Kind ()) where
  expandLambdaMatchExpressions =
    \case
      FunctionDefinition{..} -> do
        newFunctionDefinitionExpression <- expandLambdaMatchExpressions functionDefinitionExpression
        return $
          FunctionDefinition
            { functionDefinitionExpression = newFunctionDefinitionExpression
            , ..
            }

instance (Monoid a, Data a) => ExpandContext (Expression a Kind ()) where
  expandLambdaMatchExpressions =
    transformM $
      \case
        ELambdaMatch _ _ clauses ->
          return $
            lambdaE
              (varP "$lambda_match" :| [])
              ( matchE
                  (varE "$lambda_match")
                  clauses
              )
        e ->
          return e
