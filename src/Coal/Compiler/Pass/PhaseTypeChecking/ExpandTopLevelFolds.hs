{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

{- |
Module: Coal.Compiler.Pass.PhaseTypeChecking.ExpandTopLevelFolds

Expand top-level fold definitions into let definitions with explicit recursion.

This pass transforms top-level fold definitions, which declare named recursive
functions using pattern matching over data types, into standard let definitions
containing lambda expressions with explicit recursive calls. This is distinct
from fold expressions within other expressions, which are handled by the
ExpandExpressionFolds pass.

Fold definitions use @-patterns to mark recursive positions in patterns. For
example, a top-level fold definition:

@
fold sum : List Nat -> Nat {
  | [] => 0
  | x :: @rest => x + sum(rest)
}
@

is expanded into:

@
let sum = fn(sum.expr) => match(sum.expr) {
  | [] => 0
  | x :: rest => x + sum(rest)
}
@

The pass validates that @-patterns only appear within constructor patterns and
reports errors for misplaced fold patterns. This transformation enables
structural recursion by making the recursive function binding explicit.
-}
module Coal.Compiler.Pass.PhaseTypeChecking.ExpandTopLevelFolds (
  passExpandTopLevelFolds,
) where

import Coal.Common.Label (Label (..), labelName)
import Coal.Common.Supply (freshName, supplied)
import Coal.Compiler.Journal (tellErrors)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Compiler.State
import Coal.Language (Choice (..), Clause (..), Expression (..), Kind (..), Pattern (..), Qualified (..))
import Coal.Language.AST.Builders (applicationE, lambda1E, matchE, varE)
import Coal.Language.AST.Rewrite (replace)
import Coal.Language.Definition
import Coal.Language.Module (Module (..))
import Coal.Language.Module.Path (principalPath)
import Control.Monad.Except (throwError)
import Control.Monad.State (get)
import Control.Monad.Writer (execWriter, tell)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (transform, transformM)
import Data.List.NonEmpty (NonEmpty (..))
import Extras (Name, foldrM)

{- | Top-level fold expansion pass.

Expand top-level fold definitions into let definitions with explicit lambda
expressions and pattern matching. Validate that @-patterns are only used within
constructor patterns and report errors for misplaced fold patterns. This
transformation enables structural recursion by making the recursive function
binding and call sites explicit.
-}
passExpandTopLevelFolds :: (Monad m, Monoid a, Data a) => Pass a m (Module a Kind ()) (Module a Kind ())
passExpandTopLevelFolds = Pass{runPass = passImpl}

passImpl :: (Monad m, Monoid a, Data a) => Module a Kind () -> CompilerT a m (Module a Kind ())
passImpl Module{..} = do
  setCurrentModuleC Module{..}
  newModuleDefinitions <- traverse expandTopLevelFolds moduleDefinitions
  return $
    Module
      { moduleDefinitions = newModuleDefinitions
      , ..
      }

expandTopLevelFolds :: (Monad m, Monoid a, Data a) => Definition a Kind () -> CompilerT a m (Definition a Kind ())
expandTopLevelFolds =
  \case
    DFold loc name FoldDefinition{foldDefinitionAnnotation, foldDefinitionClauses} -> do
      newExpression <- expandClauses foldDefinitionClauses
      let def =
            LetDefinition
              { letDefinitionMetadata = loc
              , letDefinitionAnnotation = foldDefinitionAnnotation
              , letDefinitionType = With [] ()
              , letDefinitionExpression = newExpression
              }
      return (DLet loc name def)
    o ->
      return o

expandClauses :: (Monad m, Monoid a, Data a) => NonEmpty (Clause a Kind ()) -> CompilerT a m (Expression a Kind ())
expandClauses clauses = do
  name <- supplied (freshName "fold")
  expr <- traverse (expandFolds name []) clauses
  return $ lambda1E (name <> ".expr") (matchE (varE (name <> ".expr")) expr)

class ExpandContext a e where
  expandFolds :: (Monad m) => Name -> [(Name, Label ())] -> e -> CompilerT a m e

instance (ExpandContext a e) => ExpandContext a [e] where
  expandFolds name = traverse . expandFolds name

instance (ExpandContext a e) => ExpandContext a (NonEmpty e) where
  expandFolds name = traverse . expandFolds name

instance (Monoid a, Data a) => ExpandContext a (Clause a Kind ()) where
  expandFolds name _ =
    \case
      EClause _ (PAtVariable loc _) _ -> do
        CompilerState{compilerCurrentPath = path} <- get
        tellErrors [FoldPatternOutsideConstructor (ErrorLocation (principalPath path) loc)]
        throwError PatternAnomaly
      EClause _ (PNamedFold loc _ _) _ -> do
        CompilerState{compilerCurrentPath = path} <- get
        tellErrors [FoldPatternOutsideConstructor (ErrorLocation (principalPath path) loc)]
        throwError PatternAnomaly
      EClause{..} -> do
        newClauseChoices <- expandFolds name (atLabels clausePattern) clauseChoices
        return $
          EClause
            clauseMetadata
            (transform eliminateAtPatterns clausePattern)
            newClauseChoices

instance (Monoid a, Data a) => ExpandContext a (Choice Expression a Kind ()) where
  expandFolds name lls =
    \case
      CPlain a gs e ->
        CPlain a gs <$> expandFolds name lls e

instance (Monoid a, Data a) => ExpandContext a (Expression a Kind ()) where
  expandFolds = flip . foldrM . const (uncurry updateName)

updateName :: (Monad m, Monoid a, Data a) => Name -> Label () -> Expression a Kind () -> CompilerT a m (Expression a Kind ())
updateName name label =
  return
    . replace
      (labelName label)
      (\loc _ -> applicationE (varE name) (EVariable loc label :| []))

eliminateAtPatterns :: Pattern a Kind () -> Pattern a Kind ()
eliminateAtPatterns =
  \case
    PNamedFold a _ ll ->
      PVariable a ll
    PAtVariable a ll ->
      PVariable a ll
    p ->
      p

atLabels :: (Data a, Data t) => Pattern a Kind t -> [(Name, Label t)]
atLabels = execWriter . transformM go
 where
  go =
    \case
      p@(PNamedFold _ name label) -> do
        tell [(name, label)]
        return p
      p ->
        return p
