{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

{- |
Module: Coal.Compiler.Pass.PhaseTypeChecking.ExpandExpressionFolds

Expand fold expressions embedded within other expressions into explicit form.

This pass identifies and expands fold expressions that appear within the body
of other expressions, transforming them into recursive let bindings with lambda
expressions and pattern matching. This is distinct from top-level fold
definitions, which are handled by the ExpandTopLevelFolds pass.

Fold expressions allow structural recursion over data types using @-patterns
to bind recursive positions. For example:

@
fold(list) {
  | [] => 0
  | x :: @rest => 1 + rest
}
@

is expanded into:

@
let fold$1 =
  fn(fold$1.expr) =>
    match(fold$1.expr) {
      | [] => 0
      | x :: rest => 1 + fold$1(rest)
    }
  in
    fold$1(list)
@

The pass also validates that @-patterns are only used within fold contexts
and reports errors for misplaced or invalid fold patterns.
-}
module Coal.Compiler.Pass.PhaseTypeChecking.ExpandExpressionFolds (
  passExpandExpressionFolds,
) where

import Coal.Common.Label (Label (..), labelName)
import Coal.Common.Supply (freshName, supplied)
import Coal.Compiler.Journal (tellErrors)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Compiler.State
import Coal.Language (Choice (..), Clause (..), Expression (..), Pattern (..))
import Coal.Language.AST.Builders (applicationE, lambda1E, letE, matchE, varE)
import Coal.Language.AST.Flattening (flattenApplicationsDeep)
import Coal.Language.AST.Rewrite (replace)
import Coal.Language.Definition
import Coal.Language.Module (Module (..))
import Coal.Language.Module.Path (principalPath)
import Control.Monad (void)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State (get)
import Control.Monad.Writer (execWriter, tell)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (descendM, transformM)
import Data.List.NonEmpty (NonEmpty (..))
import Extras (Dictionary, Name, const2, traverse_)

{- | Expression fold expansion pass.

Expand fold expressions embedded within other expressions into explicit let
bindings with lambda expressions and pattern matching. Validate that @-patterns
are only used in appropriate fold contexts and report errors for misplaced
fold patterns. This transformation enables structural recursion by making the
recursive call points explicit.
-}
passExpandExpressionFolds :: (Monad m, Monoid a, Data a, Data k) => Pass a m (Module a k ()) (Module a k ())
passExpandExpressionFolds = Pass{runPass = passImpl}

passImpl :: (Monad m, Monoid a, Data a, Data k) => Module a k () -> CompilerT a m (Module a k ())
passImpl = compileFolds

class FoldContext a e where
  expandFolds :: (Monad m) => Name -> [Label ()] -> e -> CompilerT a m e
  expandMatch :: (Monad m) => e -> CompilerT a m ()

instance (FoldContext a e) => FoldContext a [e] where
  expandFolds name = traverse . expandFolds name
  expandMatch = traverse_ expandMatch

instance (FoldContext a e) => FoldContext a (NonEmpty e) where
  expandFolds name = traverse . expandFolds name
  expandMatch = traverse_ expandMatch

instance (Monoid a, Data a, Data k) => FoldContext a (Clause a k ()) where
  expandFolds name _ =
    \case
      EClause _ (PAtVariable loc _) _ -> do
        CompilerState{compilerCurrentPath = path} <- get
        tellErrors [FoldPatternOutsideConstructor (ErrorLocation (principalPath path) loc)]
        throwError PatternAnomaly
      EClause{..} ->
        EClause clauseMetadata
          <$> transformM eliminateAtPatterns clausePattern
          <*> expandFolds name (atLabels clausePattern) clauseChoices
  expandMatch EClause{..} = do
    void (checkPatterns clausePattern)
    expandMatch clauseChoices

checkPatterns :: (Monoid a, Data a, Data k, Monad m) => Pattern a k () -> CompilerT a m (Pattern a k ())
checkPatterns =
  \case
    PAtVariable loc _ -> do
      CompilerState{compilerCurrentPath = path} <- get
      tellErrors [FoldPatternInRegularMatch (ErrorLocation (principalPath path) loc)]
      throwError PatternAnomaly
    p ->
      descendM checkPatterns p

instance (Monoid a, Data a, Data k) => FoldContext a (Choice Expression a k ()) where
  expandFolds name lls =
    \case
      CPlain a gs e ->
        CPlain a gs <$> expandFolds name lls e
  expandMatch =
    \case
      CPlain _ _ e ->
        expandMatch e

instance (Monoid a, Data a, Data k) => FoldContext a (Expression a k ()) where
  expandFolds name lls expr = return (foldr (updateName name) expr lls)
  expandMatch _ = return ()

updateName :: (Monoid a, Data a, Data k) => Name -> Label () -> Expression a k () -> Expression a k ()
updateName name label =
  replace
    (labelName label)
    (const2 (applicationE (varE name) (EVariable mempty label :| [])))

eliminateAtPatterns :: (Monad m) => Pattern a k () -> CompilerT a m (Pattern a k ())
eliminateAtPatterns =
  \case
    PNamedFold loc _ _ -> do
      CompilerState{compilerCurrentPath = path} <- get
      tellErrors [NamedFoldNotAllowed (ErrorLocation (principalPath path) loc)]
      throwError PatternAnomaly
    PAtVariable a ll ->
      return (PVariable a ll)
    p ->
      return p

atLabels :: (Data a, Data k, Data t) => Pattern a k t -> [Label t]
atLabels =
  execWriter
    . transformM
      ( \case
          p@(PAtVariable _ label) -> do
            tell [label]
            return p
          p ->
            return p
      )

expandFoldExpr :: (Monad m, Monoid a, Data a, Data k) => NonEmpty (Expression a k ()) -> NonEmpty (Clause a k ()) -> CompilerT a m (Expression a k ())
expandFoldExpr args clauses = do
  name <- supplied (freshName "fold")
  expr <- traverse (expandFolds name []) clauses
  return $
    flattenApplicationsDeep $
      letE
        name
        ( lambda1E
            (name <> ".expr")
            (matchE (varE (name <> ".expr")) expr)
        )
        (applicationE (varE name) args)

class CompileContext a e where
  compileFolds :: (Monad m) => e -> CompilerT a m e

instance (CompileContext a e) => CompileContext a [e] where
  compileFolds = traverse compileFolds

instance (CompileContext a e) => CompileContext a (NonEmpty e) where
  compileFolds = traverse compileFolds

instance (CompileContext a e) => CompileContext a (Dictionary e) where
  compileFolds = traverse compileFolds

instance (Monoid a, Data a, Data k) => CompileContext a (Module a k ()) where
  compileFolds =
    \case
      Module{..} -> do
        setCurrentPathC modulePath
        newModuleDefinitions <- compileFolds moduleDefinitions
        return $
          Module
            { moduleDefinitions = newModuleDefinitions
            , ..
            }

instance (Monoid a, Data a, Data k) => CompileContext a (Definition a k ()) where
  compileFolds =
    \case
      DFunction a name def ->
        DFunction a name <$> compileFolds def
      DLet a name def ->
        DLet a name <$> compileFolds def
      DInstance a def ->
        DInstance a <$> compileFolds def
      o ->
        return o

instance (Monoid a, Data a, Data k) => CompileContext a (FunctionDefinition a k ()) where
  compileFolds =
    \case
      FunctionDefinition{..} -> do
        newFunctionDefinitionExpression <- compileFolds functionDefinitionExpression
        return $
          FunctionDefinition
            { functionDefinitionExpression = newFunctionDefinitionExpression
            , ..
            }

instance (Monoid a, Data a, Data k) => CompileContext a (LetDefinition a k ()) where
  compileFolds =
    \case
      LetDefinition{..} -> do
        newLetDefinitionExpression <- compileFolds letDefinitionExpression
        return $
          LetDefinition
            { letDefinitionExpression = newLetDefinitionExpression
            , ..
            }

instance (Monoid a, Data a, Data k) => CompileContext a (InstanceDefinition a k ()) where
  compileFolds =
    \case
      InstanceDefinition{..} -> do
        newInstanceDefinitionImplementations <- compileFolds instanceDefinitionImplementations
        return $
          InstanceDefinition
            { instanceDefinitionImplementations = newInstanceDefinitionImplementations
            , ..
            }

instance (Monoid a, Data a, Data k) => CompileContext a (Expression a k ()) where
  compileFolds =
    transformM $
      \case
        EFold _ _ es cs ->
          expandFoldExpr es cs
        e@(EMatch _ _ _ cs) -> do
          expandMatch cs
          return e
        e ->
          return e
