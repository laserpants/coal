{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

{- |
Module: Coal.Compiler.Pass.PhaseTranslation.CheckPatternAnomalies

Pattern exhaustiveness checking for match expressions.

Validates that pattern matching is exhaustive and reports missing cases. Handles
patterns in match expressions, with let-bindings and lambdas already desugared
by earlier compiler passes to ensure that all pattern contexts are checked.
-}
module Coal.Compiler.Pass.PhaseTranslation.CheckPatternAnomalies (
  passCheckPatternAnomalies,
) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Journal (listenErrors, tellErrors)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.PatternMatching.AnomalyDetection (exhaustive, translatePattern)
import Coal.Compiler.Stack
import Coal.Language.Definition
import Coal.Language.Expression (Clause (..), Expression (..))
import Coal.Language.Expression.Binding (Binding (..))
import Coal.Language.Expression.Choice (Choice (..), Guard (..))
import Coal.Language.Module (Module (moduleDefinitions, modulePath))
import Coal.Language.Module.Path (principalPath)
import Coal.Language.Type (IndexedType)
import Coal.Language.Type.Kind (Kind (..))
import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Trans (lift)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import Extras (Name, traverse_)

passCheckPatternAnomalies :: (Monad m) => Pass Metadata m (Module Metadata Kind IndexedType) (Module Metadata Kind IndexedType)
passCheckPatternAnomalies = Pass{runPass = passImpl}

passImpl :: (Monad m) => Module Metadata Kind IndexedType -> CompilerT Metadata m (Module Metadata Kind IndexedType)
passImpl m = do
  checkPatternAnomaliesM m
  return m

checkPatternAnomaliesM :: (Monad m) => Module Metadata k t -> CompilerT Metadata m ()
checkPatternAnomaliesM m = do
  (_, es) <-
    listenErrors $
      runReaderT (traverse_ checkPatternAnomalies (moduleDefinitions m)) (principalPath $ modulePath m)
  unless (null es) (throwError PatternAnomaly)

class PatternContext c where
  checkPatternAnomalies :: (Monad m) => c -> ReaderT Name (CompilerT Metadata m) ()

instance PatternContext (Definition Metadata k t) where
  checkPatternAnomalies =
    \case
      DFunction _ _ def ->
        checkPatternAnomalies def
      DLet _ _ def ->
        checkPatternAnomalies def
      DInstance _ InstanceDefinition{..} ->
        traverse_ checkPatternAnomalies instanceDefinitionImplementations
      _ ->
        pure ()

instance PatternContext (FunctionDefinition Metadata k t) where
  checkPatternAnomalies =
    \case
      FunctionDefinition{..} ->
        checkPatternAnomalies functionDefinitionExpression

instance PatternContext (LetDefinition Metadata k t) where
  checkPatternAnomalies =
    \case
      LetDefinition{..} ->
        checkPatternAnomalies letDefinitionExpression

instance PatternContext (Binding Expression Metadata k t) where
  checkPatternAnomalies =
    \case
      BPattern _ _ e ->
        checkPatternAnomalies e
      BFunction _ _ _ e ->
        checkPatternAnomalies e

instance PatternContext (Choice Expression Metadata k t) where
  checkPatternAnomalies =
    \case
      CPlain _ gs e -> do
        traverse_ checkPatternAnomalies gs
        checkPatternAnomalies e

instance PatternContext (Guard Expression Metadata k t) where
  checkPatternAnomalies =
    \case
      CGuard e ->
        checkPatternAnomalies e

instance PatternContext (Clause Metadata k t) where
  checkPatternAnomalies =
    \case
      EClause _ _ cs ->
        traverse_ checkPatternAnomalies cs

instance PatternContext (Expression Metadata k t) where
  checkPatternAnomalies =
    \case
      EAnnotation _ _ e ->
        checkPatternAnomalies e
      EApplication _ _ e es -> do
        checkPatternAnomalies e
        traverse_ checkPatternAnomalies es
      ELambda _ _ e ->
        checkPatternAnomalies e
      ELet _ gs e1 -> do
        traverse_ checkPatternAnomalies gs
        checkPatternAnomalies e1
      ERecursiveLet a p e1 e2 -> do
        checkPatternAnomalies e1
        checkPatternAnomalies e2
      EIf _ _ e1 e2 e3 -> do
        checkPatternAnomalies e1
        checkPatternAnomalies e2
        checkPatternAnomalies e3
      ERecord _ _ d me -> do
        traverse_ checkPatternAnomalies d
        traverse_ checkPatternAnomalies me
      EListCons _ _ e1 e2 -> do
        checkPatternAnomalies e1
        checkPatternAnomalies e2
      EListLiteral _ _ es ->
        traverse_ checkPatternAnomalies es
      ETuple _ _ es ->
        traverse_ checkPatternAnomalies es
      EMatch a _ e cs -> do
        checkPatternAnomalies e
        checkExhaustive a cs
        traverse_ checkPatternAnomalies cs
      ELambdaMatch a _ cs -> do
        checkExhaustive a cs
        traverse_ checkPatternAnomalies cs
      ESelect _ _ e ->
        checkPatternAnomalies e
      EFocus _ _ _ _ e1 e2 -> do
        checkPatternAnomalies e1
        checkPatternAnomalies e2
      EFFICall _ _ _ es e -> do
        traverse_ checkPatternAnomalies es
        checkPatternAnomalies e
      _ ->
        pure ()

checkExhaustive :: (Monad m) => Metadata -> NonEmpty (Clause Metadata k t) -> ReaderT Name (CompilerT Metadata m) ()
checkExhaustive loc cs = do
  name <- ask
  isExhaustive <- lift $ exhaustive patterns
  unless isExhaustive $
    lift $
      tellErrors [NonExhaustivePatterns (ErrorLocation name loc)]
 where
  patterns = NonEmpty.toList (translatePattern . clausePattern <$> cs)
