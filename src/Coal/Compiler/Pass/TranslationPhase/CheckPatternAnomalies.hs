{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.TranslationPhase.CheckPatternAnomalies (
  passCheckPatternAnomalies,
) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Journal (listenErrors, tellErrors)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.PatternMatching.AnomalyDetection (exhaustive, translatePattern)
import Coal.Compiler.Stack
import Coal.Language (IndexedType, Kind (..))
import Coal.Language.Definition
import Coal.Language.Expression (Clause (..), Expression (..))
import Coal.Language.Expression.Binding (Binding (..))
import Coal.Language.Expression.Choice (Choice (..), Guard (..))
import Coal.Language.Module
import Coal.Language.Module.Path
import Control.Monad (unless)
import Control.Monad.Except (throwError)
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
      traverse_ (checkPatternAnomalies (principalPath $ modulePath m)) (moduleDefinitions m)
  unless (null es) (throwError PatternAnomaly)

class PatternContext c where
  checkPatternAnomalies :: (Monad m) => Name -> c -> CompilerT Metadata m ()

instance PatternContext (Definition Metadata k t) where
  checkPatternAnomalies name =
    \case
      DFunction _ name def ->
        checkPatternAnomalies name def
      DLet _ name def ->
        checkPatternAnomalies name def
      DInstance _ InstanceDefinition{..} ->
        traverse_ (checkPatternAnomalies name) instanceDefinitionImplementations
      _ ->
        pure ()

instance PatternContext (FunctionDefinition Metadata k t) where
  checkPatternAnomalies name =
    \case
      FunctionDefinition{..} ->
        checkPatternAnomalies name functionDefinitionExpression

instance PatternContext (LetDefinition Metadata k t) where
  checkPatternAnomalies name =
    \case
      LetDefinition{..} ->
        checkPatternAnomalies name letDefinitionExpression

instance PatternContext (Binding Expression Metadata k t) where
  checkPatternAnomalies name =
    \case
      BPattern a p e ->
        checkPatternAnomalies name e
      BFunction a n ps e ->
        checkPatternAnomalies name e

instance PatternContext (Choice Expression Metadata k t) where
  checkPatternAnomalies name =
    \case
      CPlain a gs e -> do
        traverse_ (checkPatternAnomalies name) gs
        checkPatternAnomalies name e

instance PatternContext (Guard Expression Metadata k t) where
  checkPatternAnomalies name =
    \case
      CGuard e ->
        checkPatternAnomalies name e

instance PatternContext (Clause Metadata k t) where
  checkPatternAnomalies name =
    \case
      EClause a p cs ->
        traverse_ (checkPatternAnomalies name) cs

instance PatternContext (Expression Metadata k t) where
  checkPatternAnomalies name =
    \case
      EAnnotation a t e ->
        checkPatternAnomalies name e
      EApplication a t e es -> do
        checkPatternAnomalies name e
        traverse_ (checkPatternAnomalies name) es
      ELambda a ps e ->
        checkPatternAnomalies name e
      ELet a gs e1 -> do
        traverse_ (checkPatternAnomalies name) gs
        checkPatternAnomalies name e1
      ERecursiveLet a p e1 e2 -> do
        checkPatternAnomalies name e1
        checkPatternAnomalies name e2
      EIf a t e1 e2 e3 -> do
        checkPatternAnomalies name e1
        checkPatternAnomalies name e2
        checkPatternAnomalies name e3
      ERecord a t d me -> do
        traverse_ (checkPatternAnomalies name) d
        traverse_ (checkPatternAnomalies name) me
      EListCons a t e1 e2 -> do
        checkPatternAnomalies name e1
        checkPatternAnomalies name e2
      EListLiteral a t es ->
        traverse_ (checkPatternAnomalies name) es
      ETuple a t es ->
        traverse_ (checkPatternAnomalies name) es
      EMatch a t e cs -> do
        checkPatternAnomalies name e
        checkExhaustive name a cs
        traverse_ (checkPatternAnomalies name) cs
      ELambdaMatch a t cs -> do
        checkExhaustive name a cs
        traverse_ (checkPatternAnomalies name) cs
      ESelect a ll e ->
        checkPatternAnomalies name e
      EFocus a n ll1 ll2 e1 e2 -> do
        checkPatternAnomalies name e1
        checkPatternAnomalies name e2
      EFFICall a t ll es e -> do
        traverse_ (checkPatternAnomalies name) es
        checkPatternAnomalies name e
      _ ->
        pure ()

checkExhaustive :: (Monad m) => Name -> Metadata -> NonEmpty (Clause Metadata k t) -> CompilerT Metadata m ()
checkExhaustive name loc cs = do
  isExhaustive <- exhaustive patterns
  unless isExhaustive $ do
    tellErrors [NonExhaustivePatterns (ErrorLocation name loc)]
 where
  patterns = NonEmpty.toList (translatePattern . clausePattern <$> cs)
