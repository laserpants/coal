-- +
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

{- |
Module: Coal.Compiler.Pass.TypePhase.TypeInference
Description: Type inference and kind inference for Coal modules

This module implements bidirectional type inference using constraint generation
and solving. It performs both kind inference (ensuring type constructors have
correct kinds) and type inference (inferring types for expressions and patterns).

The inference process:
1. Generate kind constraints and solve them
2. Generate type constraints for each definition
3. Solve constraints incrementally
4. Apply substitutions and normalize types
-}
module Coal.Compiler.Pass.TypePhase.TypeInference (passTypeInference) where

import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Build.Prep (replacePlaceholders)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (CompilerT, insertConstraintsC, updateSupplyC)
import Coal.Compiler.State
import Coal.Compiler.TypeInference (define, generateConstraints, generateKindConstraints, solveT)
import Coal.Language (HasType (..), IndexedType, Kind, Trait (..), indexed, instanceLabel, rowNormalize, typeOf)
import Coal.Language.Definition
import Coal.Language.Module (Module (..))
import Coal.TypeSystem.Constraint (Constraint (Explicit))
import Coal.TypeSystem.Constraint.Assumption (Assumption (..))
import Coal.TypeSystem.Constraint.Generation.InferenceRule (InferenceRule (..))
import Coal.TypeSystem.Kind.Constraint.Solver (solveKindConstraints)
import Coal.TypeSystem.Kind.Substitution (KindSubstitutable (applyKinds, replaceVariables))
import Coal.TypeSystem.Kind.Unification (KindUnifier (kindUnifierMonad))
import Coal.TypeSystem.Substitution (apply, normalizeTypeIndexes)
import Control.Monad.Except (MonadIO (..), forM_)
import Control.Monad.State (get, gets, modify, runState)
import Data.Data (Data)
import qualified Data.Text as Text

{- | Type inference compiler pass

Infers kinds and types for all definitions in a module, returning a module
with fully annotated type information.
-}
passTypeInference :: (MonadIO m, Data a, Eq a, Show a) => Pass a m (Module a Kind ()) (Module a Kind IndexedType)
passTypeInference = Pass{runPass = passImpl}

passImpl :: (MonadIO m, Data a, Eq a, Show a) => Module a Kind () -> CompilerT a m (Module a Kind IndexedType)
passImpl = runTypeInference

runTypeInference :: (MonadIO m, Data a, Eq a, Show a) => Module a Kind () -> CompilerT a m (Module a Kind IndexedType)
runTypeInference m = do
  indexedM <- inferKinds m
  newM <- inferTypes indexedM
  replacePlaceholders
  return newM

inferTypes :: (MonadIO m, Data a, Show a, Eq a) => Module a Kind () -> CompilerT a m (Module a Kind IndexedType)
inferTypes m = do
  Module{..} <- assignTypeIndices m

  -- Generate and solve constraints for each definition
  forM_ moduleDefinitions $ \def -> do
    generateConstraints def
    sub <- solveT
    storeDefinitionType (apply sub def)

  CompilerState{..} <- get

  -- Verify all assumptions (explicit type annotations) are satisfied
  forM_ compilerAssumptions $ \Assumption{..} ->
    case Environment.lookup assumptionName compilerNameStore of
      Nothing ->
        error $
          "Type inference: assumption for name '"
            <> Text.unpack assumptionName
            <> "' not found in name store (internal compiler error)"
      Just s -> do
        insertConstraintsC [Explicit (RuleAssumptionExplicit assumptionMetadata t s) t s]
       where
        t = apply compilerSubstitution assumptionType

  -- Final solve and normalization
  sub <- solveT
  modify (overCompilerAssumptions (apply sub))

  let newDefinitions = fmap (fmap rowNormalize) (apply sub moduleDefinitions)
  pure $
    Module
      { moduleDefinitions = normalizeTypeIndexes newDefinitions
      , ..
      }

inferKinds :: (MonadIO m) => Module a Kind () -> CompilerT a m (Module a Kind ())
inferKinds m = do
  generateKindConstraints m
  constraints <- gets compilerKindConstraints
  case kindUnifierMonad (solveKindConstraints constraints) of
    Left err ->
      error $
        "Kind inference failed for module '"
          <> show (modulePath m)
          <> "': "
          <> show err
    Right sub -> do
      modify (overCompilerNameStore (replaceVariables . applyKinds sub))
      modify (overCompilerModuleWithPath (modulePath m) (replaceVariables . applyKinds sub))
      return (replaceVariables (applyKinds sub m))

storeDefinitionType :: (Monad m, Data a) => Definition a Kind IndexedType -> CompilerT a m ()
storeDefinitionType =
  \case
    def@(DFunction _ name _) ->
      define name (typeOf def)
    def@(DLet _ name _) ->
      define name (typeOf def)
    DInstance _ InstanceDefinition{..} -> do
      let trait = Trait instanceDefinitionTraitName instanceDefinitionType
      forM_ instanceDefinitionImplementations $ \case
        def@(DFunction _ name _) ->
          define (instanceLabel trait name) (typeOf def)
        def@(DLet _ name _) ->
          define (instanceLabel trait name) (typeOf def)
        _ ->
          pure ()
    -- Type definitions, aliases, traits don't need type storage
    _ ->
      pure ()

assignTypeIndices :: (Monad m, Traversable t) => t e -> CompilerT a m (t IndexedType)
assignTypeIndices ds = do
  CompilerState{..} <- get
  let (result, supply) = runState (indexed ds) compilerSupply
  updateSupplyC supply
  return result
