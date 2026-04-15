{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Pass.TypePhase.TypeInference (passTypeInference) where

import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Build (Build (..), buildNames)
import Coal.Compiler.Build.Prep (replacePlaceholders)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Compiler.State
import Coal.Compiler.TypeInference (define, generateConstraints, generateKindConstraints, solveT)
import Coal.Graphviz.Dot (generateDotSyntax)
import Coal.Language (HasType (..), IndexedType, Kind, Trait (..), indexed, instanceLabel, rowNormalize, typeOf)
import Coal.Language.Definition
import Coal.Language.Module (Module (..))
import Coal.Language.Module.Path (principalPath)
import Coal.TypeSystem.Constraint (Constraint (Explicit))
import Coal.TypeSystem.Constraint.Assumption (Assumption (..))
import Coal.TypeSystem.Constraint.Generation.InferenceRule (InferenceRule (..))
import Coal.TypeSystem.Kind.Constraint.Solver (solveKindConstraints)
import Coal.TypeSystem.Kind.Substitution
import Coal.TypeSystem.Kind.Unification (KindUnifier (kindUnifierMonad))
import Coal.TypeSystem.Substitution (apply, normalizeTypeIndexes)
import Control.Monad.Except (MonadIO (..), forM, forM_)
import Control.Monad.State (get, gets, modify, runState)
import Data.Data (Data)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Text.Lazy (toStrict)
import Text.Pretty.Simple (pShowNoColor)

passTypeInference :: (MonadIO m, Monoid a, Data a, Eq a, Show a) => Pass a m (Module a Kind ()) (Module a Kind IndexedType)
passTypeInference = Pass{runPass = passImpl}

passImpl :: (MonadIO m, Monoid a, Data a, Eq a, Show a) => Module a Kind () -> CompilerT a m (Module a Kind IndexedType)
passImpl = runTypeInference

runTypeInference :: (MonadIO m, Monoid a, Data a, Eq a, Show a) => Module a Kind () -> CompilerT a m (Module a Kind IndexedType)
runTypeInference m = do
  --  defs <- traverse indexTypes ds
  --  (tdefs, _) <- typeDefinitionsC defs

  nm <- ti m -- builtinTraits m)
  liftIO $ Text.writeFile ("tmp/defs_" <> Text.unpack (principalPath (modulePath m))) (generateDotSyntax nm)
  --  liftIO $ Text.writeFile ("tmp/olddefs_" <> Text.unpack (principalPath (modulePath m))) (generateDot (Module p ns (normalizeTypeIndexes tdefs)))
  Build{..} <- getCurrentBuildC
  liftIO $ Text.writeFile ("tmp/names_" <> Text.unpack (principalPath (modulePath m))) (toStrict $ pShowNoColor $ buildNames)

  liftIO $ Text.writeFile ("tmp/build_" <> Text.unpack (principalPath (modulePath m))) (toStrict $ pShowNoColor $ Build{..})

  pure nm

ti :: (MonadIO m, Data a, Show a, Eq a) => Module a Kind () -> CompilerT a m (Module a Kind IndexedType)
ti modul = do
  indexed <- inferKinds modul
  newModule <- inferTypes indexed
  replacePlaceholders
  return newModule

inferTypes :: (MonadIO m, Data a, Show a, Eq a) => Module a Kind () -> CompilerT a m (Module a Kind IndexedType)
inferTypes modul = do
  Module{..} <- indexTypes modul

  forM_ moduleDefinitions $
    \def -> do
      generateConstraints def
      sub <- solveT
      defineName (apply sub def)

  CompilerState{..} <- get

  forM compilerAssumptions $
    \Assumption{..} ->
      case Environment.lookup assumptionName compilerNameStore of
        Nothing ->
          error "Name not in scope"
        Just s -> do
          insertConstraintsC [Explicit (RuleAssumptionExplicit assumptionMetadata t s) t s]
         where
          t = apply compilerSubstitution assumptionType

  sub <- solveT -- again?
  modify (overCompilerAssumptions (apply sub))

  let newModuleDefinitions = fmap (fmap rowNormalize) (apply sub moduleDefinitions)
  pure $
    Module
      { moduleDefinitions = normalizeTypeIndexes newModuleDefinitions
      , ..
      }

inferKinds :: (MonadIO m) => Module a Kind () -> CompilerT a m (Module a Kind ())
inferKinds m = do
  generateKindConstraints m
  constraints <- gets compilerKindConstraints
  case kindUnifierMonad (solveKindConstraints constraints) of
    Left err ->
      error (show err)
    Right sub -> do
      modify (overCompilerNameStore (replaceVariables . applyKinds sub))
      modify (overCompilerModuleWithPath (modulePath m) (replaceVariables . applyKinds sub))
      return (replaceVariables (applyKinds sub m))

defineName :: (Monad m, Data a) => Definition a Kind IndexedType -> CompilerT a m ()
defineName =
  \case
    def@(DFunction _ name FunctionDefinition{}) ->
      define name (typeOf def)
    def@(DLet _ name LetDefinition{}) ->
      define name (typeOf def)
    DInstance _ InstanceDefinition{..} -> do
      let trait = Trait instanceDefinitionTraitName instanceDefinitionType
      forM_ instanceDefinitionImplementations $
        \case
          def@(DFunction _ name _) ->
            define (instanceLabel trait name) (typeOf def)
          def@(DLet _ name _) ->
            define (instanceLabel trait name) (typeOf def)
          _ ->
            pure ()
    _ ->
      pure ()

indexTypes :: (Monad m, Traversable t) => t e -> CompilerT a m (t IndexedType)
indexTypes ds = run (indexed ds) =<< gets compilerSupply
 where
  run s m = do
    let (r, n) = runState s m
    updateSupplyC n
    pure r
