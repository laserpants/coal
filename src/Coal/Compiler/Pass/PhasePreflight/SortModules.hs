{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

{- |
Module: Coal.Compiler.Pass.PhasePreflight.SortModules

Sort modules in dependency order and detect cyclic dependencies.

This pass performs a topological sort of modules based on their import
dependencies, ensuring that each module is processed after all of its
dependencies. It also detects cyclic imports by identifying strongly
connected components in the module dependency graph.

The pass validates that:
- A Main module exists in the compilation unit
- All imported modules are present
- No cyclic dependencies exist between modules

Modules are returned in dependency order, where dependencies always appear
before the modules that depend on them. This ordering is essential for
correct compilation and type checking.
-}
module Coal.Compiler.Pass.PhasePreflight.SortModules (
  passSortModules,
) where

import Coal.Compiler.Build (Build (buildDependencies))
import Coal.Compiler.Build.Envelope (BuildEnvelope (..), envelopePathName)
import Coal.Compiler.Builtin.Modules (builtinModulesPaths)
import Coal.Compiler.Config (CompilerConfig (..))
import Coal.Compiler.Error (CompilerError (..), ErrorLocation (..))
import Coal.Compiler.Journal (listenErrors, tellErrors)
import Coal.Compiler.Metadata (Metadata (..))
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (CompilerFailureMode (..), CompilerT)
import Coal.Compiler.State (CompilerState (..))
import Coal.Language.Definition (Definition (DImport, DNamespaceImport))
import Coal.Language.Module (Module (..))
import Coal.Language.Module.Path (Path (Path), principalPath)
import Control.Monad (unless)
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State (get)
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List.Extra (notNull)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple.Extra (second)
import Extras (Name, concatForM, for, forM_)

{- | Module sorting and cycle detection pass.

Sort modules in topological order based on their import dependencies and
detect any cyclic imports. Validate that a Main module exists and all
imported modules are present. Return modules in dependency order where
each module appears after all of its dependencies.
-}
passSortModules :: (Monad m) => Pass Metadata m [BuildEnvelope (Module Metadata () ())] [BuildEnvelope (Module Metadata () ())]
passSortModules = Pass{runPass = passImpl}

passImpl :: (Monad m) => [BuildEnvelope (Module Metadata () ())] -> CompilerT Metadata m [BuildEnvelope (Module Metadata () ())]
passImpl units = do
  CompilerState{compilerConfig} <- get
  let requiredModule = case configEntryPoint compilerConfig of
        Just (moduleName, _) -> moduleName
        Nothing -> "Main"
  unless (requiredModule `elem` names) $ do
    tellErrors [NoModuleMain requiredModule]
    throwError PreflightFailure

  -- Collect edges and listen for any ModuleNotFound errors
  (edges, errors) <- listenErrors $ traverse (collectEdges names) units

  -- Fail immediately if any modules were not found
  unless (null errors) $ throwError PreflightFailure

  let sccs = stronglyConnComp edges
      cyclicSCCs = filter isCyclicSCC sccs
  forM_ cyclicSCCs $
    \scc ->
      tellErrors [ModuleCycle (envelopePathName <$> getModulesFromSCC scc)]
  if notNull cyclicSCCs
    then throwError PreflightFailure
    else return $ concatMap getModulesFromSCC sccs
 where
  names = Set.fromList (envelopePathName <$> units)

isCyclicSCC :: SCC (BuildEnvelope (Module Metadata () ())) -> Bool
isCyclicSCC =
  \case
    CyclicSCC _ ->
      True
    _ ->
      False

getModulesFromSCC :: SCC (BuildEnvelope (Module Metadata () ())) -> [BuildEnvelope (Module Metadata () ())]
getModulesFromSCC =
  \case
    AcyclicSCC u ->
      [u]
    CyclicSCC units ->
      units

unitDependencies :: BuildEnvelope (Module Metadata () ()) -> [(Metadata, Path)]
unitDependencies =
  \case
    BSource m ->
      dependencies m
    BCached b ->
      (mempty,) <$> buildDependencies b

dependencies :: (Monoid a) => Module a k t -> [(a, Path)]
dependencies (Module p _ defs)
  | principalPath p `elem` builtinModulesPaths = imported
  | otherwise = imported <> extra
 where
  imported = mapMaybe importPath defs
  extra =
    [ (mempty, Path ["Coal", "Applicative"])
    , (mempty, Path ["Coal", "Monad"])
    ]

importPath :: Definition a k t -> Maybe (a, Path)
importPath =
  \case
    DImport loc p _ ->
      Just (loc, p)
    DNamespaceImport loc p ->
      Just (loc, p)
    _ ->
      Nothing

collectEdges :: (Monad m) => Set Name -> BuildEnvelope (Module Metadata () ()) -> CompilerT Metadata m (BuildEnvelope (Module Metadata () ()), Name, [Name])
collectEdges names unit = do
  updatedDependencies <-
    concatForM deps $
      \(loc, dep) ->
        if Set.member dep names
          then return [dep]
          else do
            tellErrors [ModuleNotFound dep (ErrorLocation path loc)]
            return []
  return (unit, path, updatedDependencies)
 where
  deps = for (unitDependencies unit) (second principalPath)
  path = envelopePathName unit
