{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

{- |
Module: Coal.Compiler.Pass.PhaseTranslation.DetectCallCycles

Call cycle detection for function definitions.

This pass analyzes the module's definition dependency graph to detect cyclic
function calls. It runs after trait dictionaries have been inserted by
passPlaceholders and fold expressions have been expanded, providing a final
check for call cycles before AST denormalization.

The analysis uses free variable analysis to extract dependencies between
definitions and applies topological sorting via strongly connected components
to identify cycles. Any detected cycles are reported as compilation errors.

A cycle occurs when function definitions have circular dependencies:

@
fun f(x) = g(x)
fun g(x) = f(x)
@

This creates a cycle @[f, g]@ that cannot be resolved. Note that structural
recursion through fold/match expressions is valid and not considered a cycle,
as the recursion is bounded by pattern matching.
-}
module Coal.Compiler.Pass.PhaseTranslation.DetectCallCycles (
  passDetectCallCycles,
) where

import Coal.Common.FreeVars (freeIn, notConstructor)
import Coal.Common.Label (Label, labelName)
import Coal.Compiler.Build (Build (..))
import Coal.Compiler.Journal (listenErrors, tellErrors)
import Coal.Compiler.Metadata (Metadata (..))
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (CompilerError (..), CompilerFailureMode (..), CompilerT, ErrorLocation (..), getCurrentBuildC)
import Coal.Language (Definition (..), IndexedType, Kind)
import Coal.Language.Definition (FunctionDefinition (..), InstanceDefinition (..), LetDefinition (..))
import Coal.Language.Expression (Expression)
import Coal.Language.Module (Module (..))
import Coal.Language.Module.Path (principalPath)
import Control.Monad (unless)
import Control.Monad.Except (throwError)
import qualified Data.Data
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List (nub)
import qualified Data.Set as Set
import Extras (Name)

passDetectCallCycles :: (Monad m) => Pass Metadata m (Module Metadata Kind IndexedType) (Module Metadata Kind IndexedType)
passDetectCallCycles = Pass{runPass = passImpl}

passImpl :: (Monad m) => Module Metadata Kind IndexedType -> CompilerT Metadata m (Module Metadata Kind IndexedType)
passImpl m = do
  detectCallCycles m
  return m

detectCallCycles :: (Monad m, Ord t, Data.Data.Data k, Data.Data.Data t) => Module Metadata k t -> CompilerT Metadata m ()
detectCallCycles m = do
  (_, es) <- listenErrors $ checkForCycles m
  unless (null es) (throwError CallCycleError)

checkForCycles :: (Monad m, Ord t, Data.Data.Data k, Data.Data.Data t) => Module Metadata k t -> CompilerT Metadata m ()
checkForCycles Module{..} = do
  -- Get fold names to exclude from cycle detection (mutually recursive folds are valid)
  Build{buildFolds} <- getCurrentBuildC
  let depGraph = buildDependencyGraph moduleDefinitions
  case topoSortDefs buildFolds depGraph of
    Left cycles -> do
      let moduleName = principalPath modulePath
      let errorLoc = ErrorLocation moduleName mempty
      tellErrors [CallCycle cycles errorLoc]
    Right _ ->
      return ()

{- | Build dependency graph from module definitions.

For each definition that contains executable code (functions, lets, instances),
extract the free variables and filter to only include names defined in the
current module. Constructors and imported names are excluded.

Returns a list of (name, dependencies) pairs suitable for topological sorting.
-}
buildDependencyGraph :: (Ord t, Data.Data.Data a, Data.Data.Data k, Data.Data.Data t) => [Definition a k t] -> [(Name, [Name])]
buildDependencyGraph defs =
  let definedNames = Set.fromList (getDefinedNames defs)
      depPairs = [(name, filter (`Set.member` definedNames) deps) | (name, deps) <- extractDependencies defs]
      namesWithDeps = Set.fromList (map fst depPairs)
      namesWithoutDeps = [(name, []) | name <- Set.toList definedNames, name `Set.notMember` namesWithDeps]
   in depPairs ++ namesWithoutDeps
 where
  getDefinedNames :: [Definition a k t] -> [Name]
  getDefinedNames = concatMap getDefName

  getDefName :: Definition a k t -> [Name]
  getDefName =
    \case
      DFunction _ name _ -> [name]
      DLet _ name _ -> [name]
      DFunctionGroup _ name _ -> [name]
      DInstance _ inst -> map getDefName' (instanceDefinitionImplementations inst)
      _ -> []

  getDefName' :: Definition a k t -> Name
  getDefName' =
    \case
      DFunction _ name _ -> name
      DLet _ name _ -> name
      _ -> error "Unexpected definition in instance"

{- | Extract dependencies from definitions.

For each definition, compute the set of free variables in its body and
return as a (name, dependencies) pair. Mutually recursive folds are filtered
out during topological sorting using the buildFolds set.
-}
extractDependencies :: forall a k t. (Ord t, Data.Data.Data a, Data.Data.Data k, Data.Data.Data t) => [Definition a k t] -> [(Name, [Name])]
extractDependencies = concatMap extractDependency
 where
  extractDependency :: Definition a k t -> [(Name, [Name])]
  extractDependency =
    \case
      DFunction _ name (FunctionDefinition{..}) ->
        let deps = getDeps functionDefinitionExpression
         in [(name, deps)]
      DLet _ name (LetDefinition{..}) ->
        let deps = getDeps letDefinitionExpression
         in [(name, deps)]
      DFunctionGroup _ name funDefs ->
        let deps = nub $ concatMap (\(FunctionDefinition{..}) -> getDeps functionDefinitionExpression) funDefs
         in [(name, deps)]
      DInstance _ (InstanceDefinition{..}) ->
        concatMap extractDependency instanceDefinitionImplementations
      _ ->
        []

  getDeps :: Expression a k t -> [Name]
  getDeps expr =
    let freeVars :: Set.Set (Coal.Common.Label.Label t)
        freeVars = Set.filter notConstructor (freeIn expr)
     in nub $ map labelName $ Set.toList freeVars

{- | Topologically sort definitions by dependencies.

Uses strongly connected components to detect cycles. Reports all cyclic
dependencies (both self-recursion and mutual recursion) EXCEPT for folds,
which represent valid structural recursion in Coal.

Folds are excluded because they are pattern-matched and structurally recursive,
making them safe. Regular recursive functions (using if/else) are not bounded
by pattern matching and should be caught.

If any problematic cycles exist, returns Left with the list of cycles.
Otherwise returns Right with a valid topological ordering.
-}
topoSortDefs :: Set.Set Name -> [(Name, [Name])] -> Either [[Name]] [Name]
topoSortDefs folds defs =
  if null problematicCycles
    then Right (concatMap flatten sccs)
    else Left problematicCycles
 where
  edges = [(name, name, deps) | (name, deps) <- defs]
  sccs = stronglyConnComp edges
  -- Report all cycles (self-recursion and mutual recursion)
  -- EXCEPT cycles where all participants are folds (valid structural recursion)
  problematicCycles =
    [ xs
    | CyclicSCC xs <- sccs
    , not (all (`Set.member` folds) xs) -- Exclude if all are folds
    ]
  flatten (AcyclicSCC x) = [x]
  flatten (CyclicSCC xs) = xs
