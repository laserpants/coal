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
passInsertDictionaries and fold expressions have been expanded, providing a
final check for call cycles before AST denormalization.

The analysis uses free variable analysis to extract dependencies between
definitions and applies topological sorting via strongly connected components
to identify cycles. Any detected cycles are reported as compilation errors.

A cycle occurs when function definitions have circular dependencies:

@
fun f(x) = g(x)
fun g(x) = f(x)
@

This creates a cycle @[f, g]@ that cannot be resolved.

Top-level folds are handled specially. The @-patterns in a fold allow valid
structural recursion: a fold may re-invoke another fold on structurally smaller
subterms via the pattern machinery. Such references are recorded in
'buildFoldExprDeps' at expand time (they do not appear as free variables of the
expanded body, which is computed while the source patterns still exist). Any
reference made from a fold's expression body is an ordinary call, and cycles
through such calls are rejected exactly like cycles among regular functions.
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
import Coal.Compiler.Stack (
  CompilerError (..),
  CompilerFailureMode (..),
  CompilerT,
  ErrorLocation (..),
  getCurrentBuildC,
  setCurrentModuleC,
 )
import Coal.Language (Definition (..), IndexedType, Kind, Trait (..))
import Coal.Language.Definition (FunctionDefinition (..), InstanceDefinition (..), LetDefinition (..))
import Coal.Language.Expression (Expression)
import Coal.Language.Module (Module (..))
import Coal.Language.Module.Path (principalPath)
import Coal.Language.Serializable (instanceLabel)
import Control.Monad (unless)
import Control.Monad.Except (throwError)
import Data.Data (Data)
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List (nub)
import qualified Data.Set as Set
import Extras (Name)

passDetectCallCycles :: (Monad m) => Pass Metadata m (Module Metadata Kind IndexedType) (Module Metadata Kind IndexedType)
passDetectCallCycles = Pass{runPass = passImpl}

passImpl :: (Monad m) => Module Metadata Kind IndexedType -> CompilerT Metadata m (Module Metadata Kind IndexedType)
passImpl m = do
  setCurrentModuleC m
  detectCallCycles m
  return m

detectCallCycles :: (Monad m, Ord t, Data k, Data t) => Module Metadata k t -> CompilerT Metadata m ()
detectCallCycles m = do
  (_, es) <- listenErrors $ checkForCycles m
  unless (null es) (throwError CallCycleError)

checkForCycles :: (Monad m, Ord t, Data k, Data t) => Module Metadata k t -> CompilerT Metadata m ()
checkForCycles Module{modulePath, moduleDefinitions} = do
  -- Fold expression-level dependencies recorded during ExpandTopLevelFolds.
  -- These are the only edges that may legitimately reference other folds (via
  -- @-patterns); expression-level recursion among folds must still be caught.
  Build{buildFoldExprDeps} <- getCurrentBuildC
  let depGraph = buildDependencyGraph buildFoldExprDeps moduleDefinitions
  case topoSortDefs depGraph of
    Left cycles -> do
      let moduleName = principalPath modulePath
          cycleNames = fmap fst <$> cycles
          errorLoc = case cycles of
            (((_, metadata) : _) : _) -> ErrorLocation moduleName metadata
            _ -> ErrorLocation moduleName mempty
      tellErrors [CallCycle cycleNames errorLoc]
    Right _ ->
      return ()

{- | Build dependency graph from module definitions.

For each definition that contains executable code (functions, lets, instances),
extract the free variables and filter to only include names defined in the
current module. Constructors and imported names are excluded.

Top-level-fold-derived @DLet@ nodes use the expression-level dependencies
recorded in @foldExprDeps@ (the @-pattern structural recursion is not a free
variable of the expanded body). This keeps valid pattern recursion out of the
graph while still exposing cyclic expression-level calls.

Returns a list of (name, dependencies) pairs suitable for topological sorting.
-}
buildDependencyGraph :: (Ord t, Data a, Data k, Data t) => Map.Map Name (Set.Set Name) -> [Definition a k t] -> [((Name, a), [Name])]
buildDependencyGraph foldExprDeps defs =
  let definedNamePairs = getDefinedNames defs
      definedNames = Set.fromList (fst <$> definedNamePairs)
      depPairs = [(name, filter (`Set.member` definedNames) deps) | (name, deps) <- extractDependencies defs]
      namesWithDeps = Set.fromList ((fst . fst) <$> depPairs)
      namesWithoutDeps = [(nameLoc, []) | nameLoc <- definedNamePairs, fst nameLoc `Set.notMember` namesWithDeps]
   in depPairs <> namesWithoutDeps
 where
  getDefinedNames :: [Definition a k t] -> [(Name, a)]
  getDefinedNames = concatMap getDefName

  getDefName :: Definition a k t -> [(Name, a)]
  getDefName =
    \case
      DFunction loc name _ ->
        [(name, loc)]
      DLet loc name _ ->
        [(name, loc)]
      DInstance _ InstanceDefinition{..} ->
        let
          tr = Trait instanceDefinitionTraitName instanceDefinitionType
          getInstanceDefName =
            \case
              DFunction loc name _ ->
                [(instanceLabel tr name, loc)]
              DLet loc name _ ->
                [(instanceLabel tr name, loc)]
              _ ->
                []
         in
          concatMap getInstanceDefName instanceDefinitionImplementations
      _ ->
        []

  -- Extract dependencies from definitions.
  --
  -- For each definition, compute the set of free variables in its body and
  -- return as a (name, dependencies) pair. Fold-derived DLet definitions use
  -- their recorded expression-level dependencies instead of the expanded body
  -- free variables, so that structural recursion through @-patterns is not
  -- mistaken for an ordinary (cyclic) call.
  extractDependencies :: [Definition a k t] -> [((Name, a), [Name])]
  extractDependencies = concatMap extractDependency

  extractDependency :: Definition a k t -> [((Name, a), [Name])]
  extractDependency =
    \case
      DFunction
        _
        name
        FunctionDefinition
          { functionDefinitionMetadata
          , functionDefinitionExpression
          } ->
          [ ((name, functionDefinitionMetadata), getDeps functionDefinitionExpression)
          ]
      DLet _ name LetDefinition{letDefinitionMetadata, letDefinitionExpression}
        | name `Map.member` foldExprDeps ->
            [ ((name, letDefinitionMetadata), Set.toList (foldExprDeps Map.! name))
            ]
        | otherwise ->
            [ ((name, letDefinitionMetadata), getDeps letDefinitionExpression)
            ]
      DInstance _ InstanceDefinition{..} ->
        let
          tr = Trait instanceDefinitionTraitName instanceDefinitionType
          extractInstanceDependency =
            \case
              DFunction
                _
                name
                FunctionDefinition
                  { functionDefinitionMetadata
                  , functionDefinitionExpression
                  } ->
                  [ ((instanceLabel tr name, functionDefinitionMetadata), getDeps functionDefinitionExpression)
                  ]
              DLet _ name LetDefinition{letDefinitionMetadata, letDefinitionExpression} ->
                [ ((instanceLabel tr name, letDefinitionMetadata), getDeps letDefinitionExpression)
                ]
              _ ->
                []
         in
          concatMap extractInstanceDependency instanceDefinitionImplementations
      _ ->
        []

  getDeps :: Expression a k t -> [Name]
  getDeps expr =
    let freeVars :: Set.Set (Coal.Common.Label.Label t)
        freeVars = Set.filter notConstructor (freeIn expr)
     in nub (labelName <$> Set.toList freeVars)

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
topoSortDefs :: Set.Set Name -> [((Name, a), [Name])] -> Either [[(Name, a)]] [(Name, a)]
topoSortDefs folds defs =
  if null problematicCycles
    then Right (concatMap flatten sccs)
    else Left problematicCycles
 where
  edges = [((name, loc), name, deps) | ((name, loc), deps) <- defs]
  sccs = stronglyConnComp edges
  -- Report all cycles (self-recursion and mutual recursion)
  -- EXCEPT cycles where all participants are folds (valid structural recursion)
  problematicCycles =
    [ xs
    | CyclicSCC xs <- sccs
    , not (all (\(name, _) -> name `Set.member` folds) xs) -- Exclude if all are folds
    ]
  flatten (AcyclicSCC x) = [x]
  flatten (CyclicSCC xs) = xs
