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

There are two kinds of call edges between definitions:

1. /Ordinary calls/: a reference to another definition from an expression body
   (a function body, a let body, or a fold clause body). An ordinary call
   places no bound on the recursion it participates in.

2. /Structural recursion calls/ ("@-pattern calls"): references arising from
   @-patterns in a top-level fold's clause /patterns/ (e.g.
   @Array(encode_array(@vals))@). The invoked fold is applied to a subterm
   bound by destructuring, so recursion through such calls terminates.

Cycles are classified by the kinds of their edges:

- A cycle consisting solely of structural recursion calls is valid: this is
  the fold recursion scheme, e.g. encode_value, encode_array and encode_object
  mutually recursing through @-patterns.

- Any cycle containing at least one ordinary call edge is rejected, whether
  the remaining edges are ordinary or structural:

  @
  fold baz : X -> int32
    | A(foo(@x)) => x    -- structural recursion call to foo
    | B => 2
  fold foo : X -> int32
    | _ => baz(A(B))     -- ordinary call to baz; mixed cycle: rejected
  @

Ordinary edges are extracted by free variable analysis. Structural recursion
edges come from the fold @-pattern dependencies recorded by passPrepareBuild
in 'buildFoldPatternDeps' (they do not appear as free variables of the
expanded body, which is computed while the source patterns still exist; see
@Coal.Compiler.Pass.PhaseTypeChecking.ExpandTopLevelFolds@).

Strongly connected components are computed over all edges (both kinds). An
SCC is reported if and only if it contains an ordinary call edge whose
endpoints both lie inside the SCC: every edge between two nodes of the same
SCC lies on a cycle, and an edge between different SCCs never does. Hence a
cycle containing an ordinary call exists if and only if some SCC has an
internal ordinary edge.
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
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Extras (Name)

-- | The kind of a call edge in the dependency graph.
data EdgeKind
  = -- | Ordinary (unbounded) call: a reference in an expression body.
    Ordinary
  | -- | Structural recursion call: a reference in a fold @-pattern.
    Structural
  deriving (Eq, Show)

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
  -- Fold dependencies recorded during PrepareBuild. Expression-level
  -- references are ordinary calls; @-pattern references are structural
  -- recursion calls. The two maps give every fold-to-fold edge a kind, which
  -- is what lets the checker allow purely structural cycles while rejecting
  -- cycles that also contain ordinary calls.
  Build{buildFoldExprDeps, buildFoldPatternDeps} <- getCurrentBuildC
  let depGraph = buildDependencyGraph buildFoldExprDeps buildFoldPatternDeps moduleDefinitions
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
extract the call edges to other definitions of the current module and tag each
with its kind (ordinary vs. structural recursion). Constructors and imported
names are excluded.

Top-level-fold-derived @DLet@ nodes use the dependency maps recorded at
PrepareBuild time: 'foldExprDeps' contributes ordinary edges (the fold's clause
expression bodies) and 'foldPatternDeps' contributes structural recursion
edges (the fold's @-patterns, computed while the source patterns still exist).
This keeps valid pattern recursion out of the ordinary edges while still
exposing cyclic expression-level calls.

Returns a list of (name, dependencies) pairs suitable for topological sorting.
-}
buildDependencyGraph :: forall a k t. (Ord t, Data a, Data k, Data t) => Map.Map Name (Set.Set Name) -> Map.Map Name (Set.Set Name) -> [Definition a k t] -> [((Name, a), [(Name, EdgeKind)])]
buildDependencyGraph foldExprDeps foldPatternDeps defs =
  let definedNamePairs = getDefinedNames defs
      definedNames = Set.fromList (fst <$> definedNamePairs)
      depPairs = [(name, filter ((`Set.member` definedNames) . fst) deps) | (name, deps) <- extractDependencies defs]
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

  -- Extract call edges from definitions.
  --
  -- For each definition, compute the free variables of its body and return
  -- them as ordinary call edges. Fold-derived DLet definitions use their
  -- recorded expression-level and @-pattern dependencies instead of the
  -- expanded body free variables, so that structural recursion through
  -- @-patterns is not mistaken for an ordinary (cyclic) call.
  extractDependencies :: [Definition a k t] -> [((Name, a), [(Name, EdgeKind)])]
  extractDependencies = concatMap extractDependency

  extractDependency :: Definition a k t -> [((Name, a), [(Name, EdgeKind)])]
  extractDependency =
    \case
      DFunction
        _
        name
        FunctionDefinition
          { functionDefinitionMetadata
          , functionDefinitionExpression
          } ->
          [ ((name, functionDefinitionMetadata), ordinary (getDeps functionDefinitionExpression))
          ]
      DLet _ name LetDefinition{letDefinitionMetadata, letDefinitionExpression}
        | name `Map.member` foldExprDeps ->
            [
              ( (name, letDefinitionMetadata)
              , ordinary (Set.toList (foldExprDeps Map.! name))
                  <> structural (Set.toList (Map.findWithDefault Set.empty name foldPatternDeps))
              )
            ]
        | otherwise ->
            [ ((name, letDefinitionMetadata), ordinary (getDeps letDefinitionExpression))
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
                  [ ((instanceLabel tr name, functionDefinitionMetadata), ordinary (getDeps functionDefinitionExpression))
                  ]
              DLet _ name LetDefinition{letDefinitionMetadata, letDefinitionExpression} ->
                [ ((instanceLabel tr name, letDefinitionMetadata), ordinary (getDeps letDefinitionExpression))
                ]
              _ ->
                []
         in
          concatMap extractInstanceDependency instanceDefinitionImplementations
      _ ->
        []

  -- Tag a list of referenced names as ordinary calls.
  ordinary = fmap (\dep -> (dep, Ordinary))

  -- Tag a list of referenced names as structural recursion calls.
  structural = fmap (\dep -> (dep, Structural))

  getDeps :: Expression a k t -> [Name]
  getDeps expr =
    let freeVars :: Set.Set (Coal.Common.Label.Label t)
        freeVars = Set.filter notConstructor (freeIn expr)
     in nub (labelName <$> Set.toList freeVars)

{- | Topologically sort definitions by dependencies.

Uses strongly connected components to detect cycles, computed over /all/
edges (ordinary and structural alike). A strongly connected component is
reported as a problematic cycle if and only if it contains an /ordinary/ call
edge between two of its own members:

- Any edge internal to an SCC lies on a cycle (its endpoints are mutually
  reachable), so an internal ordinary edge witnesses a cycle with unbounded
  recursion, whether the remaining edges of the cycle are ordinary or
  structural.

- Conversely, any cycle lies entirely within a single SCC, so a mixed cycle
  always manifests as an internal ordinary edge of some SCC.

- An SCC all of whose internal edges are structural recursion calls is a
  valid fold recursion scheme (bounded by destructuring) and is accepted.

If any problematic cycles exist, returns Left with the list of cycles.
Otherwise returns Right with a valid topological ordering.
-}
topoSortDefs :: [((Name, a), [(Name, EdgeKind)])] -> Either [[(Name, a)]] [(Name, a)]
topoSortDefs defs =
  if null problematicCycles
    then Right (concatMap flatten sccs)
    else Left problematicCycles
 where
  edges = [((name, loc), name, nub (fst <$> deps)) | ((name, loc), deps) <- defs]
  sccs = stronglyConnComp edges
  -- Ordinary call edges: these are the only edges that can make a cycle
  -- unbounded.
  ordinaryEdges = [(name, dep) | ((name, _), deps) <- defs, (dep, Ordinary) <- deps]
  -- An SCC is problematic iff it contains an internal ordinary edge.
  hasOrdinaryCycle scc =
    let members = Set.fromList (fst <$> scc)
     in any (\(from, to) -> Set.member from members && Set.member to members) ordinaryEdges
  -- Report all cycles (self-recursion and mutual recursion) that contain an
  -- ordinary call edge.
  problematicCycles =
    [ xs
    | CyclicSCC xs <- sccs
    , hasOrdinaryCycle xs
    ]
  flatten (AcyclicSCC x) = [x]
  flatten (CyclicSCC xs) = xs
