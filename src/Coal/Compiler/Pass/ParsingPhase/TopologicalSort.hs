{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

module Coal.Compiler.Pass.ParsingPhase.TopologicalSort (passTopologicalSort) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Build (moduleDependencies, modulePath)
import Coal.Compiler.Build.Core (dependencies)
import Coal.Compiler.Error (CompilerError (..), ErrorLocation (..))
import Coal.Compiler.Journal (tellErrors)
import Coal.Compiler.Pass (BuildUnit (..), Pass (..))
import Coal.Compiler.Stack (CompilerFailureMode (..), CompilerT)
import Coal.Language (Kind)
import Coal.Language.Module
import Control.Monad.Except (MonadError (throwError))
import Data.Graph (SCC (..), stronglyConnComp)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple.Extra (second)
import Extras (Name, concatForM, for, forM_)

passTopologicalSort :: (Monad m) => Pass Metadata m [BuildUnit (Module Metadata Kind ())] [BuildUnit (Module Metadata Kind ())]
passTopologicalSort = Pass{runPass = pass}

pass :: (Monad m) => [BuildUnit (Module Metadata Kind ())] -> CompilerT Metadata m [BuildUnit (Module Metadata Kind ())]
pass units = do
  edges <- traverse (collectEdges names) units
  let sccs = stronglyConnComp edges
      cyclicSCCs = filter isCyclicSCC sccs
  forM_ cyclicSCCs $
    \scc ->
      tellErrors [ModuleCycle (unitPathName <$> getModulesFromSCC scc)]
  if not (null cyclicSCCs)
    then throwError PreflightFailure
    else pure $ concatMap getModulesFromSCC sccs
 where
  names = Set.fromList (unitPathName <$> units)

unitPathName :: BuildUnit (Module Metadata Kind ()) -> Name
unitPathName =
  \case
    BSource m ->
      modulePathName m
    BCached b ->
      principalPath (modulePath b)

isCyclicSCC :: SCC (BuildUnit (Module Metadata Kind ())) -> Bool
isCyclicSCC =
  \case
    CyclicSCC _ -> True
    _ -> False

getModulesFromSCC :: SCC (BuildUnit (Module Metadata Kind ())) -> [BuildUnit (Module Metadata Kind ())]
getModulesFromSCC =
  \case
    AcyclicSCC u -> [u]
    CyclicSCC units -> units

unitDependencies :: BuildUnit (Module Metadata Kind ()) -> [(Metadata, Path)]
unitDependencies =
  \case
    BSource m ->
      dependencies m
    BCached b ->
      (mempty,) <$> moduleDependencies b

collectEdges :: (Monad m) => Set Name -> BuildUnit (Module Metadata Kind ()) -> CompilerT Metadata m (BuildUnit (Module Metadata Kind ()), Name, [Name])
collectEdges names unit = do
  unitDependencies' <- concatForM deps $
    \(loc, dep) ->
      if Set.member dep names
        then pure [dep]
        else do
          tellErrors [ModuleNotFound dep (ErrorLocation path loc)]
          pure []
  pure (unit, path, unitDependencies')
 where
  deps = for (unitDependencies unit) (second principalPath)
  path = unitPathName unit
