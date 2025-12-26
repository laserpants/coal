{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

module Coal.Compiler.Pass.ParsingPhase.TopologicalSort (passTopologicalSort) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Build (moduleDependencies)
import Coal.Compiler.Build.Core (dependencies)
import Coal.Compiler.Error (CompilerError (..), ErrorLocation (..))
import Coal.Compiler.Journal (tellErrors)
import Coal.Compiler.Pass (BuildUnit (..), Pass (..), unitPathName)
import Coal.Compiler.Stack (CompilerFailureMode (..), CompilerT)
import Coal.Language (Kind)
import Coal.Language.Module
import Control.Monad (unless)
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
  unless ("Main" `elem` names) $ do
    tellErrors [NoModuleMain]
    throwError PreflightFailure

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
