{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.PreflightPhase.TopologicalSort (passTopologicalSort) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Embedded (embeddedPaths)
import Coal.Compiler.Error (CompilerError (..), ErrorLocation (..))
import Coal.Compiler.Journal (tellErrors)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (CompilerFailureMode (..), CompilerT)
import Coal.Language (Kind)
import Coal.Language.Module
import Control.Monad.Except (MonadError (throwError))
import Data.Graph (SCC (..), stronglyConnComp)
import Data.Set (Set)
import qualified Data.Set as Set
import Extras (Name, concatForM, forM_)

passTopologicalSort :: (Monad m) => Pass Metadata m [Module Metadata Kind ()] [Module Metadata Kind ()]
passTopologicalSort = Pass{runPass = pass}

pass :: (Monad m) => [Module Metadata Kind ()] -> CompilerT Metadata m [Module Metadata Kind ()]
pass modules = do
  edges <- traverse (collectEdges names) modules
  let sccs = stronglyConnComp edges
      cyclicSCCs = filter isCyclicSCC sccs
  forM_ cyclicSCCs $
    \scc ->
      tellErrors [ModuleCycle (modulePathName <$> getModulesFromSCC scc)]
  if not (null cyclicSCCs)
    then throwError PreflightFailure
    else pure $ concatMap getModulesFromSCC sccs
 where
  names = Set.fromList (modulePathName <$> modules)

isCyclicSCC :: SCC (Module Metadata Kind ()) -> Bool
isCyclicSCC =
  \case
    CyclicSCC _ -> True
    _ -> False

getModulesFromSCC :: SCC (Module Metadata Kind ()) -> [Module Metadata Kind ()]
getModulesFromSCC =
  \case
    AcyclicSCC m -> [m]
    CyclicSCC ms -> ms

collectEdges :: (Monad m) => Set Name -> Module Metadata Kind () -> CompilerT Metadata m (Module Metadata Kind (), Name, [Name])
collectEdges names m = do
  deps' <- concatForM (dependencies m) $
    \(loc, dep) ->
      if Set.member dep names
        then pure [dep]
        else do
          tellErrors [ModuleNotFound dep (ErrorLocation (modulePathName m) loc)]
          pure []
  pure (m, modulePathName m, deps')

dependencies :: Module Metadata Kind () -> [(Metadata, Name)]
dependencies (Module p _ defs)
  | principalPath p `elem` embeddedPaths = imported
  | otherwise = imported <> extra
 where
  imported = concatMap importPath defs
  extra =
    [ (mempty, "Coal.Applicative")
    , (mempty, "Coal.Monad")
    ]

importPath :: Definition Metadata Kind () -> [(Metadata, Name)]
importPath =
  \case
    DImport loc p _ ->
      [(loc, principalPath p)]
    DQualifiedImport loc p ->
      [(loc, principalPath p)]
    _ ->
      []
