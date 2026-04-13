{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

module Coal.Compiler.Pass.PreflightPhase.SortModules (
  passSortModules,
) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Build
import Coal.Compiler.Build.Envelope (BuildEnvelope (..), envelopePathName)
import Coal.Compiler.Embedded (embeddedPaths)
import Coal.Compiler.Error (CompilerError (..), ErrorLocation (..))
import Coal.Compiler.Journal (tellErrors)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (CompilerFailureMode (..), CompilerT)
import Coal.Language.Definition
import Coal.Language.Module (Module (..))
import Coal.Language.Module.Path
import Control.Monad (unless)
import Control.Monad.Except (MonadError (throwError))
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List.Extra (notNull)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple.Extra (second)
import Extras (Name, concatForM, for, forM_)

passSortModules :: (Monad m) => Pass Metadata m [BuildEnvelope (Module Metadata () ())] [BuildEnvelope (Module Metadata () ())]
passSortModules = Pass{runPass = passImpl}

passImpl :: (Monad m) => [BuildEnvelope (Module Metadata () ())] -> CompilerT Metadata m [BuildEnvelope (Module Metadata () ())]
passImpl units = do
  -- TODO: move ?
  unless ("Main" `elem` names) $ do
    tellErrors [NoModuleMain]
    throwError PreflightFailure
  edges <- traverse (collectEdges names) units
  let sccs = stronglyConnComp edges
      cyclicSCCs = filter isCyclicSCC sccs
  forM_ cyclicSCCs $
    \scc ->
      tellErrors [ModuleCycle (envelopePathName <$> getModulesFromSCC scc)]
  if notNull cyclicSCCs
    then throwError PreflightFailure
    else pure $ concatMap getModulesFromSCC sccs
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
  | principalPath p `elem` embeddedPaths = imported
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
  unitDependencies' <-
    concatForM deps $
      \(loc, dep) ->
        if Set.member dep names
          then pure [dep]
          else do
            tellErrors [ModuleNotFound dep (ErrorLocation path loc)]
            pure []
  pure (unit, path, unitDependencies')
 where
  deps = for (unitDependencies unit) (second principalPath)
  path = envelopePathName unit
