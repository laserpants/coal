{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TupleSections #-}

module Coal.Compiler.Pass.ParsingPhase.TopologicalSort (passTopologicalSort) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Build.Envelope (BuildEnvelope (..), envelopePathName)
import Coal.Compiler.Embedded (embeddedPaths)
import Coal.Compiler.Error (CompilerError (..), ErrorLocation (..))
import Coal.Compiler.Journal (tellErrors)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (CompilerFailureMode (..), CompilerT)
import Coal.Language.Module.Path
import Coal.ProtoCompiler.ProtoBuild
import Coal.ProtoLanguage.ProtoDefinition
import Coal.ProtoLanguage.ProtoModule (ProtoModule (..))
import Control.Monad (unless)
import Control.Monad.Except (MonadError (throwError))
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List.Extra (notNull)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Tuple.Extra (second)
import Extras (Name, concatForM, for, forM_)

passTopologicalSort :: (Monad m) => Pass Metadata m [BuildEnvelope (ProtoModule Metadata () ())] [BuildEnvelope (ProtoModule Metadata () ())]
passTopologicalSort = Pass{runPass = pass}

pass :: (Monad m) => [BuildEnvelope (ProtoModule Metadata () ())] -> CompilerT Metadata m [BuildEnvelope (ProtoModule Metadata () ())]
pass units = do
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

isCyclicSCC :: SCC (BuildEnvelope (ProtoModule Metadata () ())) -> Bool
isCyclicSCC =
  \case
    CyclicSCC _ ->
      True
    _ ->
      False

getModulesFromSCC :: SCC (BuildEnvelope (ProtoModule Metadata () ())) -> [BuildEnvelope (ProtoModule Metadata () ())]
getModulesFromSCC =
  \case
    AcyclicSCC u ->
      [u]
    CyclicSCC units ->
      units

unitDependencies :: BuildEnvelope (ProtoModule Metadata () ()) -> [(Metadata, Path)]
unitDependencies =
  \case
    BSource m ->
      dependencies m
    BCached b ->
      (mempty,) <$> protoObuildDependencies b

dependencies :: (Monoid a) => ProtoModule a k t -> [(a, Path)]
dependencies (ProtoModule p _ defs)
  | principalPath p `elem` embeddedPaths = imported
  | otherwise = imported <> extra
 where
  imported = mapMaybe importPath defs
  extra =
    [ (mempty, Path ["Coal", "Applicative"])
    , (mempty, Path ["Coal", "Monad"])
    ]

-- TODO: move
importPath :: ProtoDefinition a k t -> Maybe (a, Path)
importPath =
  \case
    ProtoDImport loc p _ ->
      Just (loc, p)
    ProtoDNamespaceImport loc p ->
      Just (loc, p)
    _ ->
      Nothing

collectEdges :: (Monad m) => Set Name -> BuildEnvelope (ProtoModule Metadata () ()) -> CompilerT Metadata m (BuildEnvelope (ProtoModule Metadata () ()), Name, [Name])
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
