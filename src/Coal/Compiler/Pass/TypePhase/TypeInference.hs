{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Pass.TypePhase.TypeInference (passTypeInference) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Common.Environment (mapEnvironment)
import Coal.Compiler.Build.Core (buildEnv, replacePlaceholders)
import Coal.Compiler.Builtin.Definitions (builtinFunctions)
import Coal.Compiler.Builtin.Traits (builtinTraits)
import Coal.Compiler.Journal (tellErrors)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Compiler.TypeInference (generateKindConstraints, protoOdefine, protoOgenerateConstraints, solveX, typeDefinitionsC)
import Coal.Graphviz.Dot (generateDot)
import Coal.Graphviz.ProtoDot
import Coal.Language (HasType (..), IndexedType, Kind, Trait (..), TypeIndex, indexed, instanceLabel, normalizeRowTypes, typeOf)
import Coal.Language.Module (Module (..), fromProtoModule, principalPath, toProtoModule)
import Coal.Language.Module.Definition (definitionName)
import Coal.Language.Type (Type (..))
import Coal.Language.Type.Kind.Indexed (ToKindIndexed (..))
import Coal.ProtoCompiler.ProtoBuild (ProtoBuild (..), protoObuildNames)
import Coal.ProtoCompiler.ProtoBuild.ProtoPrep
import Coal.ProtoCompiler.ProtoStack (ProtoCompilerT, protoOclearAssumptionsC, protoOclearNameStoreC, protoOgetCurrentBuildC, protoOinsertNameC, protoOupdateSupplyC, setCurrentModuleC)
import Coal.ProtoCompiler.ProtoState
import Coal.ProtoLanguage.ProtoDefinition
import Coal.ProtoLanguage.ProtoModule
import Coal.ProtoTypeSystem.Kind.Constraint.Solver (protoOsolveKindConstraints)
import Coal.ProtoTypeSystem.Kind.Substitution
import Coal.ProtoTypeSystem.Kind.Unification
import Coal.TypeSystem.Constraint.Assumption (Assumption (..))
import Coal.TypeSystem.Substitution (apply, normalizeTypeIndexes)
import Control.Monad.Except
import Control.Monad.State (gets, modify, runState)
import Data.Data (Data)
import Data.List (nub)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Text.Lazy (toStrict)
import Debug.Trace
import Text.Pretty.Simple (pPrint, pShowNoColor)
import TextShow (showt)

passTypeInference :: (MonadIO m, Monoid a, Data a, Eq a, Show a) => Pass a m (Module a Kind ()) (Module a Kind IndexedType)
passTypeInference = Pass{runPass = pass}

pass :: (MonadIO m, Monoid a, Data a, Eq a, Show a) => Module a Kind () -> CompilerT a (ProtoCompilerT m a) (Module a Kind IndexedType)
pass m@(Module path _ _) = do
  env <- buildEnv
  setNamesC env
  insertNamesC builtinFunctions

  next <- runTypeInference m
  names <- gets compilerNameStore
  replacePlaceholders names

  assumptions <- gets (filter (not . isFoldAssumption) . nub . compilerAssumptions)
  forM_ assumptions $
    \Assumption{..} -> do
      tellErrors [NameNotInScope assumptionName (ErrorLocation (principalPath path) assumptionMetadata)]
  unless (null assumptions) $
    throwError NoSuchIdentifier
  pure next

isFoldAssumption :: Assumption a t -> Bool
isFoldAssumption Assumption{..} = "!" `Text.isPrefixOf` assumptionName

indexTypes :: (Monad m, Traversable t) => t e -> CompilerT a m (t IndexedType)
indexTypes ds = run (indexed ds) =<< gets compilerSupply
 where
  run s m = do
    let (r, n) = runState s m
    insertSupplyC n
    pure r

runTypeInference :: (MonadIO m, Monoid a, Data a, Eq a, Show a) => Module a Kind () -> CompilerT a (ProtoCompilerT m a) (Module a Kind IndexedType)
runTypeInference m = do
  defs <- traverse indexTypes ds
  (tdefs, _) <- typeDefinitionsC defs

  nm <- lift $ ti (toProtoModule builtinTraits m)

  liftIO $ Text.writeFile ("tmp/defs_" <> Text.unpack (principalPath (modulePath m))) (generateDotSyntax nm)
  liftIO $ Text.writeFile ("tmp/olddefs_" <> Text.unpack (principalPath (modulePath m))) (generateDot (Module p ns (normalizeTypeIndexes tdefs)))
  ProtoBuild{..} <- lift $ protoOgetCurrentBuildC
  liftIO $ Text.writeFile ("tmp/names_" <> Text.unpack (principalPath (modulePath m))) (toStrict $ pShowNoColor $ protoObuildNames)
  stor <- gets compilerNameStore
  liftIO $ Text.writeFile ("tmp/oldnames_" <> Text.unpack (principalPath (modulePath m))) (toStrict $ pShowNoColor $ stor)

  --  traceShowM (modulePath m)

  --  pure (Module p ns (normalizeTypeIndexes tdefs))

  --  traceShowM (definitionName <$> tdefs)
  --  traceShowM (definitionName <$> (moduleDefinitions $ fromProtoModule nm))

  pure (fromProtoModule nm)
 where
  Module p ns ds = m

ti :: (MonadIO m, Data a, Show a, Eq a) => ProtoModule a () () -> ProtoCompilerT m a (ProtoModule a Kind IndexedType)
ti modul = do
  protoOclearAssumptionsC
  protoOclearNameStoreC
  setCurrentModuleC modul

  forM_ builtinFunctions $ uncurry protoOinsertNameC

  indexed <- inferKinds modul
  protoOprepareBuild indexed
  newModule <- inferTypes indexed
  protoOreplacePlaceholders
  return newModule

inferTypes :: (MonadIO m, Data a, Show a, Eq a) => ProtoModule a Kind () -> ProtoCompilerT m a (ProtoModule a Kind IndexedType)
inferTypes modul = do
  ProtoModule{..} <- protoOindexTypes modul
  forM_ protoOmoduleDefinitions $
    \def -> do
      protoOgenerateConstraints def
      sub <- solveX
      defineName (apply sub def)
  sub <- gets protoOcompilerSubstitution
  modify (overProtoCompilerAssumptions (apply sub))
  let newModuleDefinitions = fmap (fmap normalizeRowTypes) (apply sub protoOmoduleDefinitions)
  pure $
    ProtoModule
      { protoOmoduleDefinitions = normalizeTypeIndexes newModuleDefinitions
      , ..
      }

inferKinds :: (MonadIO m, Show a) => ProtoModule a () () -> ProtoCompilerT m a (ProtoModule a Kind ())
inferKinds modul = do
  indexed <- toKindIndexed modul
  generateKindConstraints indexed
  constraints <- gets protoOcompilerKindConstraints
  case protoOkindUnifierMonad (protoOsolveKindConstraints constraints) of
    Left err ->
      error (show err)
    Right sub ->
      return (protoOreplaceVariables (protoOapplyKinds sub indexed))

defineName :: (Monad m, Data a) => ProtoDefinition a Kind IndexedType -> ProtoCompilerT m a ()
defineName =
  \case
    def@(ProtoDFunction _ name ProtoFunctionDefinition{..}) ->
      protoOdefine name (typeOf def)
    def@(ProtoDLet _ name ProtoLetDefinition{..}) ->
      protoOdefine name (typeOf def)
    ProtoDInstance _ ProtoInstanceDefinition{..} -> do
      let trait = Trait protoOinstanceDefinitionTraitName protoOinstanceDefinitionType
      forM_ protoOinstanceDefinitionImplementations $
        \case
          def@(ProtoDFunction _ name _) ->
            protoOdefine (instanceLabel trait name) (typeOf def)
          def@(ProtoDLet _ name _) ->
            protoOdefine (instanceLabel trait name) (typeOf def)
    _ ->
      pure ()

protoOindexTypes :: (Monad m, Traversable t) => t e -> ProtoCompilerT m a (t IndexedType)
protoOindexTypes ds = run (indexed ds) =<< gets protoOcompilerSupply
 where
  run s m = do
    let (r, n) = runState s m
    protoOupdateSupplyC n
    pure r
