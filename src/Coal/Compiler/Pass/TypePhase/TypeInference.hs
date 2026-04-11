{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Pass.TypePhase.TypeInference (passTypeInference) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Common.Environment (Environment (..), mapEnvironment)
import Coal.Compiler.Builtin.Definitions (builtinFunctions)
import Coal.Compiler.Journal (tellErrors)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.ProtoBuild (ProtoBuild (..), protoObuildNames)
import Coal.Compiler.ProtoBuild.ProtoPrep
import Coal.Compiler.ProtoState
import Coal.Compiler.Stack
import Coal.Compiler.TypeInference (generateKindConstraints, protoOdefine, protoOgenerateConstraints, solveX)
import Coal.Graphviz.ProtoDot
import Coal.Language (HasType (..), IndexedType, Kind, Trait (..), TypeIndex, indexed, instanceLabel, normalizeRowTypes, typeOf)
import Coal.Language.Module.Path
import Coal.Language.Type (Type (..))
import Coal.Language.Type.Kind.Indexed (ToKindIndexed (..))
import Coal.ProtoLanguage.ProtoDefinition
import Coal.ProtoLanguage.ProtoModule
import Coal.ProtoTypeSystem.Kind.Constraint.Solver (protoOsolveKindConstraints)
import Coal.ProtoTypeSystem.Kind.Substitution
import Coal.ProtoTypeSystem.Kind.Unification
import Coal.TypeSystem.Constraint
import Coal.TypeSystem.Constraint.Assumption (Assumption (..), normalizedName)
import Coal.TypeSystem.Constraint.Generation.InferenceRule (InferenceRule (..))
import Coal.TypeSystem.Substitution (apply, normalizeTypeIndexes)
import Control.Monad.Except
import Control.Monad.State (gets, modify, runState)
import Data.Data (Data)
import Data.List (nub)
import qualified Data.Map as Map
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Text.Lazy (toStrict)
import Debug.Trace
import Text.Pretty.Simple (pPrint, pShowNoColor)
import TextShow (showt)

passTypeInference :: (MonadIO m, Monoid a, Data a, Eq a, Show a) => Pass a m (ProtoModule a Kind ()) (ProtoModule a Kind IndexedType)
passTypeInference = Pass{runPass = pass}

pass :: (MonadIO m, Monoid a, Data a, Eq a, Show a) => ProtoModule a Kind () -> CompilerT a m (ProtoModule a Kind IndexedType)
pass m@(ProtoModule path _ _) = do
  --  env <- buildEnv
  --  setNamesC env
  --  insertNamesC builtinFunctions

  runTypeInference m

--  next <- runTypeInference m
--
--  names <- gets compilerNameStore
--  replacePlaceholders names
--
----  assumptions <- lift $ gets (filter (not . isFoldAssumption) . nub . protoOcompilerAssumptions)
----  forM_ assumptions $
----    \Assumption{..} -> do
----      tellErrors [NameNotInScope assumptionName (ErrorLocation (principalPath path) assumptionMetadata)]
----  unless (null assumptions) $
----    throwError NoSuchIdentifier
--
--  pure next

isFoldAssumption :: Assumption a t -> Bool
isFoldAssumption Assumption{..} = "!" `Text.isPrefixOf` assumptionName

indexTypes :: (Monad m, Traversable t) => t e -> CompilerT a m (t IndexedType)
indexTypes ds = undefined -- run (indexed ds) =<< gets compilerSupply
-- where
--  run s m = do
--    let (r, n) = runState s m
--    insertSupplyC n
--    pure r

runTypeInference :: (MonadIO m, Monoid a, Data a, Eq a, Show a) => ProtoModule a Kind () -> CompilerT a m (ProtoModule a Kind IndexedType)
runTypeInference m = do
  --  defs <- traverse indexTypes ds
  --  (tdefs, _) <- typeDefinitionsC defs

  nm <- ti m -- builtinTraits m)
  liftIO $ Text.writeFile ("tmp/defs_" <> Text.unpack (principalPath (protoOmodulePath m))) (generateDotSyntax nm)
  --  liftIO $ Text.writeFile ("tmp/olddefs_" <> Text.unpack (principalPath (modulePath m))) (generateDot (Module p ns (normalizeTypeIndexes tdefs)))
  ProtoBuild{..} <- protoOgetCurrentBuildC
  liftIO $ Text.writeFile ("tmp/names_" <> Text.unpack (principalPath (protoOmodulePath m))) (toStrict $ pShowNoColor $ protoObuildNames)

  liftIO $ Text.writeFile ("tmp/build_" <> Text.unpack (principalPath (protoOmodulePath m))) (toStrict $ pShowNoColor $ ProtoBuild{..})

  --  stor <- gets compilerNameStore
  --  liftIO $ Text.writeFile ("tmp/oldnames_" <> Text.unpack (principalPath (modulePath m))) (toStrict $ pShowNoColor $ stor)

  --  traceShowM (modulePath m)

  --  pure (Module p ns (normalizeTypeIndexes tdefs))

  --  traceShowM (definitionName <$> tdefs)
  --  traceShowM (definitionName <$> (moduleDefinitions $ fromProtoModule nm))

  --  when (protoOmodulePath m == Path ["Main"]) $ do
  --    pPrint nm

  pure nm

-- where
--  Module p ns ds = m

ti :: (MonadIO m, Data a, Monoid a, Show a, Eq a) => ProtoModule a Kind () -> CompilerT a m (ProtoModule a Kind IndexedType)
ti modul = do
  indexed <- inferKinds modul
  newModule <- inferTypes indexed
  protoOreplacePlaceholders
  return newModule

inferTypes :: (MonadIO m, Data a, Show a, Eq a) => ProtoModule a Kind () -> CompilerT a m (ProtoModule a Kind IndexedType)
inferTypes modul = do
  ProtoModule{..} <- protoOindexTypes modul
  forM_ protoOmoduleDefinitions $
    \def -> do
      protoOgenerateConstraints def
      sub <- solveX
      defineName (apply sub def)

  sub <- gets protoOcompilerSubstitution
  assumptions <- gets protoOcompilerAssumptions

  Environment env <- gets protoOcompilerNameStore
  protoOinsertConstraintsC $ do
    (n, s) <- Map.toList env
    Assumption{..} <- assumptions
    let t = apply sub assumptionType
    [Explicit (RuleAssumptionExplicit assumptionMetadata t s) t s | n == normalizedName assumptionName]

  sub <- solveX
  modify (overCompilerAssumptions (apply sub))

  let newModuleDefinitions = fmap (fmap normalizeRowTypes) (apply sub protoOmoduleDefinitions)
  pure $
    ProtoModule
      { protoOmoduleDefinitions = normalizeTypeIndexes newModuleDefinitions
      , ..
      }

inferKinds :: (MonadIO m, Show a) => ProtoModule a Kind () -> CompilerT a m (ProtoModule a Kind ())
inferKinds indexed = do
  generateKindConstraints indexed
  constraints <- gets protoOcompilerKindConstraints
  case protoOkindUnifierMonad (protoOsolveKindConstraints constraints) of
    Left err ->
      error (show err)
    Right sub -> do
      modify (overCompilerNameStore (protoOreplaceVariables . protoOapplyKinds sub))
      modify (overCompilerModuleWithPath (protoOmodulePath indexed) (protoOreplaceVariables . protoOapplyKinds sub))
      return (protoOreplaceVariables (protoOapplyKinds sub indexed))

defineName :: (Monad m, Data a) => ProtoDefinition a Kind IndexedType -> CompilerT a m ()
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

protoOindexTypes :: (Monad m, Traversable t) => t e -> CompilerT a m (t IndexedType)
protoOindexTypes ds = run (indexed ds) =<< gets protoOcompilerSupply
 where
  run s m = do
    let (r, n) = runState s m
    protoOupdateSupplyC n
    pure r
