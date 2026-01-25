{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.ProtoCompiler.ProtoBuild.ProtoPrep where

import Coal.Language
import Coal.Language.HasKind (foldKindOf)
import Coal.ProtoCompiler.ProtoBuild (ProtoBuild (..))
import qualified Coal.ProtoCompiler.ProtoBuild as Build
import Coal.ProtoCompiler.ProtoBuild.ProtoNameEntry (ProtoNameEntry (..))
import Coal.ProtoCompiler.ProtoStack (ProtoCompilerT (..))
import Coal.ProtoLanguage.ProtoDefinition
import Coal.ProtoLanguage.ProtoModule (ProtoModule (..))
import Control.Monad.State (StateT, modify)
import Extras (Name, traverse_)

insertNameEntry :: (Monad m) => Name -> ProtoNameEntry -> StateT ProtoBuild m ()
insertNameEntry name entry = modify (Build.insertBuildNameEntry name entry)

protoOprepareBuild :: (Monad m) => ProtoModule a Kind t -> ProtoCompilerT m ProtoBuild
protoOprepareBuild ProtoModule{..} = do
  undefined

protoOprepareDefinitions :: (Monad m) => [ProtoDefinition a Kind t] -> StateT ProtoBuild (ProtoCompilerT m) ()
protoOprepareDefinitions defs = do
  -- collect type constructors
  traverse_ collectTypeConstructors defs

  -- collect data constructors
  traverse_ collectDataConstructors defs

  -- collect traits
  traverse_ collectTraits defs

  -- collect instances
  traverse_ collectInstances defs

collectTypeConstructors :: (Monad m) => ProtoDefinition a Kind t -> StateT ProtoBuild (ProtoCompilerT m) ()
collectTypeConstructors =
  \case
    ProtoDType a name ProtoTypeDefinition{..} -> do
      insertNameEntry name (ProtoNType name kind)
     where
      kind = foldKindOf KType protoOtypeDefinitionParameters
    ProtoDImport loc path items ->
      undefined
    ProtoDQualifiedImport loc path ->
      undefined
    _ ->
      undefined

collectDataConstructors :: (Monad m) => ProtoDefinition a Kind t -> StateT ProtoBuild (ProtoCompilerT m) ()
collectDataConstructors =
  \case
    ProtoDType a name ProtoTypeDefinition{..} ->
      undefined
    ProtoDImport loc path items ->
      undefined
    ProtoDQualifiedImport loc path ->
      undefined
    _ ->
      undefined

collectTraits :: (Monad m) => ProtoDefinition a Kind t -> StateT ProtoBuild (ProtoCompilerT m) ()
collectTraits =
  \case
    ProtoDTrait loc name ProtoTraitDefinition{..} ->
      undefined
    ProtoDImport loc path items ->
      undefined
    ProtoDQualifiedImport loc path ->
      undefined
    _ ->
      undefined

collectInstances :: (Monad m) => ProtoDefinition a Kind t -> StateT ProtoBuild (ProtoCompilerT m) ()
collectInstances =
  \case
    ProtoDInstance loc ProtoInstanceDefinition{..} ->
      undefined
    ProtoDImport loc path items ->
      undefined
    ProtoDQualifiedImport loc path ->
      undefined
    _ ->
      undefined
