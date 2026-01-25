{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.ProtoCompiler.ProtoBuild.ProtoPrep where

import Coal.Language
import Coal.ProtoCompiler.ProtoBuild (ProtoBuild (..))
import Coal.ProtoCompiler.ProtoStack (ProtoCompilerT (..))
import Coal.ProtoLanguage.ProtoDefinition
import Coal.ProtoLanguage.ProtoModule (ProtoModule (..))
import Extras (traverse_)

protoOprepareBuild :: (Monad m) => ProtoModule a Kind t -> ProtoCompilerT m ProtoBuild
protoOprepareBuild ProtoModule{..} = do
  undefined

protoOprepareDefinitions :: (Monad m) => [ProtoDefinition a Kind t] -> ProtoCompilerT m ()
protoOprepareDefinitions defs = do
  -- collect type constructors
  traverse_ collectTypeConstructors defs

  -- collect data constructors
  traverse_ collectDataConstructors defs

  -- collect traits
  traverse_ collectTraits defs

  -- collect instances
  traverse_ collectInstances defs

collectTypeConstructors :: ProtoDefinition a Kind t -> ProtoCompilerT m ()
collectTypeConstructors =
  \case
    ProtoDType a name ProtoTypeDefinition{..} ->
      undefined
    ProtoDImport loc path items ->
      undefined
    ProtoDQualifiedImport loc path ->
      undefined
    _ ->
      undefined

collectDataConstructors :: ProtoDefinition a k t -> ProtoCompilerT m ()
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

collectTraits :: ProtoDefinition a k t -> ProtoCompilerT m ()
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

collectInstances :: ProtoDefinition a k t -> ProtoCompilerT m ()
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
