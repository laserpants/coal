{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.ProtoCompiler.KindEnvironment (moduleKindEnvironment) where

import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Language.HasKind (HasKind (..), foldKindOf)
import Coal.Language.Module.Import (Import (..))
import Coal.Language.Module.Path (Path (..))
import Coal.Language.Type.Kind (Kind (..))
import Coal.ProtoCompiler.ProtoBuild
import Coal.ProtoCompiler.ProtoBuild.ProtoNameEntry
import Coal.ProtoCompiler.ProtoStack (ProtoCompilerT)
import Coal.ProtoCompiler.ProtoState
import Coal.ProtoLanguage.ProtoDefinition
import Coal.ProtoLanguage.ProtoModule
import Control.Applicative ((<|>))
import Control.Monad.State (get)
import Data.Maybe (fromMaybe)
import Extras (Name, concatForM, forM)

moduleKindEnvironment :: (Monad m) => ProtoModule a Kind () -> ProtoCompilerT m a (Environment Kind)
moduleKindEnvironment ProtoModule{..} = do
  res <- forM protoOmoduleDefinitions $
    \case
      ProtoDTrait _ name ProtoTraitDefinition{..} ->
        pure
          [
            ( name
            , KArrow (kindOf protoOtraitDefinitionParameter) KTrait
            )
          ]
      ProtoDType _ name ProtoTypeDefinition{..} ->
        pure
          [
            ( name
            , foldKindOf KType protoOtypeDefinitionParameters
            )
          ]
      ProtoDTypeAlias _ name ProtoAliasDefinition{..} ->
        pure
          [
            ( name
            , kindOf protoOaliasDefinitionType
            )
          ]
      ProtoDImport _ path items -> do
        importedModule <- importedBuild path
        concatForM items $
          \case
            TypeImport _ name _ | importedModule `exports` name -> do
              pure $
                fromMaybe [] $ do
                  kind <-
                    typeConstructorKind name importedModule
                      <|> traitKind name importedModule
                      <|> aliasKind name importedModule
                  Just [(name, kind)]
            _ ->
              pure []
      ProtoDQualifiedImport _ path -> do
        ProtoBuild{protoObuildExportedNames = exportedNames, ..} <- importedBuild path
        pure
          []
      _ ->
        pure []

  pure (Environment.fromList (concat res))

exports :: ProtoBuild a -> Name -> Bool
exports ProtoBuild{..} name = name `elem` protoObuildExportedNames

typeConstructorKind :: Name -> ProtoBuild a -> Maybe Kind
typeConstructorKind name ProtoBuild{..} =
  case Environment.lookup name protoObuildTypeConstructors of
    Nothing ->
      Nothing
    Just ProtoTypeConstructorEntry{..} ->
      Just protoOtypeConstructorEntryKind

traitKind :: Name -> ProtoBuild a -> Maybe Kind
traitKind name ProtoBuild{..} =
  case Environment.lookup name protoObuildTraits of
    Nothing ->
      Nothing
    Just ProtoTraitEntry{..} ->
      Just (KArrow (kindOf protoOtraitEntryParameter) KTrait)

aliasKind :: Name -> ProtoBuild a -> Maybe Kind
aliasKind name ProtoBuild{..} =
  case Environment.lookup name protoObuildAliases of
    Nothing ->
      Nothing
    Just ProtoAliasEntry{..} ->
      Just (kindOf protoOaliasEntryType)

importedBuild :: (Monad m) => Path -> ProtoCompilerT m a (ProtoBuild a)
importedBuild path = do
  ProtoCompilerState{..} <- get

  undefined

-- exportedTypes
