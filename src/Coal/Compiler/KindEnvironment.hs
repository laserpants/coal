{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.KindEnvironment (moduleKindEnvironment) where

import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Compiler.ProtoBuild
import Coal.Compiler.ProtoBuild.ProtoNameEntry
import Coal.Compiler.ProtoState
import Coal.Compiler.Stack
import Coal.Language.HasKind (HasKind (..), foldKindOf)
import Coal.Language.Module.Import (Import (..))
import Coal.Language.Module.Path (Path (..), principalPath)
import Coal.Language.Type.Kind (Kind (..))
import Coal.ProtoLanguage.ProtoDefinition
import Coal.ProtoLanguage.ProtoModule
import Control.Applicative ((<|>))
import Control.Monad.State (get)
import Data.Maybe (fromMaybe)
import Extras (Name, concatForM, forM, (<.>))

moduleKindEnvironment :: (Monad m) => ProtoModule a Kind () -> CompilerT b m (Environment Kind)
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
      -- TODO: temp
      ProtoDImport _ (Path ["Builtin$"]) items -> do
        pure []
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
      ProtoDNamespaceImport _ path -> do
        let qualified name = principalPath path <.> name
        importedModule@ProtoBuild{..} <- importedBuild path
        ps1 <- concatForM (Environment.names protoObuildTypeConstructors) $
          \name ->
            pure $
              if importedModule `exports` name
                then nameKindPairs (qualified name) (typeConstructorKind name importedModule)
                else []
        ps2 <- concatForM (Environment.names protoObuildTraits) $
          \name ->
            pure $
              if importedModule `exports` name
                then nameKindPairs (qualified name) (traitKind name importedModule)
                else []
        ps3 <- concatForM (Environment.names protoObuildAliases) $
          \name ->
            pure $
              if importedModule `exports` name
                then nameKindPairs (qualified name) (aliasKind name importedModule)
                else []
        pure (ps1 <> ps2 <> ps3)
      _ ->
        pure []
  pure $
    Environment.fromList (concat res)

nameKindPairs :: Name -> Maybe Kind -> [(Name, Kind)]
nameKindPairs name maybeKind =
  fromMaybe [] $ do
    kind <- maybeKind
    pure [(name, kind)]

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

importedBuild :: (Monad m) => Path -> CompilerT a m (ProtoBuild a)
importedBuild path = do
  CompilerState{..} <- get
  case Environment.lookup (principalPath path) protoOcompilerModules of
    Nothing ->
      -- TODO
      error ("No module: " <> show path)
    Just build ->
      pure build
