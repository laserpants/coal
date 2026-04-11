{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.KindEnvironment (moduleKindEnvironment) where

import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Build
import Coal.Compiler.Build.NameEntry
import Coal.Compiler.Stack
import Coal.Compiler.State
import Coal.Language.Definition
import Coal.Language.HasKind (HasKind (..), foldKindOf)
import Coal.Language.Module
import Coal.Language.Module.Import (Import (..))
import Coal.Language.Module.Path (Path (..), principalPath)
import Coal.Language.Type.Kind (Kind (..))
import Control.Applicative ((<|>))
import Control.Monad.State (get)
import Data.Maybe (fromMaybe)
import Extras (Name, concatForM, forM, (<.>))

moduleKindEnvironment :: (Monad m) => Module a Kind () -> CompilerT b m (Environment Kind)
moduleKindEnvironment Module{..} = do
  res <- forM protoOmoduleDefinitions $
    \case
      DTrait _ name TraitDefinition{..} ->
        pure
          [
            ( name
            , KArrow (kindOf protoOtraitDefinitionParameter) KTrait
            )
          ]
      DType _ name TypeDefinition{..} ->
        pure
          [
            ( name
            , foldKindOf KType protoOtypeDefinitionParameters
            )
          ]
      DTypeAlias _ name AliasDefinition{..} ->
        pure
          [
            ( name
            , kindOf protoOaliasDefinitionType
            )
          ]
      -- TODO: temp
      DImport _ (Path ["Builtin$"]) items -> do
        pure []
      DImport _ path items -> do
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
      DNamespaceImport _ path -> do
        let qualified name = principalPath path <.> name
        importedModule@Build{..} <- importedBuild path
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

exports :: Build a -> Name -> Bool
exports Build{..} name = name `elem` protoObuildExportedNames

typeConstructorKind :: Name -> Build a -> Maybe Kind
typeConstructorKind name Build{..} =
  case Environment.lookup name protoObuildTypeConstructors of
    Nothing ->
      Nothing
    Just TypeConstructorEntry{..} ->
      Just protoOtypeConstructorEntryKind

traitKind :: Name -> Build a -> Maybe Kind
traitKind name Build{..} =
  case Environment.lookup name protoObuildTraits of
    Nothing ->
      Nothing
    Just TraitEntry{..} ->
      Just (KArrow (kindOf protoOtraitEntryParameter) KTrait)

aliasKind :: Name -> Build a -> Maybe Kind
aliasKind name Build{..} =
  case Environment.lookup name protoObuildAliases of
    Nothing ->
      Nothing
    Just AliasEntry{..} ->
      Just (kindOf protoOaliasEntryType)

importedBuild :: (Monad m) => Path -> CompilerT a m (Build a)
importedBuild path = do
  CompilerState{..} <- get
  case Environment.lookup (principalPath path) protoOcompilerModules of
    Nothing ->
      -- TODO
      error ("No module: " <> show path)
    Just build ->
      pure build
