{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.KindEnvironment (moduleKindEnvironment) where

import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Build (Build (..))
import Coal.Compiler.Build.NameEntry
import Coal.Compiler.Error (CompilerError (..), ErrorLocation (..))
import Coal.Compiler.Journal (tellErrors)
import Coal.Compiler.Stack (CompilerFailureMode (..), CompilerT)
import Coal.Compiler.State
import Coal.Language.Definition
import Coal.Language.HasKind (HasKind (..), foldKindOf)
import Coal.Language.Module (Module (..))
import Coal.Language.Module.Import (Import (..))
import Coal.Language.Module.Path (Path (..), principalPath)
import Coal.Language.Type.Kind (Kind (..))
import Control.Applicative ((<|>))
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.State (get)
import Data.Maybe (fromMaybe)
import Extras (Name, concatForM, forM, (<.>))

moduleKindEnvironment :: (Monad m, Monoid b) => Module a Kind () -> CompilerT b m (Environment Kind)
moduleKindEnvironment Module{moduleDefinitions} = do
  res <- forM moduleDefinitions $
    \case
      DTrait _ name TraitDefinition{traitDefinitionParameter} ->
        pure
          [
            ( name
            , KArrow (kindOf traitDefinitionParameter) KTrait
            )
          ]
      DType _ name TypeDefinition{typeDefinitionParameters} ->
        pure
          [
            ( name
            , foldKindOf KType typeDefinitionParameters
            )
          ]
      DTypeAlias _ name AliasDefinition{aliasDefinitionType} ->
        pure
          [
            ( name
            , kindOf aliasDefinitionType
            )
          ]
      DImport _ (Path ["Builtin$"]) _ -> do
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
        importedModule@Build{buildTypeConstructors, buildTraits, buildAliases} <- importedBuild path
        ps1 <- concatForM (Environment.names buildTypeConstructors) $
          \name ->
            pure $
              if importedModule `exports` name
                then nameKindPairs (qualified name) (typeConstructorKind name importedModule)
                else []
        ps2 <- concatForM (Environment.names buildTraits) $
          \name ->
            pure $
              if importedModule `exports` name
                then nameKindPairs (qualified name) (traitKind name importedModule)
                else []
        ps3 <- concatForM (Environment.names buildAliases) $
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
exports Build{buildExportedNames} name = name `elem` buildExportedNames

typeConstructorKind :: Name -> Build a -> Maybe Kind
typeConstructorKind name Build{buildTypeConstructors} =
  case Environment.lookup name buildTypeConstructors of
    Nothing ->
      Nothing
    Just TypeConstructorEntry{typeConstructorEntryKind} ->
      Just typeConstructorEntryKind

traitKind :: Name -> Build a -> Maybe Kind
traitKind name Build{buildTraits} =
  case Environment.lookup name buildTraits of
    Nothing ->
      Nothing
    Just TraitEntry{traitEntryParameter} ->
      Just (KArrow (kindOf traitEntryParameter) KTrait)

aliasKind :: Name -> Build a -> Maybe Kind
aliasKind name Build{buildAliases} =
  case Environment.lookup name buildAliases of
    Nothing ->
      Nothing
    Just AliasEntry{..} ->
      Just (kindOf aliasEntryType)

importedBuild :: (Monad m, Monoid a) => Path -> CompilerT a m (Build a)
importedBuild path = do
  CompilerState{compilerModules} <- get
  case Environment.lookup (principalPath path) compilerModules of
    Nothing -> do
      tellErrors [ModuleNotFound (principalPath path) (ErrorLocation (principalPath path) mempty)]
      throwError CompilerError
    Just build ->
      pure build
