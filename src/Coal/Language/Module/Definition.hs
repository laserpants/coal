{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Module.Definition (
  Definition (..),
  Import (..),
  Path (..),
  definitionName,
  isImport,
  isType,
  importPath,
) where

import Coal.Language.Module.Definition.Alias (AliasDefinition (..))
import Coal.Language.Module.Definition.Constant (ConstantDefinition (..))
import Coal.Language.Module.Definition.Cotype (CotypeDefinition (..))
import Coal.Language.Module.Definition.Fold (FoldDefinition (..))
import Coal.Language.Module.Definition.Function (FunctionDefinition (..))
import Coal.Language.Module.Definition.Instance (InstanceDefinition (..))
import Coal.Language.Module.Definition.Trait (TraitDefinition (..))
import Coal.Language.Module.Definition.Type (TypeDefinition (..))
import Coal.Language.Module.Definition.Unfold (UnfoldDefinition (..))
import Coal.Language.Module.Import (Import (..))
import Coal.Language.Module.Path (Path (..))
import Data.Data (Data, Typeable)
import Data.List.NonEmpty (NonEmpty)
import Extras (Name)

data Definition a k t
  = -- | Type definition
    DType a Name TypeDefinition
  | -- | Codata type definition
    DCotype a Name CotypeDefinition
  | -- | Function definition
    DFunction a Name (NonEmpty (FunctionDefinition a t)) [Definition a k t]
  | -- | Other (constant) top-level definitions
    DConstant a Name (ConstantDefinition a t) [Definition a k t]
  | -- | Import statement
    DImport a Path [Import a]
  | -- | Namespace (qualified) import
    DQualifiedImport a Path
  | -- | Trait
    DTrait a Name (TraitDefinition ())
  | -- | Trait instance
    DInstance a Name (InstanceDefinition Definition a k t)
  | -- | Type alias
    DTypeAlias a Name AliasDefinition
  | -- | Top-level fold
    DFold a Name (FoldDefinition a t)
  | -- | Top-level unfold
    DUnfold a Name (UnfoldDefinition a t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)

definitionName :: Definition a k t -> Name
definitionName =
  \case
    DFunction _ name _ _ ->
      name
    DConstant _ name _ _ ->
      name
    DCotype _ name _ ->
      name
    DFold _ name _ ->
      name
    DUnfold _ name _ ->
      name
    _ ->
      error "Not implemented"

isImport :: Definition a k t -> Bool
isImport =
  \case
    DImport{} ->
      True
    DQualifiedImport{} ->
      True
    _ ->
      False

isType :: Definition a k t -> Bool
isType =
  \case
    DType{} ->
      True
    _ ->
      False

importPath :: Definition a k t -> Maybe (a, Path)
importPath =
  \case
    DImport loc p _ ->
      Just (loc, p)
    DQualifiedImport loc p ->
      Just (loc, p)
    _ ->
      Nothing
