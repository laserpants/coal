{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Module.Definition (Definition (..), Path (..), definitionName) where

import Coal.Language.Module.Definition.Alias (AliasDef (..))
import Coal.Language.Module.Definition.Constant (ConstantDef (..))
import Coal.Language.Module.Definition.Cotype (CotypeDef (..))
import Coal.Language.Module.Definition.Fold (FoldDef (..))
import Coal.Language.Module.Definition.Function (FunctionDef (..))
import Coal.Language.Module.Definition.Instance (InstanceDef (..))
import Coal.Language.Module.Definition.Trait (TraitDef (..))
import Coal.Language.Module.Definition.Type (TypeDef (..))
import Coal.Language.Module.Definition.Unfold (UnfoldDef (..))
import Data.Data (Data, Typeable)
import Extra (Name)

newtype Path = Path {pathComponents :: [Name]}
  deriving (Show, Eq, Ord, Read, Data, Typeable)

data Definition a k t
  = -- | Type definition
    DType a Name TypeDef
  | -- | Codata type definition
    DCotype a Name CotypeDef
  | -- | Function definition
    DFunction a Name (FunctionDef a t) [Definition a k t]
  | -- | Other (constant) top-level definitions
    DConstant a Name (ConstantDef a t) [Definition a k t]
  | -- | Import statement
    DImport a Path [Name]
  | -- | Trait
    DTrait a Name (TraitDef t)
  | -- | Trait instance
    DInstance a Name (InstanceDef Definition a k t)
  | -- | Type alias
    DTypeAlias a Name AliasDef
  | -- | Top-level fold
    DFold a Name (FoldDef a t)
  | -- | Top-level unfold
    DUnfold a Name (UnfoldDef a t)
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
