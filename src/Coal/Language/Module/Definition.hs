{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Module.Definition (Definition (..), Path (..), definitionName) where

import Coal.Language.Expression (Clause (..), Expression (..))
import Coal.Language.Module.Alias (AliasDef (..))
import Coal.Language.Module.Constant (ConstantDef (..))
import Coal.Language.Module.Cotype (CotypeDef (..))
import Coal.Language.Module.Fold (FoldDef (..))
import Coal.Language.Module.Function (FunctionDef (..))
import Coal.Language.Module.Instance (InstanceDef (..))
import Coal.Language.Module.Trait (TraitDef (..))
import Coal.Language.Module.Type (TypeDef (..))
import Coal.Language.Module.Unfold (UnfoldDef (..))
import Coal.Language.Pattern (Pattern (..))
import Coal.Language.Trait (With (..))
import Coal.Language.Type (Parameter, ParameterizedType)
import Data.Data (Data, Typeable)
import Data.List.NonEmpty (NonEmpty)
import Extra (Dictionary, Name)

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
