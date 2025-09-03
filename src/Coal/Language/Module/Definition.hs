{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Module.Definition (Definition (..), Path (..), definitionName) where

import Coal.Language.Expression (Clause (..), Expression (..))
import Coal.Language.Module.Constant (ConstantDef (..))
import Coal.Language.Module.Cotype (CotypeDef (..))
import Coal.Language.Module.Function (FunctionDef (..))
import Coal.Language.Module.Type (TypeDef (..))
import Coal.Language.Pattern (Pattern (..))
import Coal.Language.Trait (Trait (..), With (..))
import Coal.Language.Type (Parameter, ParameterizedType)
import Coal.Language.Type.Kind (Kind (..))
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
    DFunction a Name (FunctionDef Expression a t) [Definition a k t]
  | -- | Other (constant) top-level definitions
    DConstant a Name (ConstantDef Expression a t) [Definition a k t]
  | -- | Import statement
    DImport a Path [Name]
  | -- | Trait
    DTrait a Name [Trait t] (Parameter Kind) [(Name, ParameterizedType)]
  | -- | Trait instance
    DInstance a Name [Trait ParameterizedType] ParameterizedType [Definition a k t]
  | -- | Type alias
    DTypeAlias a Name [Parameter ()] ParameterizedType
  | -- | Top-level fold
    DFold a Name (With ParameterizedType) (NonEmpty (Clause a t)) (Maybe (Expression a t))
  | -- | Top-level unfold
    DUnfold a Name (With ParameterizedType) (NonEmpty (Pattern a t)) (Dictionary (Expression a t)) (Maybe (Expression a t))
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
    DFold _ name _ _ _ ->
      name
    DUnfold _ name _ _ _ _ ->
      name
    _ ->
      error "Not implemented"
