{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Module.Definition (Definition (..), Path (..), definitionName) where

import Coal.Language.DataConstructor (DataConstructor (..))
import Coal.Language.Expression (Expression (..))
import Coal.Language.Module.Constant (Constant (..))
import Coal.Language.Module.Function (Function (..))
import Coal.Language.Trait (Trait (..), With (..))
import Coal.Language.Type (Parameter, Type (..))
import Coal.Language.Type.Kind (Kind (..))
import Data.Data (Data, Typeable)
import Extra (Name)

newtype Path = Path {pathComponents :: [Name]}
  deriving (Show, Eq, Ord, Read, Data, Typeable)

type ParameterizedType = Type Parameter ()

-- TODO: Make DAnnotation separate object?
data Definition a k t
  = -- | Type-annotated definition
    DAnnotation (With ParameterizedType) (Definition a k t)
  | -- | Type definition
    DType Name [Parameter ()] [DataConstructor Parameter () ParameterizedType]
  | -- | Codata type definition
    DCodata Name [Parameter ()] [(Name, ParameterizedType)]
  | -- | Function definition
    DFunction Name (Function Expression a t) [Definition a k t]
  | -- | Other (constant) top-level definitions
    DConstant Name (Constant Expression a t) [Definition a k t]
  | -- | Import statement
    DImport Path [Name]
  | -- | Trait
    DTrait Name [Trait t] (Parameter Kind) [(Name, ParameterizedType)]
  | -- | Trait instance
    DInstance Name [Trait ParameterizedType] ParameterizedType [Definition a k t]
  | -- | Type alias
    DTypeAlias Name [Parameter ()] ParameterizedType
  | -- | Top-level fold
    DFold Name -- TODO
  | -- | Top-level unfold
    DUnfold Name -- TODO
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)

definitionName :: Definition a k t -> Name
definitionName =
  \case
    DFunction name _ _ ->
      name
    DConstant name _ _ ->
      name
    DAnnotation _ d ->
      definitionName d
    DCodata name _ _ ->
      name
    DFold name ->
      name
    DUnfold name ->
      name
    _ ->
      error "Not implemented"
