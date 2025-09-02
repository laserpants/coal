{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Module.Definition (Definition (..), Path (..), definitionName) where

import Coal.Language.DataConstructor (DataConstructor (..))
import Coal.Language.Expression (Clause (..), Expression (..))
import Coal.Language.Module.Constant (Constant (..))
import Coal.Language.Module.Function (Function (..))
import Coal.Language.Pattern (Pattern (..))
import Coal.Language.Trait (Trait (..), With (..))
import Coal.Language.Type (Parameter, Type (..))
import Coal.Language.Type.Kind (Kind (..))
import Data.Data (Data, Typeable)
import Data.List.NonEmpty (NonEmpty)
import Extra (Dictionary, Name)

newtype Path = Path {pathComponents :: [Name]}
  deriving (Show, Eq, Ord, Read, Data, Typeable)

type ParameterizedType = Type Parameter ()

-- TODO: Make DAnnotation separate object?
data Definition a k t
  = -- | Type-annotated definition
    DAnnotation (With ParameterizedType) (Definition a k t)
  | -- | Type definition
    DType a Name [Parameter ()] [DataConstructor Parameter () ParameterizedType]
  | -- | Codata type definition
    DCodata a Name [Parameter ()] [(Name, ParameterizedType)]
  | -- | Function definition
    DFunction Name (Function Expression a t) [Definition a k t]
  | -- | Other (constant) top-level definitions
    DConstant Name (Constant Expression a t) [Definition a k t]
  | -- | Import statement
    DImport a Path [Name]
  | -- | Trait
    DTrait Name [Trait t] (Parameter Kind) [(Name, ParameterizedType)]
  | -- | Trait instance
    DInstance Name [Trait ParameterizedType] ParameterizedType [Definition a k t]
  | -- | Type alias
    DTypeAlias a Name [Parameter ()] ParameterizedType
  | -- | Top-level fold
    DFold a Name (NonEmpty (Clause a t)) (Maybe (Expression a t))
  | -- | Top-level unfold
    DUnfold a Name (NonEmpty (Pattern a t)) (Dictionary (Expression a t)) (Maybe (Expression a t))
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
    DCodata _ name _ _ ->
      name
    DFold _ name _ _ ->
      name
    DUnfold _ name _ _ _ ->
      name
    _ ->
      error "Not implemented"
