{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Module.Definition (Definition (..), Path (..), definitionName) where

import Data.Data (Data, Typeable)
import Extra (Name)
import Noll.Language.Constructor (Constructor (..))
import Noll.Language.Expression (Expression (..))
import Noll.Language.Module.Constant (Constant (..))
import Noll.Language.Module.Function (Function (..))
import Noll.Language.Trait (Trait (..), With (..))
import Noll.Language.Type (Parameter, Type (..))
import Noll.Language.Type.Kind (Kind (..))

newtype Path = Path {pathComponents :: [Name]}
  deriving (Show, Eq, Ord, Read, Data, Typeable)

type ParameterizedType = Type Parameter ()

data Definition a k t
  = -- | Type-annotated definition
    DAnnotation (With ParameterizedType) (Definition a k t)
  | -- | Type definition
    DType Name [Parameter ()] [Constructor Parameter () ParameterizedType]
  | -- | Codata type definition
    DCodata Name [Parameter ()] [(Name, ParameterizedType)]
  | -- | Function definition
    DFunction Name (Function Expression a t)
  | -- | Other (constant) top-level definitions
    DConstant Name (Constant Expression a t)
  | -- | Stand-alone type signature
    DSignature Name (With ParameterizedType)
  | -- | Import statement
    DImport Path [Name]
  | -- | Trait
    DTrait Name [Trait t] (Parameter Kind) [(Name, ParameterizedType)]
  | -- | Trait instance
    DInstance Name ParameterizedType [Definition a k t]
  | -- | Trait instance
    DTypeAlias Name [Parameter ()] ParameterizedType
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable)

definitionName :: Definition a k t -> Name
definitionName =
  \case
    DFunction name _ ->
      name
    DConstant name _ ->
      name
    DAnnotation _ d ->
      definitionName d
    DCodata name _ _ ->
      name
    _ ->
      error "Not implemented"
