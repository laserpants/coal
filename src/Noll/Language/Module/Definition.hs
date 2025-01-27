{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Module.Definition (Definition (..), Path (..)) where

import Noll.Language.Constructor (Constructor (..))
import Noll.Language.Expression (Expression (..))
import Noll.Language.Module.Constant (Constant (..))
import Noll.Language.Module.Function (Function (..))
import Noll.Language.Trait (Trait (..), Uses (..))
import Noll.Language.Type (Parameter, Type (..), TypeIndex)
import Noll.Utils (Name)

newtype Path = Path {pathComponents :: [Name]}
  deriving (Show, Eq, Ord, Read)

data Definition a k t
  = -- | Type-annotated definition
    DAnnotation (Uses (Type Parameter ())) (Definition a k t)
  | -- | Type definition
    DType Name [Type TypeIndex k] [Constructor TypeIndex k (Type TypeIndex k)]
  | -- | Codata type definition
    DCodata a Name -- TODO
  | -- | Function definition
    DFunction Name (Function Expression a t)
  | -- | Other (constant) top-level definitions
    DConstant Name (Constant Expression a t)
  | -- | Stand-alone type signature
    DSignature Name (Uses (Type TypeIndex k))
  | -- | Import statement
    DImport Path [Name]
  | -- | Trait
    DTrait Name [Trait t] (Type TypeIndex k) [TypeIndex (Type TypeIndex k)]
  | -- | Trait instance
    DInstance Name (Type TypeIndex k) [Definition a k t]
  | -- | Type alias
    DTypeAlias Name [Type TypeIndex k] (Type TypeIndex k)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)
