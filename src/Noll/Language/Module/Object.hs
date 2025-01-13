{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Module.Object (Object (..), Path (..)) where

import Noll.Language.Constructor (Constructor (..))
import Noll.Language.Expression (Expression (..))
import Noll.Language.Module.Function (Function (..))
import Noll.Language.Module.Global (Global (..))
import Noll.Language.Trait (Trait (..), Uses (..))
import Noll.Language.Type (Type (..), TypeIndex)
import Noll.Utils (Name)

newtype Path = Path {pathComponents :: [Name]}
  deriving (Show, Eq, Ord, Read)

data Object a k t
  = -- | Type definition
    DType Name [Type TypeIndex k] [Constructor TypeIndex k (Type TypeIndex k)]
  | -- | Codata type definition
    DCotype a Name -- TODO
  | -- | Function definition
    DFunction Name (Function Expression a t)
  | -- | TODO
    DGlobal Name (Global Expression a t)
  | -- | Type signature
    DSignature Name (Uses (Type TypeIndex k))
  | -- | Import statement
    DImport Path [Name]
  | -- | Trait
    DTrait Name [Trait t] (Type TypeIndex k) [TypeIndex (Type TypeIndex k)]
  | -- | Trait instance
    DInstance Name (Type TypeIndex k) [Object a k t]
  | -- | Type alias
    DTypeAlias Name [Type TypeIndex k] (Type TypeIndex k)
  --  | -- | TODO
  --    DType TypeIndex (Uses (Type TypeIndex k)) (Object k t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)
