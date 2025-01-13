{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Module.Object (Object (..)) where

import Noll.Language.Constructor (Constructor (..))
import Noll.Language.Expression (Expression (..))
import Noll.Language.Module.Function (Function (..))
import Noll.Language.Module.Global (Global (..))
import Noll.Language.Trait (Trait (..), Uses (..))
import Noll.Language.Type (Type (..), TypeIndex)
import Noll.Utils (Name)

type Annotation = Type TypeIndex

newtype Path = Path {pathComponents :: [Name]}
  deriving (Show, Eq, Ord, Read)

-- TODO: move
data Instance e a t
  = TFunction (Function e a t)
  | TConstant (Global e a t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

data Object a k t
  = -- | Type definition
    DType Name [Annotation k] [Constructor TypeIndex k (Type TypeIndex k)]
  | -- | Codata type definition
    DCotype a Name -- TODO
  | -- | Function definition
    DFunction Name (Function Expression a t)
  | -- | TODO
    DGlobal Name (Global Expression a t)
  | -- | Type signature
    DSignature Name (Uses (Annotation k))
  | -- | Import statement
    DImport Path [Name]
  | -- | Trait
    DTrait Name [Trait t] (Annotation k) [TypeIndex (Annotation k)]
  | --   | -- | Trait instance
    DInstance Name (Annotation k) [(Name, Instance Expression a t)]
  | -- | Type alias
    DTypeAlias Name [Annotation k] (Type TypeIndex k)
  --  | -- | TODO
  --    DAnnotation (Uses (Annotation k)) (Object k t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)
