{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Module.Object (Object (..)) where

import Noll.Language.Expression (Expression (..))
import Noll.Language.Module.Function (Function (..))
import Noll.Language.Module.Global (Global (..))
import Noll.Utils (Name)

newtype Path = Path {pathComponents :: [Name]}
  deriving (Show, Eq, Ord, Read)

data Object a k t
  = -- | Codata type definition
    DCotype a Name -- TODO
  | -- | Function definition
    DFunction Name (Function Expression a t)
  | -- | TODO
    DGlobal Name (Global Expression a t)
  | --  | -- | Type signature
    --    DSignature Name (Uses (Annotation k))
    --  | -- | Trait instance
    --    DInstance Name (Annotation k) [(Name, Instance Expression t)]

    -- | Import statement
    DImport Path [Name]
  --  | -- | Trait
  --    DTrait Name [Trait t] (Annotation k) [TypeId (Annotation k)]
  --  | -- | Type definition
  --    DType Name [Annotation k] [Constructor k (Type TypeId k)]
  --  | -- | Type alias
  --    DTypeAlias Name [Annotation k] (Type TypeId k)
  --  | -- | TODO
  --    DAnnotation (Uses (Annotation k)) (Object k t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)
