{-# LANGUAGE StrictData #-}

module Noll.Language.Type (Type (..), foldType) where

import Noll.Language.Type.Intrinsic (Intrinsic (..))
import Noll.Language.Type.Row (Row (..))
import Noll.Utils (Name, Some)

data Type o k
  = TApplication k (Type o k) (Some (Type o k))
  | TArrow (Type o k) (Type o k)
  | TConstructor k Name
  | TIntrinsic (Intrinsic (Type o k))
  | TRow (Row o k (Type o k))
  | TVariable (o k)
  | TAlias Name [Type o k] (Type o k)
  deriving (Show, Eq, Ord, Read)

infixr 1 `TArrow`

{-# INLINE foldType #-}
foldType :: (Foldable f) => Type o k -> f (Type o k) -> Type o k
foldType = foldr TArrow
