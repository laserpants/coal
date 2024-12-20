{-# LANGUAGE StrictData #-}

module Noll.Language.Type (Type (..)) where

import Noll.Language (Name)
import Noll.Language.Type.Intrinsic (Intrinsic (..))
import Noll.Language.Type.Row (Row (..))
import Noll.Library (Some)

data Type o k
  = TApplication k (Type o k) (Some (Type o k))
  | TArrow (Type o k) (Type o k)
  | TConstructor k Name
  | TIntrinsic (Intrinsic (Type o k))
  | TRow (Row o k (Type o k))
  | TVariable (o k)
  deriving (Show, Eq, Ord, Read)

infixr 1 `TArrow`
