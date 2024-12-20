{-# LANGUAGE StrictData #-}

module Noll.Language.Type (Type (..)) where

import Noll.Language (Name)
import Noll.Language.Type.Intrinsic (Intrinsic (..))

data Type o k
  = TArrow (Type o k) (Type o k)
  | TConstructor k Name
  | TIntrinsic (Intrinsic (Type o k))
  | TVariable (o k)
  deriving (Show, Eq, Ord, Read)

infixr 1 `TArrow`
