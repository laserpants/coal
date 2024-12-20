{-# LANGUAGE StrictData #-}

module Noll.Language.Type (Type (..)) where

import Noll.Language (Name)
import Noll.Language.Type.Intrinsic (Intrinsic (..))
import Noll.Language.Type.Row (Row (..))
import Noll.Library (Some)

data Type o k
  = Application k (Type o k) (Some (Type o k))
  | Arrow (Type o k) (Type o k)
  | Constructor k Name
  | Intrinsic (Intrinsic (Type o k))
  | Row (Row o k (Type o k))
  | Variable (o k)
  deriving (Show, Eq, Ord, Read)

infix 1 `Arrow`
