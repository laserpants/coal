{-# LANGUAGE StrictData #-}

module Noll.Language.Type.Kind (Kind (..), foldKind) where

data Kind o
  = KType
  | KRow
  | KArrow (Kind o) (Kind o)
  | KTrait
  | KVariable o
  deriving (Show, Eq, Ord, Read)

infixr 1 `KArrow`

{-# INLINE foldKind #-}
foldKind :: (Foldable f) => Kind o -> f (Kind o) -> Kind o
foldKind = foldr KArrow
