{-# LANGUAGE StrictData #-}

module Noll.Language.Type.Kind (Kind (..), foldKind) where

data Kind o
  = Type
  | Row
  | Arrow (Kind o) (Kind o)
  | Trait
  | Variable o
  deriving (Show, Eq, Ord, Read)

infixr 1 `Arrow`

{-# INLINE foldKind #-}
foldKind :: (Foldable f) => Kind o -> f (Kind o) -> Kind o
foldKind = foldr Arrow
