{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Type.Kind (Kind (..), KindIndex (..), KindRep (..), foldKind) where

data Kind o
  = KType
  | KRow
  | KArrow (Kind o) (Kind o)
  | KTrait
  | KVariable o
  deriving (Show, Eq, Ord, Read)

infixr 1 `KArrow`

newtype KindIndex = KindIndex {kindIndexId :: Int}
  deriving (Show, Eq, Ord, Read)

class KindRep k where
  kindRep :: Kind KindIndex -> k

instance KindRep () where
  kindRep = const ()

instance KindRep (Kind KindIndex) where
  kindRep = id

{-# INLINE foldKind #-}
foldKind :: (Foldable f) => Kind o -> f (Kind o) -> Kind o
foldKind = foldr KArrow
