{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Type.Kind (Kind (..), foldKind) where

import Data.Data (Data, Typeable)

data Kind
  = KType
  | KRow
  | KArrow Kind Kind
  | KTrait
  deriving (Show, Eq, Ord, Read, Data, Typeable)

infixr 1 `KArrow`

{-# INLINE foldKind #-}
foldKind :: (Foldable f) => Kind -> f Kind -> Kind
foldKind = foldr KArrow
