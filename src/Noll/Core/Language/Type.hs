{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.Core.Language.Type (Type (..), normalizeRow) where

import Noll.Utils (Name)

import qualified Data.Map.Strict as Map

-- | Core language types
data Type
  = -- | Type constructor
    TCon Name [Type]
  | -- | Opaque type
    TOpq
  | -- | Row extension
    RExt Name Type Type
  | -- | Empty row
    RNil
  deriving (Show, Eq, Ord, Read)

normalizeRow :: Type -> Type
normalizeRow = fromMap . toMap mempty
 where
  toMap m =
    \case
      RExt name t r ->
        toMap (Map.insertWith (<>) name [t] m) r
      r ->
        (m, r)
  fromMap (d, row) =
    Map.foldrWithKey (flip . foldr . RExt) row d
