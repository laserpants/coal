{-# LANGUAGE LambdaCase #-}

module Noll.Core.Language.Type.Row (normalizeRow, extend) where

import Noll.Core.Language.Type (Type (..))
import Noll.Utils (Name)

import qualified Data.Map.Strict as Map

{-# INLINE extend #-}
extend :: Name -> Type -> Type -> Type
extend field t r = normalizeRow (RExt field t r)

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
