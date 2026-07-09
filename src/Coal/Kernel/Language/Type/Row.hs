{-# LANGUAGE LambdaCase #-}

{- |
Row type operations.

Provides utilities for working with extensible record types (rows):

  * 'extend': Add a field to a row, automatically normalizing
  * 'normalizeRow': Sort fields lexicographically for canonical representation
  * 'toNormalForm': Convert a row to a map plus a tail
  * 'dropField': Remove a field from a row

= Normalization

Rows are normalized by sorting fields lexicographically. This enables
structural equality checking and simplifies type compatibility testing.

For example, @{y:int32, x:bool}@ and @{x:bool, y:int32}@ normalize to the same
representation.
-}
module Coal.Kernel.Language.Type.Row (
  normalizeRow,
  toNormalForm,
  extend,
  dropField,
) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Coal.Common.Name (Name)
import Coal.Kernel.Language.Type (Type (..))

{-# INLINE extend #-}
extend :: Name -> Type -> Type -> Type
extend field t r = normalizeRow (RExt field t r)

{-# INLINE normalizeRow #-}
normalizeRow :: Type -> Type
normalizeRow = fromMap . toMap mempty

dropField :: Name -> Type -> Type
dropField field t = fromMap (Map.delete field m, r)
 where
  (m, r) = toNormalForm t

{-# INLINE toNormalForm #-}
toNormalForm :: Type -> (Map Name Type, Type)
toNormalForm = toMap mempty

toMap :: Map Name Type -> Type -> (Map Name Type, Type)
toMap m =
  \case
    RExt name t r ->
      toMap (Map.insert name t m) r
    r ->
      (m, r)

fromMap :: (Map Name Type, Type) -> Type
fromMap (d, row) = Map.foldrWithKey RExt row d
