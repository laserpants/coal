{-# LANGUAGE LambdaCase #-}

module Coal.LegacyKernel.Language.Type.Row (normalizeRow, extend, dropField) where

import Coal.LegacyKernel.Language.Type (Type (..))
import qualified Data.Map.Strict as Map
import Extras (Map, Name)

{-# INLINE extend #-}
extend :: Name -> Type -> Type -> Type
extend field t r = normalizeRow (RExt field t r)

{-# INLINE normalizeRow #-}
normalizeRow :: Type -> Type
normalizeRow = fromMap . toMap mempty

dropField :: Name -> Type -> Type
dropField field t = fromMap (Map.delete field m, r)
 where
  (m, r) = toMap mempty t

toMap :: Map Name Type -> Type -> (Map Name Type, Type)
toMap m =
  \case
    RExt name t r ->
      toMap (Map.insert name t m) r
    r ->
      (m, r)

fromMap :: (Map Name Type, Type) -> Type
fromMap (d, row) = Map.foldrWithKey RExt row d
