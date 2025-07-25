{-# LANGUAGE LambdaCase #-}

module Coal.Kernel.Language.Type.Row (normalizeRow, extend, dropField) where

import Extra (Map, Name)
import Coal.Kernel.Language.Type (Type (..))

import qualified Data.Map.Strict as Map

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

toMap :: Map Name [Type] -> Type -> (Map Name [Type], Type)
toMap m =
  \case
    RExt name t r ->
      toMap (Map.insertWith (<>) name [t] m) r
    r ->
      (m, r)

fromMap :: (Map Name [Type], Type) -> Type
fromMap (d, row) = Map.foldrWithKey (flip . foldr . RExt) row d
