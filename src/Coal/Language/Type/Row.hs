{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Coal.Language.Type.Row (
  Row (..),
  RowData (..),
  toRowData,
  fromRowData,
  fromDictionary,
  normalizeRow,
  extractField,
  updateTail,
) where

import Data.Data (Data, Typeable)
import Extras (Dictionary, Name, (<$$>))
import GHC.Generics (Generic)

import qualified Data.Map.Strict as Map

data Row o k t
  = RExtend Name t (Row o k t)
  | RVariable (o k)
  | RNil
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable, Generic)

data RowData o k t = RowData (Dictionary t) (Row o k t)
  deriving (Show, Eq, Ord, Read)

toRowData :: Row o k t -> RowData o k t
toRowData = go mempty
 where
  go m =
    \case
      RExtend name t r ->
        go (Map.insert name t m) r
      r ->
        RowData m r

{-# INLINE fromRowData #-}
fromRowData :: RowData o k t -> Row o k t
fromRowData (RowData d row) = Map.foldrWithKey RExtend row d

{-# INLINE fromDictionary #-}
fromDictionary :: Dictionary t -> Row o k t -> Row o k t
fromDictionary = fromRowData <$$> RowData

{-# INLINE normalizeRow #-}
normalizeRow :: Row o k t -> Row o k t
normalizeRow = fromRowData . toRowData

extractField :: Name -> Row o k t -> Maybe (t, Row o k t)
extractField name row =
  case Map.lookup name dict of
    Nothing ->
      Nothing
    Just t ->
      Just (t, fromDictionary (Map.delete name dict) r)
 where
  RowData dict r =
    toRowData row

updateTail :: Row o k t -> Row o k t -> Row o k t
updateTail r =
  \case
    RExtend name t _ ->
      RExtend name t r
    row ->
      row
