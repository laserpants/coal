{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Type.Row (
  Row (..),
  RowData (..),
  fromDictionary,
  normalizeRow,
  extractField,
  updateTail,
) where

import Data.Data (Data, Typeable)
import Data.Tuple.Extra (second)
import GHC.Generics (Generic)
import Lang.Utils (Dictionary, Name, (<$$>))

import qualified Data.Map.Strict as Map

data Row o k t
  = RExtend Name t (Row o k t)
  | RVariable (o k)
  | RNil
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable, Data, Typeable, Generic)

data RowData o k t = RowData (Dictionary [t]) (Row o k t)
  deriving (Show, Eq, Ord, Read)

toRowData :: Row o k t -> RowData o k t
toRowData = go mempty
 where
  go m =
    \case
      RExtend name t r ->
        go (Map.insertWith (<>) name [t] m) r
      r ->
        RowData m r

{-# INLINE fromRowData #-}
fromRowData :: RowData o k t -> Row o k t
fromRowData (RowData d row) = Map.foldrWithKey (flip . foldr . RExtend) row d

{-# INLINE fromDictionary #-}
fromDictionary :: Dictionary [t] -> Row o k t -> Row o k t
fromDictionary = fromRowData <$$> RowData

{-# INLINE normalizeRow #-}
normalizeRow :: Row o k t -> Row o k t
normalizeRow = fromRowData . toRowData

extractField :: Name -> Row o k t -> Maybe (t, Row o k t)
extractField name row =
  second (fromRowData . (`RowData` r)) <$> go
 where
  go =
    case Map.lookup name dict of
      Just (t : ts) ->
        Just (t, Map.filter (not . null) (Map.insert name ts dict))
      _ ->
        Nothing
  RowData dict r =
    toRowData row

updateTail :: Row o k t -> Row o k t -> Row o k t
updateTail r =
  \case
    RExtend name t _ ->
      RExtend name t r
    row ->
      row
