{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.Type.Row (Row (..)) where

import Data.Tuple.Extra (second)
import Noll.Utils (Dictionary, Name)
import Noll.Utils ((<$$>))

import qualified Data.Map.Strict as Map

data Row o k t
  = RExtend Name t (Row o k t)
  | RVariable (o k)
  | RNil
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

data RowMap o k t = RowMap (Dictionary [t]) (Row o k t)
  deriving (Show, Eq, Ord, Read)

toRowMap :: Row o k t -> RowMap o k t
toRowMap = go mempty
 where
  go m =
    \case
      RExtend name t r ->
        go (Map.insertWith (<>) name [t] m) r
      r ->
        RowMap m r

{-# INLINE fromRowMap #-}
fromRowMap :: RowMap o k t -> Row o k t
fromRowMap (RowMap d row) = Map.foldrWithKey (flip . foldr . RExtend) row d

{-# INLINE fromDictionary #-}
fromDictionary :: Dictionary [t] -> Row o k t -> Row o k t
fromDictionary = fromRowMap <$$> RowMap

{-# INLINE normalizeRow #-}
normalizeRow :: Row o k t -> Row o k t
normalizeRow = fromRowMap . toRowMap

extractField :: Name -> Row o k t -> Maybe (t, Row o k t)
extractField name row =
  second (fromRowMap . (`RowMap` r)) <$> extractFieldImpl
 where
  RowMap dict r = toRowMap row
  extractFieldImpl =
    case Map.lookup name dict of
      Just (t : ts) ->
        Just (t, Map.filter (not . null) (Map.insert name ts dict))
      _ ->
        Nothing

updateTail :: Row o k t -> Row o k t -> Row o k t
updateTail r =
  \case
    RExtend name t _ ->
      RExtend name t r
    row ->
      row
