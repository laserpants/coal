{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.TypeSystem.Constraint.Generation.State (
  ConstraintsGenState (..),
  overConstraintsGenStateTypeIndexes,
  overConstraintsGenStateSupply,
) where

import Coal.Common.Supply (Supply (..))
import Coal.Language (Kind (..), TypeIndex (..))
import Extras (Dictionary)

data ConstraintsGenState c = ConstraintsGenState
  { constraintsGenStateTypeIndexes :: Dictionary (c, TypeIndex Kind)
  , constraintsGenStateSupply :: Int
  }
  deriving (Show, Eq, Ord, Read)

instance Supply (ConstraintsGenState c) where
  updateSupply = overConstraintsGenStateSupply
  getSupply = constraintsGenStateSupply

{-# INLINE overConstraintsGenStateTypeIndexes #-}
overConstraintsGenStateTypeIndexes :: (Dictionary (c, TypeIndex Kind) -> Dictionary (c, TypeIndex Kind)) -> ConstraintsGenState c -> ConstraintsGenState c
overConstraintsGenStateTypeIndexes fn ConstraintsGenState{..} =
  ConstraintsGenState
    { constraintsGenStateTypeIndexes = fn constraintsGenStateTypeIndexes
    , ..
    }

{-# INLINE overConstraintsGenStateSupply #-}
overConstraintsGenStateSupply :: (Int -> Int) -> ConstraintsGenState c -> ConstraintsGenState c
overConstraintsGenStateSupply fn ConstraintsGenState{..} =
  ConstraintsGenState
    { constraintsGenStateSupply = fn constraintsGenStateSupply
    , ..
    }
