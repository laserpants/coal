{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.TypeSystem.Constraint.Generation.State (
  ConstraintsGenState (..),
  overConstraintsGenStateAnnotationIndexes,
  overConstraintsGenStateSupply,
) where

import Coal.Common.Supply (Supply (..))
import Coal.Language (Kind (..), TypeIndex (..))
import Extras (Dictionary)

data ConstraintsGenState c = ConstraintsGenState
  { constraintsGenStateAnnotationIndexes :: Dictionary (c, TypeIndex Kind)
  , constraintsGenStateSupply :: Int
  }
  deriving (Show, Eq, Ord, Read)

instance Supply (ConstraintsGenState c) where
  updateSupply = overConstraintsGenStateSupply
  getSupply = constraintsGenStateSupply

{-# INLINE overConstraintsGenStateAnnotationIndexes #-}
overConstraintsGenStateAnnotationIndexes :: (Dictionary (c, TypeIndex Kind) -> Dictionary (c, TypeIndex Kind)) -> ConstraintsGenState c -> ConstraintsGenState c
overConstraintsGenStateAnnotationIndexes fn ConstraintsGenState{..} =
  ConstraintsGenState
    { constraintsGenStateAnnotationIndexes = fn constraintsGenStateAnnotationIndexes
    , ..
    }

{-# INLINE overConstraintsGenStateSupply #-}
overConstraintsGenStateSupply :: (Int -> Int) -> ConstraintsGenState c -> ConstraintsGenState c
overConstraintsGenStateSupply fn ConstraintsGenState{..} =
  ConstraintsGenState
    { constraintsGenStateSupply = fn constraintsGenStateSupply
    , ..
    }
