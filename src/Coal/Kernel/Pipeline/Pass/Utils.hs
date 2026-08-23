{-# LANGUAGE LambdaCase #-}

{- |
Shared utility functions used by multiple kernel normalization passes.

Provides general-purpose helpers for transforming expressions within the
normalization pipeline.
-}
module Coal.Kernel.Pipeline.Pass.Utils (
  opOperands,
  rebuildOp,
) where

import Control.Monad.State.Strict (State, evalState, state)
import Data.Foldable (toList)

-- | Extract the operands of a 'Traversable'/'Foldable' functor as a list.
opOperands :: (Foldable f) => f a -> [a]
opOperands = toList

{- | Rebuild a 'Traversable' functor by popping from a replacement list in
order. The replacement list must supply exactly as many elements as the functor
contains.
-}
rebuildOp :: (Traversable f) => [a] -> f b -> f a
rebuildOp ts op = evalState (traverse (const pop) op) ts
 where
  pop :: State [a] a
  pop = state $
    \case
      (x : xs) -> (x, xs)
      [] -> error "rebuildOp: empty list"
