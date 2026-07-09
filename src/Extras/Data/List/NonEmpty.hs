module Extras.Data.List.NonEmpty (unsnoc) where

import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty

{-# INLINE unsnoc #-}
unsnoc :: NonEmpty a -> ([a], a)
unsnoc xs = (NonEmpty.init xs, NonEmpty.last xs)
