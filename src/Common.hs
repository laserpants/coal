module Common (Name, unsnoc) where

-- TODO: REMOVE

import Coal.Common.Name (Name)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty

{-# INLINE unsnoc #-}
unsnoc :: NonEmpty a -> ([a], a)
unsnoc xs = (NonEmpty.init xs, NonEmpty.last xs)
