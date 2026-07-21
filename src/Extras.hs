{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StrictData #-}

module Extras (
  module Control.Monad,
  module Data.Foldable,
  module Data.Set,
  module Data.Map.Strict,
  module Extras.Operators,
  module Coal.Common.Name,
  module Extras.Data.Functor.Foldable,
  module Extras.Data.Text,
  module Extras.Data.Set,
  module Extras.Data.Traversable,
  module Extras.Data.List,
  module Extras.Data.Functor,
  module Extras.Control.Applicative,
  module Extras.Control.Monad,
  module Extras.Control.Monad.State,
  module Extras.Control.Monad.Writer,
  module Data.Tuple.Extra,
  fromMaybe,
  const2,
  optionalOr,
  for,
  Over,
) where

import Coal.Common.Name
import Control.Applicative (Alternative (..))
import Control.Monad (foldM, forM, forM_, mapM, replicateM, void)
import Data.Foldable (foldrM, traverse_)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Tuple.Extra (first, second)
import Extras.Control.Applicative
import Extras.Control.Monad
import Extras.Control.Monad.State
import Extras.Control.Monad.Writer
import Extras.Data.Functor
import Extras.Data.Functor.Foldable
import Extras.Data.List
import Extras.Data.Set
import Extras.Data.Text
import Extras.Data.Traversable
import Extras.Operators

type Over o n = (n -> n) -> o -> o

{-# INLINE const2 #-}
const2 :: a -> b -> c -> a
const2 = const . const

{-# INLINE optionalOr #-}
optionalOr :: (Alternative f) => a -> f a -> f a
optionalOr def fa = fa <|> pure def

{-# INLINE for #-}
for :: (Functor f) => f a -> (a -> b) -> f b
for = flip fmap
