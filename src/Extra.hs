{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StrictData #-}

module Extra (
  module Control.Monad,
  module Data.Foldable,
  module Data.Set,
  module Data.Map.Strict,
  module Extra.Operators,
  module Lang.Common.Name,
  module Extra.Data.Functor.Foldable,
  module Extra.Data.Text,
  module Extra.Data.Set,
  module Extra.Data.List,
  module Extra.Data.Functor,
  module Extra.Control.Monad,
  module Extra.Control.Monad.State,
  module Extra.Control.Monad.Writer,
  module Data.Tuple.Extra,
  IndexMap,
  fromMaybe,
  const2,
  traverse2,
  optionalOr,
  Over,
  forSM,
  forSM_,
)
where

import Control.Applicative (Alternative (..))
import Control.Monad (forM, forM_, mapM, void)
import Data.Foldable (foldrM, traverse_)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Tuple.Extra (first, second)
import Extra.Control.Monad
import Extra.Control.Monad.State
import Extra.Control.Monad.Writer
import Extra.Data.Functor
import Extra.Data.Functor.Foldable
import Extra.Data.List
import Extra.Data.Set
import Extra.Data.Text
import Lang.Common.Name
import Extra.Operators

type IndexMap = Map Int

type Over o n = (n -> n) -> o -> o

{-# INLINE const2 #-}
const2 :: a -> b -> c -> a
const2 = const . const

{-# INLINE traverse2 #-}
traverse2 :: (Applicative f, Traversable t1, Traversable t2) => (a -> f b) -> t2 (t1 a) -> f (t2 (t1 b))
traverse2 = traverse . traverse

{-# INLINE optionalOr #-}
optionalOr :: (Alternative f) => a -> f a -> f a
optionalOr def fa = fa <|> pure def

{-# INLINE forSM #-}
forSM :: (Monad m, Enum n) => n -> [a] -> (a -> n -> m b) -> m [b]
forSM n vs = forM (zip vs [n ..]) . uncurry

{-# INLINE forSM_ #-}
forSM_ :: (Monad m, Enum n) => n -> [a] -> (a -> n -> m b) -> m ()
forSM_ n vs = void . forSM n vs
