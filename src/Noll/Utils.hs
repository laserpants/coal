{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StrictData #-}

module Noll.Utils (
  module Control.Monad,
  module Data.Foldable,
  module Data.Set,
  module Data.Map.Strict,
  module Noll.Utils.Operators,
  module Noll.Utils.Name,
  module Noll.Utils.Data.Functor.Foldable,
  module Noll.Utils.Data.Text,
  module Noll.Utils.Data.Set,
  module Noll.Utils.Data.List,
  module Noll.Utils.Data.Functor,
  module Noll.Utils.Control.Monad,
  module Noll.Utils.Control.Monad.State,
  module Noll.Utils.Control.Monad.Writer,
  module Data.Tuple.Extra,
  IndexMap,
  fromMaybe,
  lexOrderRank,
  const2,
  traverse2,
  optionalOr,
  Over,
)
where

import Control.Applicative (Alternative (..))
import Control.Monad (forM, forM_, mapM)
import Data.Char (ord)
import Data.Foldable (foldrM, traverse_)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Data.Tuple.Extra (first, second)
import Noll.Utils.Control.Monad
import Noll.Utils.Control.Monad.State
import Noll.Utils.Control.Monad.Writer
import Noll.Utils.Data.Functor
import Noll.Utils.Data.Functor.Foldable
import Noll.Utils.Data.List
import Noll.Utils.Data.Set
import Noll.Utils.Data.Text
import Noll.Utils.Name
import Noll.Utils.Operators

import qualified Data.Text as Text

type IndexMap = Map Int

type Over o n = (n -> n) -> o -> o

{-# INLINE const2 #-}
const2 :: a -> b -> c -> a
const2 a _ _ = a

{-# INLINE traverse2 #-}
traverse2 :: (Applicative f, Traversable t1, Traversable t2) => (a -> f b) -> t2 (t1 a) -> f (t2 (t1 b))
traverse2 = traverse . traverse

{-# INLINE optionalOr #-}
optionalOr :: (Alternative f) => a -> f a -> f a
optionalOr def fa = fa <|> pure def

lexOrderRank :: Text -> Int
lexOrderRank text
  | Text.null text =
      error "Empty string"
  | otherwise =
      snd (Text.foldr f (0, 0) text) - 1
 where
  f :: Char -> (Int, Int) -> (Int, Int)
  f c (m, n) = (m + 1, n + (36 ^ m) + g (ord c))
  g n
    | not ((n >= 97 && n <= 122) || (n >= 48 && n <= 57)) =
        error "Invalid character"
    | n >= 97 =
        n - 97
    | otherwise =
        n - 22
