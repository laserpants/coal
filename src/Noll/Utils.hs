{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Utils (
  Label (..),
  Name,
  Dictionary,
  Some,
) where

import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Text (Text)

type Name = Text

data Label t = Label t Name
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

type Dictionary = Map Name

type Some = NonEmpty
