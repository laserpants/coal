{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Language (Label (..), Name) where

import Data.Text (Text)

type Name = Text

data Label t = Label t Name
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)
