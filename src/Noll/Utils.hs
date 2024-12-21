{-# LANGUAGE StrictData #-}

module Noll.Utils (
  Name,
  Dictionary,
  Some,
) where

import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Data.Text (Text)

type Name = Text

type Dictionary = Map Name

type Some = NonEmpty
