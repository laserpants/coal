{-# LANGUAGE StrictData #-}

module Noll.Library (Dictionary, Some) where

import Data.List.NonEmpty (NonEmpty)
import Data.Map.Strict (Map)
import Noll.Language (Name)

type Dictionary = Map Name

type Some = NonEmpty
