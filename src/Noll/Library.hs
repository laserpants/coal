{-# LANGUAGE StrictData #-}

module Noll.Library (Dictionary) where

import Data.Map.Strict (Map)
import Noll.Language (Name)

type Dictionary = Map Name
