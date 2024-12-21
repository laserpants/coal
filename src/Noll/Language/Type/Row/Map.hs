{-# LANGUAGE StrictData #-}

module Noll.Language.Type.Row.Map (RowMap (..)) where

import Noll.Language.Type.Row (Row (..))
import Noll.Utils (Dictionary)

data RowMap o k t = RowMap (Dictionary [t]) (Row o k t)
  deriving (Show, Eq, Ord, Read)
