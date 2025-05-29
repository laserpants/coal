{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Noll.Set3.Test01 where

import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Noll.Language
import Noll.Language.Type.Intrinsic
import Noll.Module

import qualified Noll.Module as Module

-- Untyped source tree
prog3_01 :: [Module () () ()]
prog3_01 =
  [ moduleExample
  , moduleMain
  ]

moduleExample :: Module () () ()
moduleExample =
  undefined

moduleMain :: Module () () ()
moduleMain =
  undefined


