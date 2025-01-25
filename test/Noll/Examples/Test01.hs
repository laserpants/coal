{-# LANGUAGE OverloadedStrings #-}

module Noll.Examples.Test01 (test01) where

import Noll.Language (Module (..), Path (..))

import qualified Noll.Language.Module as Module

moduleOrdered :: Module () () ()
moduleOrdered =
  Module.fromObjectList
    (Path ["Ordered"])
    -- Exports
    []
    -- Objects
    [
    ]

moduleBinarySearch :: Module () () ()
moduleBinarySearch =
  Module.fromObjectList
    (Path ["BinarySearch"])
    -- Exports
    ["Tree", "build_tree", "flatten_tree"]
    -- Objects
    [
    ]

moduleMain :: Module () () ()
moduleMain =
    Module.fromObjectList
      (Path ["Main"])
      -- Exports
      []
      -- Objects
      [
      ]

-- Untyped source tree
test01 :: [Module () () ()]
test01 =
  [ moduleOrdered
  , moduleBinarySearch
  , moduleMain
  ]
