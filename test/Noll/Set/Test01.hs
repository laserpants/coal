{-# LANGUAGE OverloadedStrings #-}

module Noll.Set.Test01 where

import Noll.Language
import Noll.Module (Definition (..), Function (..), Module (..), Path (..))

import qualified Noll.Module as Module

module1 :: Module () () ()
module1 =
  Module.fromDefinitionList
    (Path ["Utils"])
    -- Exports
    ["Predicate"]
    -- Definitions
    [ -- type_alias Predicate
      DTypeAlias
        "Predicate"
        [TVariable undefined]
        undefined
    ]

module2 :: Module () () ()
module2 =
  Module.fromDefinitionList
    (Path ["Ordered"])
    -- Exports
    ["Ordering", "Ordered", "less_than_or_equal_to", "greater_than"]
    -- Definitions
    []

-- import Utils(Predicate)
-- type Ordering
-- trait Ordered
-- instance Ordered(int32)
-- less_than_or_equal_to
-- greater_than

module3 :: Module () () ()
module3 =
  Module.fromDefinitionList
    (Path ["BinarySearch"])
    -- Exports
    ["Tree", "from_list", "flatten"]
    -- Definitions
    []

-- type Tree
-- type_alias Range
-- in_range
-- from_list
-- flatten
-- sort

module4 :: Module () () ()
module4 =
  Module.fromDefinitionList
    (Path ["Main"])
    -- Exports
    []
    -- Definitions
    []

-- import BinarySearch
-- main
