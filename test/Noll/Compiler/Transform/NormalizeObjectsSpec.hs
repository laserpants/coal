{-# LANGUAGE OverloadedStrings #-}

module Noll.Compiler.Transform.NormalizeObjectsSpec where

import Noll.Compiler.Transform.NormalizeObjects (normalizeObject)
import Test.Hspec (Spec, describe, it)

import qualified Noll.Examples.Test04 as Test04
import qualified Noll.Examples.Test05 as Test05

spec :: Spec
spec =
  describe "Noll.Compiler.Transform.Expression" $ do
    describe "" $ do
      it "" $ do
        normalizeObject Test04.moduleOrdered == Test05.moduleOrdered
      it "" $ do
        normalizeObject Test04.moduleBinarySearch == Test05.moduleBinarySearch
