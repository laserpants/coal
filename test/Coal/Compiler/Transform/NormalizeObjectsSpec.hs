{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Transform.NormalizeObjectsSpec where

import Coal.Compiler.Transform.NormalizeObjects (normalizeObject)
import Test.Hspec (Spec, describe, it)

import qualified Coal.Examples.Test04 as Test04
import qualified Coal.Examples.Test05 as Test05

spec :: Spec
spec =
  describe "Coal.Compiler.Transform.Expression" $ do
    describe "" $ do
      it "" $ do
        normalizeObject Test04.moduleOrdered == Test05.moduleOrdered
      it "" $ do
        normalizeObject Test04.moduleBinarySearch == Test05.moduleBinarySearch
