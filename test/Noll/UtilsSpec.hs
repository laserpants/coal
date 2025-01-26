{-# LANGUAGE OverloadedStrings #-}

module Noll.UtilsSpec (spec) where

import Noll.Utils (lexOrderRank)
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "Noll.Utils" $ do
    describe "lexOrderRank" $ do
      it "a" $
        lexOrderRank "a" == 0
      it "b" $
        lexOrderRank "b" == 1
      it "z" $
        lexOrderRank "z" == 25
      it "0" $
        lexOrderRank "0" == 26
      it "9" $
        lexOrderRank "9" == 35
      it "aa" $
        lexOrderRank "aa" == 36
      it "ab" $
        lexOrderRank "ab" == 37
      it "az" $
        lexOrderRank "az" == 61
      it "a0" $
        lexOrderRank "a0" == 62
      it "aaa" $
        lexOrderRank "aaa" == 1332
