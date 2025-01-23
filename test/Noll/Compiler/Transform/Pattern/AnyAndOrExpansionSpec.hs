{-# LANGUAGE OverloadedStrings #-}

module Noll.Compiler.Transform.Pattern.AnyAndOrExpansionSpec where

import Noll.Compiler.Transform.Pattern.AnyAndOrExpansion
import Noll.Label (Label (..))
import Noll.Language (
  Pattern (..),
 )
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "" $ do
    it "" $
      1 > 2

fixture = PConstructor () (Label () "Foo") [PVariable () (Label () "a"), PVariable () (Label () "b")]
