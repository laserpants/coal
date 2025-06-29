{-# LANGUAGE OverloadedStrings #-}

module Noll.Compiler2Spec where

import Control.Monad.Identity (runIdentity)
import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Noll.Compiler2
--import Noll.Compiler2Examples.Test02 (bazz)
import Noll.Language
import Noll.Module (Constant (..), Definition (..), Function (..), Module (..))
import Noll.SystemF
import Noll.SystemFSpec.TestRunner
import Test.Hspec (Spec, describe, it)

import qualified Data.Set as Set
import qualified Lang.Common.Environment as Environment

spec :: Spec
spec =
  describe "Noll.Compiler2" $ do
    it "" $ do
      1 == 2


