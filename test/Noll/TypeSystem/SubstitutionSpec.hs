{-# LANGUAGE OverloadedStrings #-}

module Noll.TypeSystem.SubstitutionSpec where

import Noll.Language.Type (Type)
import qualified Noll.Language.Type as Type
import Noll.Language.Type.Index (TypeIndex (..))
import qualified Noll.Language.Type.Intrinsic as Intrinsic
import Noll.TypeSystem.Substitution
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "Noll.TypeSystem.Substitution" $ do
    it "" $
      apply sub (Type.Variable (TypeIndex () 1)) == (Type.Intrinsic Intrinsic.Bool)
    it "" $
      apply sub (Type.Variable (TypeIndex () 0)) == (Type.Variable (TypeIndex () 0))

sub :: TypeSubstitution
sub = mapsTo 1 (Type.Intrinsic Intrinsic.Bool)
