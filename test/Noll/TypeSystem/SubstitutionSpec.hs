{-# LANGUAGE OverloadedStrings #-}

module Noll.TypeSystem.SubstitutionSpec where

import Noll.Language.Type (Type)
import qualified Noll.Language.Type as Type
import Noll.Language.Type.Index (TypeIndex (..))
import qualified Noll.Language.Type.Intrinsic as Intrinsic
import Noll.Language.Type.Kind (Kind)
import qualified Noll.Language.Type.Kind as Kind
import Noll.TypeSystem.Substitution
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "Noll.TypeSystem.Substitution" $ do
    it "" $
      apply sub t1 == Type.Intrinsic Intrinsic.Bool
    it "" $
      apply sub t2 == t2

t1 :: Type TypeIndex (Kind Int)
t1 = Type.Variable (TypeIndex Kind.Type 1)

t2 :: Type TypeIndex (Kind Int)
t2 = Type.Variable (TypeIndex Kind.Type 0)

sub :: TypeSubstitution
sub = mapsTo 1 (Type.Intrinsic Intrinsic.Bool)
