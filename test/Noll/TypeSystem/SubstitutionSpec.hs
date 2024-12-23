{-# LANGUAGE OverloadedStrings #-}

module Noll.TypeSystem.SubstitutionSpec where

import Test.Hspec (Spec, describe, it)
import Noll.TypeSystem.Substitution
import qualified Noll.Language.Type.Intrinsic as Intrinsic
import qualified Noll.Language.Type as Type
import Noll.Language.Type (Type)
import Noll.Language.Type.Index (TypeIndex (..))

spec :: Spec
spec =
  undefined
--  describe "Noll.TypeSystem.Substitution" $ do
--    it "" $
--      apply sub (Type.Variable (TypeIndex () 1)) == (Type.Intrinsic Intrinsic.Bool)
--
--sub :: Substitution (Type TypeIndex ())
--sub = mapsTo 1 (Type.Intrinsic Intrinsic.Bool)
