{-# LANGUAGE OverloadedStrings #-}

module Noll.Language.HasTypeIndexesSpec where

import Data.Set (Set)
import qualified Data.Set as Set
import Noll.Language.HasTypeIndexes
import Noll.Language.Type (Type (..))
import qualified Noll.Language.Type as Type
import Noll.Language.Type.Index (TypeIndex (..))
import Noll.Language.Type.Kind (Kind)
import qualified Noll.Language.Type.Kind as Kind
import Noll.Language.Type.Scheme (Scheme (..))
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "Noll.Language.HasTypeIndexes" $ do
    it "" $
      1 == 1

fixture_1 :: Scheme TypeIndex () (Type TypeIndex ())
fixture_1 =
  Forall
    (Set.fromList [TypeIndex () 0])
    []
    (Type.Variable (TypeIndex () 3) `Type.Arrow` Type.Variable (TypeIndex () 0) `Type.Arrow` Type.Variable (TypeIndex () 0))

fixture_2 :: Scheme TypeIndex () (Type TypeIndex ())
fixture_2 =
  Forall
    (Set.fromList [])
    []
    (Type.Variable (TypeIndex () 3) `Type.Arrow` Type.Variable (TypeIndex () 0) `Type.Arrow` Type.Variable (TypeIndex () 0))

fixture_3 :: Scheme TypeIndex () (Type TypeIndex ())
fixture_3 =
  Forall
    (Set.fromList [TypeIndex () 0])
    []
    (Type.Variable (TypeIndex () 0) `Type.Arrow` Type.Variable (TypeIndex () 0) `Type.Arrow` Type.Variable (TypeIndex () 0))

fixture_4 :: Scheme TypeIndex (Kind Int) (Type TypeIndex (Kind Int))
fixture_4 =
  Forall
    (Set.fromList [TypeIndex Kind.Type 0])
    []
    (Type.Variable (TypeIndex Kind.Row 0) `Type.Arrow` Type.Variable (TypeIndex Kind.Type 0) `Type.Arrow` Type.Variable (TypeIndex Kind.Trait 0))

fixture_5 :: Scheme TypeIndex (Kind Int) (Type TypeIndex (Kind Int))
fixture_5 =
  Forall
    (Set.fromList [TypeIndex Kind.Type 0])
    []
    (Type.Variable (TypeIndex Kind.Row 0) `Type.Arrow` Type.Variable (TypeIndex Kind.Type 1) `Type.Arrow` Type.Variable (TypeIndex Kind.Trait 0))

boz :: Set (TypeIndex (Kind Int))
boz = typeIndexesIn fixture_5
