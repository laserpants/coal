{-# LANGUAGE OverloadedStrings #-}

module Noll.Language.HasTypeIndexesSpec where

import Data.Set (Set)
import qualified Data.Set as Set
import Noll.Language.HasTypeIndexes
import Noll.Language.Type (Type (..))
import Noll.Language.Type.Index (TypeIndex (..))
import Noll.Language.Type.Kind (Kind (..))
import Noll.Language.Type.Scheme (Scheme (..))
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "Noll.Language.HasTypeIndexes" $ do
    describe "Scheme" $ do
      it "" $
        typeIndexesIn fixture_1 == (Set.fromList [TypeIndex () 3] :: Set (TypeIndex ()))
      it "" $
        typeIndexesIn fixture_2 == (Set.fromList [TypeIndex () 0, TypeIndex () 3] :: Set (TypeIndex ()))
      it "" $
        typeIndexesIn fixture_3 == (mempty :: Set (TypeIndex ()))
      it "" $
        typeIndexesIn fixture_4 == (mempty :: Set (TypeIndex (Kind Int)))
      it "" $
        typeIndexesIn fixture_5 == (Set.fromList [TypeIndex KType 1] :: Set (TypeIndex (Kind Int)))

fixture_1 :: Scheme TypeIndex () (Type TypeIndex ())
fixture_1 =
  Forall
    (Set.fromList [TypeIndex () 0])
    []
    (TVariable (TypeIndex () 3) `TArrow` TVariable (TypeIndex () 0) `TArrow` TVariable (TypeIndex () 0))

fixture_2 :: Scheme TypeIndex () (Type TypeIndex ())
fixture_2 =
  Forall
    (Set.fromList [])
    []
    (TVariable (TypeIndex () 3) `TArrow` TVariable (TypeIndex () 0) `TArrow` TVariable (TypeIndex () 0))

fixture_3 :: Scheme TypeIndex () (Type TypeIndex ())
fixture_3 =
  Forall
    (Set.fromList [TypeIndex () 0])
    []
    (TVariable (TypeIndex () 0) `TArrow` TVariable (TypeIndex () 0) `TArrow` TVariable (TypeIndex () 0))

fixture_4 :: Scheme TypeIndex (Kind Int) (Type TypeIndex (Kind Int))
fixture_4 =
  Forall
    (Set.fromList [TypeIndex KType 0])
    []
    (TVariable (TypeIndex KRow 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KTrait 0))

fixture_5 :: Scheme TypeIndex (Kind Int) (Type TypeIndex (Kind Int))
fixture_5 =
  Forall
    (Set.fromList [TypeIndex KType 0])
    []
    (TVariable (TypeIndex KRow 0) `TArrow` TVariable (TypeIndex KType 1) `TArrow` TVariable (TypeIndex KTrait 0))
