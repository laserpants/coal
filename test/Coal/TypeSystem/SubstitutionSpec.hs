{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.TypeSystem.SubstitutionSpec (substitutionSpec) where

import Coal.Language
import Coal.TypeSystem.Substitution
import Control.Monad (forM_)
import qualified Data.List.NonEmpty as NonEmpty
import Test.Hspec

data SubstitutionSpecTestCase = SubstitutionSpecTestCase
  { substitution :: Substitution
  , input :: IndexedType
  , expected :: IndexedType
  }

substitutionTests :: [SubstitutionSpecTestCase]
substitutionTests =
  [ -- [0 ↦ int32] applied to '0
    SubstitutionSpecTestCase
      (mapsTo 0 (TIntrinsic IInt32))
      (TVariable (TypeIndex KType 0))
      (TIntrinsic IInt32)
  , -- [0 ↦ int32] applied to '1 (no effect)
    SubstitutionSpecTestCase
      (mapsTo 0 (TIntrinsic IInt32))
      (TVariable (TypeIndex KType 1))
      (TVariable (TypeIndex KType 1))
  , -- [0 ↦ List<'1>] applied to '0
    SubstitutionSpecTestCase
      ( mapsTo
          0
          ( applyTypeArgs
              KType
              (TConstructor (KArrow KType KType) "List")
              (NonEmpty.singleton (TVariable (TypeIndex KType 1)))
          )
      )
      (TVariable (TypeIndex KType 0))
      ( applyTypeArgs
          KType
          (TConstructor (KArrow KType KType) "List")
          (NonEmpty.singleton (TVariable (TypeIndex KType 1)))
      )
  , -- [0 ↦ int32] applied to List<'0>
    SubstitutionSpecTestCase
      (mapsTo 0 (TIntrinsic IInt32))
      ( applyTypeArgs
          KType
          (TConstructor (KArrow KType KType) "List")
          (NonEmpty.singleton (TVariable (TypeIndex KType 0)))
      )
      ( applyTypeArgs
          KType
          (TConstructor (KArrow KType KType) "List")
          (NonEmpty.singleton (TIntrinsic IInt32))
      )
  , -- row variable substitution
    SubstitutionSpecTestCase
      (mapsTo 0 (TRow (RExtend "x" (TIntrinsic IInt32) RNil)))
      (TRow (RVariable (TypeIndex KRow 0)))
      (TRow (RExtend "x" (TIntrinsic IInt32) RNil))
  , -- row extension substitution
    SubstitutionSpecTestCase
      (mapsTo 0 (TIntrinsic IInt32))
      (TRow (RExtend "x" (TVariable (TypeIndex KType 0)) (RNil)))
      (TRow (RExtend "x" (TIntrinsic IInt32) RNil))
  ]

mergeTests :: [(Substitution, Substitution, Maybe Substitution)]
mergeTests =
  [ -- disjoint domains, merges fine

    ( mapsTo 0 (TIntrinsic IInt32)
    , mapsTo 1 (TIntrinsic IBool)
    , Just (fromList [(0, TIntrinsic IInt32), (1, TIntrinsic IBool)])
    )
  , -- same mapping, merges fine

    ( mapsTo 0 (TIntrinsic IInt32)
    , mapsTo 0 (TIntrinsic IInt32)
    , Just (mapsTo 0 (TIntrinsic IInt32))
    )
  , -- conflicting mapping, fails

    ( mapsTo 0 (TIntrinsic IInt32)
    , mapsTo 0 (TIntrinsic IBool)
    , Nothing
    )
  ]

normalizeTests :: [(IndexedType, IndexedType)]
normalizeTests =
  [
    ( TVariable (TypeIndex KType 42)
    , TVariable (TypeIndex KType 0)
    )
  ,
    ( applyTypeArgs
        KType
        (TConstructor (KArrow KType KType) "List")
        (NonEmpty.singleton (TVariable (TypeIndex KType 5)))
    , applyTypeArgs
        KType
        (TConstructor (KArrow KType KType) "List")
        (NonEmpty.singleton (TVariable (TypeIndex KType 0)))
    )
  ]

substitutionSpec :: Spec
substitutionSpec =
  describe "Substitution tests" $ do
    describe "apply" $ do
      forM_ substitutionTests $ \(SubstitutionSpecTestCase sub inp expct) ->
        it (show inp <> " under " <> show sub) $
          apply sub inp `shouldBe` expct
    describe "merge" $ do
      forM_ mergeTests $ \(s1, s2, expected) ->
        it (show s1 <> " <> " <> show s2) $
          merge s1 s2 `shouldBe` expected
    describe "normalizeTypeIndexes" $ do
      forM_ normalizeTests $ \(inp, expected) ->
        it (show inp) $
          normalizeTypeIndexes inp `shouldBe` expected
    describe "Row substitution" $ do
      it "substitutes a row variable with a concrete row" $ do
        let rVar = RVariable (TypeIndex KRow 0)
            sub = fromList [(0, TRow (RExtend "x" (TIntrinsic IInt32) RNil))]
            expected = RExtend "x" (TIntrinsic IInt32) RNil :: Row TypeIndex Kind IndexedType
        apply sub rVar `shouldBe` expected
      it "substitutes a row variable with a type variable" $ do
        let rVar = RVariable (TypeIndex KRow 0)
            sub = fromList [(0, TVariable (TypeIndex KRow 1))]
            expected = RVariable (TypeIndex KRow 1) :: Row TypeIndex Kind IndexedType
        apply sub rVar `shouldBe` expected
      it "leaves a row variable unchanged if no substitution applies" $ do
        let rVar = RVariable (TypeIndex KRow 0) :: Row TypeIndex Kind IndexedType
            sub = mempty
        apply sub rVar `shouldBe` rVar
      it "applies substitution inside RExtend field type" $ do
        let row = RExtend "x" (TVariable (TypeIndex KType 0)) (RVariable (TypeIndex KRow 1))
            sub =
              fromList
                [ (0, TIntrinsic IInt32)
                , (1, TRow RNil)
                ]
            expected = RExtend "x" (TIntrinsic IInt32) RNil :: Row TypeIndex Kind IndexedType
        apply sub row `shouldBe` expected
      it "applies substitution recursively in nested RExtend rows" $ do
        let row =
              RExtend
                "x"
                (TIntrinsic IInt32)
                (RExtend "y" (TVariable (TypeIndex KType 0)) (RVariable (TypeIndex KRow 1)))
            sub =
              fromList
                [ (0, TIntrinsic IBool)
                , (1, TRow RNil)
                ]
            expected = RExtend "x" (TIntrinsic IInt32) (RExtend "y" (TIntrinsic IBool) RNil) :: Row TypeIndex Kind IndexedType
        apply sub row `shouldBe` expected
      it "leaves RExtend row unchanged if substitution is empty" $ do
        let row = RExtend "x" (TVariable (TypeIndex KType 0)) (RVariable (TypeIndex KRow 1))
            sub = mempty
        apply sub row `shouldBe` row
