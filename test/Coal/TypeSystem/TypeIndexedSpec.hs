{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.TypeSystem.TypeIndexedSpec (typeIndexedSpec, typeIndexedTrickySpec) where

import Coal.Language
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Set (Set)
import qualified Data.Set as Set
import Test.Hspec

typeIndexedSpec :: Spec
typeIndexedSpec =
  describe "TypeIndexed.typeIndexesIn" $ do
    it "returns singleton for a single TypeIndex" $ do
      let idx = TypeIndex KType 123
      typeIndexesIn idx `shouldBe` Set.singleton idx
      typeIdsIn idx `shouldBe` Set.singleton 123

    it "returns union for a list of TypeIndexes" $ do
      let xs = [TypeIndex KType 1, TypeIndex KType 2]
      typeIndexesIn xs `shouldBe` Set.fromList xs
      typeIdsIn xs `shouldBe` Set.fromList [1, 2]

    it "returns union for Maybe" $ do
      let x = Just (TypeIndex KType 42)
      typeIndexesIn x `shouldBe` Set.singleton (TypeIndex KType 42)
      typeIndexesIn (Nothing :: Maybe (TypeIndex Kind)) `shouldBe` (Set.empty :: Set (TypeIndex Kind))

    it "returns union for NonEmpty" $ do
      let xs = TypeIndex KType 3 :| [TypeIndex KType 4]
      typeIndexesIn xs `shouldBe` Set.fromList [TypeIndex KType 3, TypeIndex KType 4]

    it "returns union for Set" $ do
      let s = Set.fromList [TypeIndex KType 5, TypeIndex KType 6]
      typeIndexesIn s `shouldBe` s

    it "finds indexes in a simple Type" $ do
      let t = TVariable (TypeIndex KType 7) ~> TIntrinsic IInt32
      typeIndexesIn t `shouldBe` Set.singleton (TypeIndex KType 7)

    it "finds indexes in nested rows" $ do
      let row = RExtend "x" (TVariable (TypeIndex KType 8)) RNil
      typeIndexesIn (TRow row) `shouldBe` Set.singleton (TypeIndex KType 8)

    it "finds indexes in schemes, ignoring bound variables" $ do
      let body = TVariable (TypeIndex KType 9) ~> TVariable (TypeIndex KType 10)
          scheme_ = Forall (Set.singleton (TypeIndex KType 9)) mempty body
      typeIndexesIn scheme_ `shouldBe` Set.singleton (TypeIndex KType 10)
      typeIdsIn scheme_ `shouldBe` Set.singleton 10

    it "returns empty set for an empty row" $ do
      typeIndexesIn (TRow RNil :: IndexedType) `shouldBe` (Set.empty :: Set (TypeIndex Kind))

--    it "finds indexes in patterns and expressions" $ do
--      let pat = EVariable () (Label (TVariable (TypeIndex KType 11)) "x")
--      typeIndexesIn pat `shouldBe` Set.singleton (TypeIndex KType 11)

typeIndexedTrickySpec :: Spec
typeIndexedTrickySpec =
  describe "TypeIndexed.typeIndexesIn (tricky / recursive cases)" $ do
    it "finds indexes in nested RExtend rows with row variables" $ do
      let row =
            RExtend
              "x"
              (TVariable (TypeIndex KType 12))
              ( RExtend
                  "y"
                  (TVariable (TypeIndex KType 13))
                  (RVariable (TypeIndex KRow 14))
              )
      typeIndexesIn (TRow row)
        `shouldBe` Set.fromList
          [ TypeIndex KType 12
          , TypeIndex KType 13
          , TypeIndex KRow 14
          ]

    it "finds indexes in nested applyTypeArgs types" $ do
      let t =
            applyTypeArgs
              KType
              (TConstructor (KArrow KType KType) "List")
              ( NonEmpty.fromList
                  [ TVariable (TypeIndex KType 15)
                  , applyTypeArgs
                      KType
                      (TConstructor KType "Option")
                      (NonEmpty.singleton (TVariable (TypeIndex KType 16)))
                  ]
              )
      typeIndexesIn t
        `shouldBe` Set.fromList
          [ TypeIndex KType 15
          , TypeIndex KType 16
          ]

    it "finds indexes in alias types" $ do
      let alias =
            TAlias
              "Pair"
              [TVariable (TypeIndex KType 17), TVariable (TypeIndex KType 18)]
              (TArrow (TVariable (TypeIndex KType 17)) (TVariable (TypeIndex KType 18)))
      typeIndexesIn alias
        `shouldBe` Set.fromList
          [ TypeIndex KType 17
          , TypeIndex KType 18
          ]

    it "finds all indexes in a scheme with multiple quantified variables" $ do
      let body = TArrow (TVariable (TypeIndex KType 19)) (TVariable (TypeIndex KType 20))
          scheme_ = Forall (Set.fromList [TypeIndex KType 19, TypeIndex KType 21]) mempty body
      -- 19 and 21 are bound, so only 20 should remain
      typeIndexesIn scheme_ `shouldBe` Set.singleton (TypeIndex KType 20)

    it "finds indexes in a deeply nested row inside a type application" $ do
      let row =
            RExtend
              "a"
              (TVariable (TypeIndex KType 22))
              (RExtend "b" (TVariable (TypeIndex KType 23)) RNil)
          t =
            applyTypeArgs
              KType
              (TConstructor KType "Map")
              (NonEmpty.fromList [TRow row])
      typeIndexesIn t
        `shouldBe` Set.fromList
          [ TypeIndex KType 22
          , TypeIndex KType 23
          ]

--    it "finds indexes in pattern containing nested labeled types" $ do
--      let pat = EVariable () (Label (TVariable (TypeIndex KType 24)) "foo")
--      typeIndexesIn pat `shouldBe` Set.singleton (TypeIndex KType 24)
