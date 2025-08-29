{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.TypeSystem.SubstitutionSpec where

import Coal.Common.List1
import Coal.Language
import Coal.TypeSystem.Substitution
import Coal.TypeSystem.Unification
import Control.Monad (forM_)
import Prettyprinter
import Prettyprinter.Render.String (renderString)
import Test.Hspec

import qualified Coal.Common.List1 as List1
import qualified Data.Set as Set

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
          ( TApplication
              KType
              (TConstructor KType "List")
              (List1.singleton (TVariable (TypeIndex KType 1)))
          )
      )
      (TVariable (TypeIndex KType 0))
      ( TApplication
          KType
          (TConstructor KType "List")
          (List1.singleton (TVariable (TypeIndex KType 1)))
      )
  , -- [0 ↦ int32] applied to List<'0>
    SubstitutionSpecTestCase
      (mapsTo 0 (TIntrinsic IInt32))
      ( TApplication
          KType
          (TConstructor KType "List")
          (List1.singleton (TVariable (TypeIndex KType 0)))
      )
      ( TApplication
          KType
          (TConstructor KType "List")
          (List1.singleton (TIntrinsic IInt32))
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
    ( TApplication
        KType
        (TConstructor KType "List")
        (List1.singleton (TVariable (TypeIndex KType 5)))
    , TApplication
        KType
        (TConstructor KType "List")
        (List1.singleton (TVariable (TypeIndex KType 0)))
    )
  ]

-- schemeTests :: [(Substitution, IndexedScheme, IndexedScheme)]
-- schemeTests =
--  [ -- [0 ↦ int32] inside a simple scheme
--    ( mapsTo 0 (TIntrinsic IInt32)
--    , Forall mempty mempty (TVariable (TypeIndex KType 0))
--    , Forall mempty mempty (TIntrinsic IInt32)
--    )
--  , -- quantified var is protected
--    ( mapsTo 0 (TIntrinsic IInt32)
--    , Forall (Set.singleton (TypeIndex KType 0)) mempty (TVariable (TypeIndex KType 0))
--    , Forall (Set.singleton (TypeIndex KType 0)) mempty (TVariable (TypeIndex KType 0))
--    )
--  ]

substitutionSpec :: Spec
substitutionSpec =
  describe "Substitution tests" $ do
    describe "apply" $ do
      forM_ substitutionTests $ \(SubstitutionSpecTestCase sub inp expct) ->
        it (show inp ++ " under " ++ show sub) $
          apply sub inp `shouldBe` expct

    describe "merge" $ do
      forM_ mergeTests $ \(s1, s2, expected) ->
        it (show s1 ++ " <> " ++ show s2) $
          merge s1 s2 `shouldBe` expected

    describe "normalizeTypeIndexes" $ do
      forM_ normalizeTests $ \(inp, expected) ->
        it (show inp) $
          normalizeTypeIndexes inp `shouldBe` expected

--    describe "substituteInScheme" $ do
--      forM_ schemeTests $ \(sub, sch, expected) ->
--        it (show sch ++ " under " ++ show sub) $
--          substituteInScheme sub sch `shouldBe` expected
