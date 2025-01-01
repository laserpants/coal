{-# LANGUAGE OverloadedStrings #-}

module Noll.Language.IndexedSpec (spec) where

import Data.Set (Set)
import qualified Data.Set as Set
import Noll.Language.Indexed
import Noll.Language.Type (Type (..), TypeIndex (..))
import Noll.Language.Type.Kind (Kind (..))
import Noll.Language.Type.Scheme (Scheme (..))
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "Noll.Language.Indexed" $ do
    describe "typeIndexesIn" $ do
      describe "Scheme" $ do
        it "" $
          typeIndexesIn fixture1 == (Set.fromList [TypeIndex () 3] :: Set (TypeIndex ()))
        it "" $
          typeIndexesIn fixture2 == (Set.fromList [TypeIndex () 0, TypeIndex () 3] :: Set (TypeIndex ()))
        it "" $
          typeIndexesIn fixture3 == (mempty :: Set (TypeIndex ()))
        it "" $
          typeIndexesIn fixture4 == (mempty :: Set (TypeIndex (Kind Int)))
        it "" $
          typeIndexesIn fixture5 == (Set.fromList [TypeIndex KType 1] :: Set (TypeIndex (Kind Int)))

fixture1 :: Scheme TypeIndex () (Type TypeIndex ())
fixture1 =
  Forall
    (Set.fromList [TypeIndex () 0])
    []
    (TVariable (TypeIndex () 3) `TArrow` TVariable (TypeIndex () 0) `TArrow` TVariable (TypeIndex () 0))

fixture2 :: Scheme TypeIndex () (Type TypeIndex ())
fixture2 =
  Forall
    (Set.fromList [])
    []
    (TVariable (TypeIndex () 3) `TArrow` TVariable (TypeIndex () 0) `TArrow` TVariable (TypeIndex () 0))

fixture3 :: Scheme TypeIndex () (Type TypeIndex ())
fixture3 =
  Forall
    (Set.fromList [TypeIndex () 0])
    []
    (TVariable (TypeIndex () 0) `TArrow` TVariable (TypeIndex () 0) `TArrow` TVariable (TypeIndex () 0))

fixture4 :: Scheme TypeIndex (Kind Int) (Type TypeIndex (Kind Int))
fixture4 =
  Forall
    (Set.fromList [TypeIndex KType 0])
    []
    (TVariable (TypeIndex KRow 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KTrait 0))

fixture5 :: Scheme TypeIndex (Kind Int) (Type TypeIndex (Kind Int))
fixture5 =
  Forall
    (Set.fromList [TypeIndex KType 0])
    []
    (TVariable (TypeIndex KRow 0) `TArrow` TVariable (TypeIndex KType 1) `TArrow` TVariable (TypeIndex KTrait 0))
