{-# LANGUAGE OverloadedStrings #-}

module Noll.Compiler2Spec where

import Control.Monad.Identity (runIdentity)
import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Noll.Compiler2
import Noll.Compiler2.Internal

-- import Noll.Compiler2Examples.Test02 (bazz)
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

compiler2TestEnvironment =
  Compiler2Environment
    { compiler2DataConstructorEnv = env1
    , compiler2TypeConstructorEnv = env2
    , compiler2TraitEnvironment = mempty
    , compiler2TraitEnv = env3
    , compiler2AliasEnv = mempty
    }

env1 =
        Environment.fromList
            [
              ( "LessThan"
              , Constructor
                  "LessThan"
                  0
                  (Forall mempty [] (TConstructor KType "Ordering"))
              )
            ,
              ( "GreaterThan"
              , Constructor
                  "GreaterThan"
                  0
                  (Forall mempty [] (TConstructor KType "Ordering"))
              )
            ,
              ( "EqualTo"
              , Constructor
                  "EqualTo"
                  0
                  (Forall mempty [] (TConstructor KType "Ordering"))
              )
            ,
              ( "Node"
              , Constructor
                  "Node"
                  3
                  ( Forall
                      (Set.fromList [TypeIndex KType 0])
                      []
                      ( tvariable0
                          `TArrow` tree0
                          `TArrow` tree0
                          `TArrow` tree0
                      )
                  )
              )
            ,
              ( "Leaf"
              , Constructor
                  "Leaf"
                  0
                  ( Forall
                      (Set.fromList [TypeIndex KType 0])
                      []
                      tree0
                  )
              )
            ,
              ( "Succ"
              , Constructor
                  "Succ"
                  1
                  (Forall mempty [] (TIntrinsic INat `TArrow` TIntrinsic INat))
              )
            ,
              ( "Zero"
              , Constructor
                  "Zero"
                  0
                  (Forall mempty [] (TIntrinsic INat))
              )
            ]

env2 =
        Environment.fromList
            [
              ( "Tree"
              , KArrow KType KType
              )
            ]

env3 = 
        Environment.fromList
            [
              ( "Numeric"
              ,
                ( TypeIndex KType 0
                , Environment.fromList
                    [
                      ( "from_int32"
                      , Forall
                          (Set.fromList [TypeIndex KType 0])
                          []
                          ( TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 0)
                          )
                      )
                    ]
                )
              )
            ,
              ( "Ordered"
              ,
                ( TypeIndex KType 0
                , Environment.fromList
                    [
                      ( "compare"
                      , Forall
                          (Set.fromList [TypeIndex KType 0])
                          []
                          ( TVariable (TypeIndex KType 0)
                              `TArrow` TVariable (TypeIndex KType 0)
                              `TArrow` TConstructor KType "Ordering"
                          )
                      )
                    ]
                )
              )
            ]

tree0 :: IndexedType
tree0 =
  TApplication
    KType
    (TConstructor (KArrow KType KType) "Tree")
    (TVariable (TypeIndex KType 0) :| [])

tvariable0 :: IndexedType
tvariable0 = TVariable (TypeIndex KType 0)

tvariable1 :: IndexedType
tvariable1 = TVariable (TypeIndex KType 1)

bool :: IndexedType
bool = TIntrinsic IBool

