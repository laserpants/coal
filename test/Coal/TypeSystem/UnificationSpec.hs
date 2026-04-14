{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.TypeSystem.UnificationSpec (unificationSpec) where

import Coal.Language
import Coal.TypeSystem.Substitution
import Coal.TypeSystem.Unification
import Control.Monad (forM_)
import Data.List.NonEmpty (NonEmpty (..))
import Prettyprinter
import Prettyprinter.Render.String (renderString)
import Test.Hspec

import qualified Coal.TypeSystem.Substitution as Substitution
import qualified Data.List.NonEmpty as NonEmpty

data UnificationSpecTestCase t
  = UnifyTestCase t t (Either UnificationError Substitution)
  | MatchTestCase t t (Either UnificationError Substitution)
  deriving (Show, Eq, Ord)

testCase :: UnificationSpecTestCase IndexedType -> Either UnificationError Substitution
testCase (UnifyTestCase t1 t2 _) = evalUnifier (freshIdIn [t1, t2]) (unify t1 t2)
testCase (MatchTestCase t1 t2 _) = evalUnifier (freshIdIn [t1, t2]) (match t1 t2)

unifyTestCases :: [UnificationSpecTestCase IndexedType]
unifyTestCases =
  [ -- '0 ~ int32
    -- Substitution [ 0 :=> int32 ]
    UnifyTestCase
      (TVariable (TypeIndex KType 0))
      (TIntrinsic IInt32)
      ( Right $
          Substitution.fromList
            [
              ( 0
              , TIntrinsic IInt32
              )
            ]
      )
  , -- '0 ~ '0
    -- Substitution []
    UnifyTestCase
      (TVariable (TypeIndex KType 0))
      (TVariable (TypeIndex KType 0))
      (Right mempty)
  , -- int32 ~ int32
    -- Substitution []
    UnifyTestCase
      (TIntrinsic IInt32)
      (TIntrinsic IInt32)
      (Right mempty)
  , -- '0 -> '1 ~ int32 -> int32
    -- Substitution [ 0 :=> int32, 1 :=> int32 ]
    UnifyTestCase
      (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 1))
      (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32)
      ( Right $
          Substitution.fromList
            [
              ( 0
              , TIntrinsic IInt32
              )
            ,
              ( 1
              , TIntrinsic IInt32
              )
            ]
      )
  , -- int32 -> int32 ~ int32 -> int32
    -- Substitution []
    UnifyTestCase
      (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32)
      (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32)
      (Right mempty)
  , -- bool -> int32 ~ int32 -> int32
    -- ECannotUnify
    UnifyTestCase
      (TIntrinsic IBool `TArrow` TIntrinsic IInt32)
      (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32)
      (Left ECannotUnify)
  , -- bool ~ int32
    -- ECannotUnify
    UnifyTestCase
      (TIntrinsic IBool)
      (TIntrinsic IInt32)
      (Left ECannotUnify)
  , -- { | '0 } ~ { id : int32, name : string }
    -- Substitution [ 0 :=> { id : int32, name : string } ]
    UnifyTestCase
      (TRow (RVariable (TypeIndex KRow 0)))
      (TRow (RExtend "id" (TIntrinsic IInt32) (RExtend "name" (TIntrinsic IString) RNil)))
      ( Right $
          Substitution.fromList
            [
              ( 0
              , TRow (RExtend "id" (TIntrinsic IInt32) (RExtend "name" (TIntrinsic IString) RNil))
              )
            ]
      )
  , -- { name : '0 | '1 } ~ { id : int32, name : string }
    -- Substitution [ 0 :=> string, 1 :=> { id : int32 } ]
    UnifyTestCase
      (TRow (RExtend "name" (TVariable (TypeIndex KType 0)) (RVariable (TypeIndex KRow 1))))
      (TRow (RExtend "id" (TIntrinsic IInt32) (RExtend "name" (TIntrinsic IString) RNil)))
      ( Right $
          Substitution.fromList
            [
              ( 0
              , TIntrinsic IString
              )
            ,
              ( 1
              , TRow (RExtend "id" (TIntrinsic IInt32) RNil)
              )
            ]
      )
  , -- { name : string, id : int32 } ~ { id : int32, name : string }
    -- Substitution []
    UnifyTestCase
      (TRow (RExtend "name" (TIntrinsic IString) (RExtend "id" (TIntrinsic IInt32) RNil)))
      (TRow (RExtend "id" (TIntrinsic IInt32) (RExtend "name" (TIntrinsic IString) RNil)))
      (Right mempty)
  , -- { name : string, id : int32 } ~ { name : int32, id : int32 }
    -- ECannotUnify
    UnifyTestCase
      (TRow (RExtend "name" (TIntrinsic IString) (RExtend "id" (TIntrinsic IInt32) RNil)))
      (TRow (RExtend "name" (TIntrinsic IInt32) (RExtend "id" (TIntrinsic IInt32) RNil)))
      (Left ECannotUnify)
  , -- { name : '0 | '1 } ~ { name : string }
    -- [ 0 :=> string, 1 :=> {} ]
    UnifyTestCase
      (TRow (RExtend "name" (TVariable (TypeIndex KType 0)) (RVariable (TypeIndex KRow 1))))
      (TRow (RExtend "name" (TIntrinsic IString) RNil))
      ( Right $
          Substitution.fromList
            [
              ( 0
              , TIntrinsic IString
              )
            ,
              ( 1
              , TRow RNil
              )
            ]
      )
  , -- '0:Type ~ '0:Row
    -- EKindMismatch
    UnifyTestCase
      (TVariable (TypeIndex KType 0))
      (TVariable (TypeIndex KRow 0))
      (Left EKindMismatch)
  , -- '0 ~ '0 -> int32
    -- EInfiniteType
    UnifyTestCase
      (TVariable (TypeIndex KType 0))
      (TArrow (TVariable (TypeIndex KType 0)) (TIntrinsic IInt32))
      (Left EInfiniteType)
  , -- '0:Row ~ { x : int32 | '0 }
    -- EInfiniteType
    UnifyTestCase
      (TVariable (TypeIndex KRow 0))
      (TRow (RExtend "x" (TIntrinsic IInt32) (RVariable (TypeIndex KRow 0))))
      (Left EInfiniteType)
  , -- string ~ int32
    -- ECannotUnify
    UnifyTestCase
      (TIntrinsic IString)
      (TIntrinsic IInt32)
      (Left ECannotUnify)
  , -- record { x : int32 } ~ record { x : int32 }
    -- Substitution []
    UnifyTestCase
      (TRecord (TRow (RExtend "x" (TIntrinsic IInt32) RNil)))
      (TRecord (TRow (RExtend "x" (TIntrinsic IInt32) RNil)))
      (Right mempty)
  , -- int32 -> string ~ int32 -> int32
    -- ECannotUnify
    UnifyTestCase
      (TArrow (TIntrinsic IInt32) (TIntrinsic IString))
      (TArrow (TIntrinsic IInt32) (TIntrinsic IInt32))
      (Left ECannotUnify)
  , -- '0 -> ('1 -> int32) ~ bool -> (string -> int32)
    -- Substitution [ 0 :=> bool, 1 :=> string ]
    UnifyTestCase
      ( TArrow
          (TVariable (TypeIndex KType 0))
          (TArrow (TVariable (TypeIndex KType 1)) (TIntrinsic IInt32))
      )
      ( TArrow
          (TIntrinsic IBool)
          (TArrow (TIntrinsic IString) (TIntrinsic IInt32))
      )
      ( Right $
          Substitution.fromList
            [ (0, TIntrinsic IBool)
            , (1, TIntrinsic IString)
            ]
      )
  , -- { x : int32, y : string } ~ { y : string, x : int32 }
    -- Substitution []
    UnifyTestCase
      ( TRow
          ( RExtend
              "x"
              (TIntrinsic IInt32)
              (RExtend "y" (TIntrinsic IString) RNil)
          )
      )
      ( TRow
          ( RExtend
              "y"
              (TIntrinsic IString)
              (RExtend "x" (TIntrinsic IInt32) RNil)
          )
      )
      (Right mempty)
  , -- { x : int32 | '0 } ~ { x : int32, y : string }
    -- Substitution [ 0 :=> { y : string } ]
    UnifyTestCase
      (TRow (RExtend "x" (TIntrinsic IInt32) (RVariable (TypeIndex KRow 0))))
      ( TRow
          ( RExtend
              "x"
              (TIntrinsic IInt32)
              (RExtend "y" (TIntrinsic IString) RNil)
          )
      )
      ( Right $
          Substitution.fromList
            [(0, TRow (RExtend "y" (TIntrinsic IString) RNil))]
      )
  , -- { x : int32 } ~ { x : string }
    -- ECannotUnify
    UnifyTestCase
      (TRow (RExtend "x" (TIntrinsic IInt32) RNil))
      (TRow (RExtend "x" (TIntrinsic IString) RNil))
      (Left ECannotUnify)
  , -- List '0 ~ int32
    -- ECannotUnify
    UnifyTestCase
      ( applyTypeArgs
          KType
          (TConstructor (KArrow KType KType) "List")
          (NonEmpty.singleton (TVariable (TypeIndex KType 0)))
      )
      (TIntrinsic IInt32)
      (Left ECannotUnify)
  , -- type alias T a = a
    -- T int32 ~ int32
    UnifyTestCase
      (TAlias "T" [TIntrinsic IInt32] (TIntrinsic IInt32))
      (TIntrinsic IInt32)
      (Right mempty)
  , -- type alias T a = a
    -- T int32 ~ string
    -- ECannotUnify
    UnifyTestCase
      (TAlias "T" [TIntrinsic IInt32] (TIntrinsic IInt32))
      (TIntrinsic IString)
      (Left ECannotUnify)
  , -- type alias Id a = a
    -- Id '0 ~ int32
    -- Substitution [ 0 :=> int32 ]
    UnifyTestCase
      (TAlias "Id" [TVariable (TypeIndex KType 0)] (TVariable (TypeIndex KType 0)))
      (TIntrinsic IInt32)
      ( Right $
          Substitution.fromList
            [(0, TIntrinsic IInt32)]
      )
  , -- type alias Pair a b = (a -> b)
    -- Pair '0 '1 ~ (int32 -> string)
    -- Substitution [ 0 :=> int32, 1 :=> string ]
    UnifyTestCase
      ( TAlias
          "Pair"
          [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)]
          (TArrow (TVariable (TypeIndex KType 0)) (TVariable (TypeIndex KType 1)))
      )
      (TArrow (TIntrinsic IInt32) (TIntrinsic IString))
      ( Right $
          Substitution.fromList
            [ (0, TIntrinsic IInt32)
            , (1, TIntrinsic IString)
            ]
      )
  , -- '0 ~ '1, '1 ~ int32
    -- Substitution [ 0 :=> int32, 1 :=> int32 ]
    UnifyTestCase
      (TVariable (TypeIndex KType 0))
      (TVariable (TypeIndex KType 1))
      ( Right $
          Substitution.fromList
            [(0, TVariable (TypeIndex KType 1))]
      )
  , -- '0 ~ '1 -> '1, '1 ~ int32
    -- Substitution [ 1 :=> int32, 0 :=> (int32 -> int32) ]
    UnifyTestCase
      (TVariable (TypeIndex KType 0))
      (TArrow (TVariable (TypeIndex KType 1)) (TVariable (TypeIndex KType 1)))
      ( Right $
          Substitution.fromList
            [(0, TArrow (TVariable (TypeIndex KType 1)) (TVariable (TypeIndex KType 1)))]
      )
  , UnifyTestCase
      ( (TVariable (TypeIndex KType 312) `TArrow` TVariable (TypeIndex KType 311))
          `TArrow` applyTypeArgs KType (TVariable (TypeIndex (KArrow KType KType) 310)) (TVariable (TypeIndex KType 312) :| [])
          `TArrow` applyTypeArgs KType (TVariable (TypeIndex (KArrow KType KType) 310)) (TVariable (TypeIndex KType 311) :| [])
      )
      ( (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32)
          `TArrow` applyTypeArgs KType (TConstructor (KArrow KType KType) "Option") (TIntrinsic IInt32 :| [])
          `TArrow` applyTypeArgs KType (TConstructor (KArrow KType KType) "Option") (TIntrinsic IInt32 :| [])
      )
      ( Right $
          Substitution.fromList
            [ (310, TConstructor (KArrow KType KType) "Option")
            , (311, TIntrinsic IInt32)
            , (312, TIntrinsic IInt32)
            ]
      )
  ]

matchTestCases :: [UnificationSpecTestCase IndexedType]
matchTestCases =
  [ -- int32 >~ int32
    -- Substitution []
    MatchTestCase
      (TIntrinsic IInt32)
      (TIntrinsic IInt32)
      (Right mempty)
  , -- int32 >~ List<'0>
    -- Substitution []
    MatchTestCase
      (TIntrinsic IInt32)
      ( applyTypeArgs
          KType
          (TConstructor (KArrow KType KType) "List")
          (NonEmpty.singleton (TVariable (TypeIndex KType 0)))
      )
      (Left ECannotMatch)
  , -- '0 >~ int32
    -- Substitution [ 0 :=> int32 ]
    MatchTestCase
      (TVariable (TypeIndex KType 0))
      (TIntrinsic IInt32)
      ( Right $
          Substitution.fromList
            [ (0, TIntrinsic IInt32)
            ]
      )
  , -- int32 >~ '0
    -- ECannotMatch
    MatchTestCase
      (TIntrinsic IInt32)
      (TVariable (TypeIndex KType 0))
      (Left ECannotMatch)
  , -- '0 -> '1 >~ int32 -> int32
    -- Substitution [ 0 :=> int32, 1 :=> int32 ]
    MatchTestCase
      (TArrow (TVariable (TypeIndex KType 0)) (TVariable (TypeIndex KType 1)))
      (TArrow (TIntrinsic IInt32) (TIntrinsic IInt32))
      ( Right $
          Substitution.fromList
            [ (0, TIntrinsic IInt32)
            , (1, TIntrinsic IInt32)
            ]
      )
  , -- int32 -> int32 >~ int32 -> int32
    -- Substitution []
    MatchTestCase
      (TArrow (TIntrinsic IInt32) (TIntrinsic IInt32))
      (TArrow (TIntrinsic IInt32) (TIntrinsic IInt32))
      (Right mempty)
  , -- bool -> int32 >~ int32 -> int32
    -- ECannotMatch
    MatchTestCase
      (TArrow (TIntrinsic IBool) (TIntrinsic IInt32))
      (TArrow (TIntrinsic IInt32) (TIntrinsic IInt32))
      (Left ECannotMatch)
  , -- { | '0 } >~ { id : int32, name : string }
    -- Substitution [ 0 :=> { id : int32, name : string } ]
    MatchTestCase
      (TRow (RVariable (TypeIndex KRow 0)))
      ( TRow
          ( RExtend
              "id"
              (TIntrinsic IInt32)
              (RExtend "name" (TIntrinsic IString) RNil)
          )
      )
      ( Right $
          Substitution.fromList
            [
              ( 0
              , TRow
                  ( RExtend
                      "id"
                      (TIntrinsic IInt32)
                      (RExtend "name" (TIntrinsic IString) RNil)
                  )
              )
            ]
      )
  , -- { name : '0 | '1 } >~ { id : int32, name : string }
    -- Substitution [ 0 :=> string, 1 :=> { id : int32 } ]
    MatchTestCase
      ( TRow
          ( RExtend
              "name"
              (TVariable (TypeIndex KType 0))
              (RVariable (TypeIndex KRow 1))
          )
      )
      ( TRow
          ( RExtend
              "id"
              (TIntrinsic IInt32)
              (RExtend "name" (TIntrinsic IString) RNil)
          )
      )
      ( Right $
          Substitution.fromList
            [ (0, TIntrinsic IString)
            , (1, TRow (RExtend "id" (TIntrinsic IInt32) RNil))
            ]
      )
  , -- { name : string, id : int32 } >~ { id : int32, name : string }
    -- Substitution []
    MatchTestCase
      ( TRow
          ( RExtend
              "name"
              (TIntrinsic IString)
              (RExtend "id" (TIntrinsic IInt32) RNil)
          )
      )
      ( TRow
          ( RExtend
              "id"
              (TIntrinsic IInt32)
              (RExtend "name" (TIntrinsic IString) RNil)
          )
      )
      (Right mempty)
  , -- { name : string, id : int32 } >~ { name : int32, id : int32 }
    -- ECannotMatch
    MatchTestCase
      ( TRow
          ( RExtend
              "name"
              (TIntrinsic IString)
              (RExtend "id" (TIntrinsic IInt32) RNil)
          )
      )
      ( TRow
          ( RExtend
              "name"
              (TIntrinsic IInt32)
              (RExtend "id" (TIntrinsic IInt32) RNil)
          )
      )
      (Left ECannotMatch)
  , -- { name : '0 | '1 } >~ { name : string }
    -- Substitution [ 0 :=> string, 1 :=> {} ]
    MatchTestCase
      ( TRow
          ( RExtend
              "name"
              (TVariable (TypeIndex KType 0))
              (RVariable (TypeIndex KRow 1))
          )
      )
      (TRow (RExtend "name" (TIntrinsic IString) RNil))
      ( Right $
          Substitution.fromList
            [ (0, TIntrinsic IString)
            , (1, TRow RNil)
            ]
      )
  , -- { name : string } >~ { name : string, id : int32 }
    -- ECannotMatch (extra field in target not covered by pattern)
    MatchTestCase
      (TRow (RExtend "name" (TIntrinsic IString) RNil))
      ( TRow
          ( RExtend
              "name"
              (TIntrinsic IString)
              (RExtend "id" (TIntrinsic IInt32) RNil)
          )
      )
      (Left ECannotMatch)
  , -- '0 >~ '0 -> int32
    -- EInfiniteType (occurs check still applies in matching)
    MatchTestCase
      (TVariable (TypeIndex KType 0))
      (TArrow (TVariable (TypeIndex KType 0)) (TIntrinsic IInt32))
      (Left EInfiniteType)
  , -- type alias Id a = a
    -- Id '0 >~ int32
    -- Substitution [ 0 :=> int32 ]
    MatchTestCase
      (TAlias "Id" [TVariable (TypeIndex KType 0)] (TVariable (TypeIndex KType 0)))
      (TIntrinsic IInt32)
      ( Right $
          Substitution.fromList
            [(0, TIntrinsic IInt32)]
      )
  , -- type alias T a = a
    -- T int32 >~ int32
    -- Substitution []
    MatchTestCase
      (TAlias "T" [TIntrinsic IInt32] (TIntrinsic IInt32))
      (TIntrinsic IInt32)
      (Right mempty)
  , -- type alias T a = a
    -- T int32 >~ string
    -- ECannotMatch
    MatchTestCase
      (TAlias "T" [TIntrinsic IInt32] (TIntrinsic IInt32))
      (TIntrinsic IString)
      (Left ECannotMatch)
  , -- type alias Pair a b = a -> b
    -- Pair '0 '1 >~ int32 -> string
    -- Substitution [ 0 :=> int32, 1 :=> string ]
    MatchTestCase
      ( TAlias
          "Pair"
          [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)]
          (TArrow (TVariable (TypeIndex KType 0)) (TVariable (TypeIndex KType 1)))
      )
      (TArrow (TIntrinsic IInt32) (TIntrinsic IString))
      ( Right $
          Substitution.fromList
            [ (0, TIntrinsic IInt32)
            , (1, TIntrinsic IString)
            ]
      )
  , -- type alias R a = { x : a }
    -- R '0 >~ { x : int32 }
    -- Substitution [ 0 :=> int32 ]
    MatchTestCase
      ( TAlias
          "R"
          [TVariable (TypeIndex KType 0)]
          (TRow (RExtend "x" (TVariable (TypeIndex KType 0)) RNil))
      )
      (TRow (RExtend "x" (TIntrinsic IInt32) RNil))
      ( Right $
          Substitution.fromList
            [(0, TIntrinsic IInt32)]
      )
  , -- type alias R a = { x : a }
    -- R int32 >~ { x : string }
    -- ECannotMatch
    MatchTestCase
      ( TAlias
          "R"
          [TIntrinsic IInt32]
          (TRow (RExtend "x" (TIntrinsic IInt32) RNil))
      )
      (TRow (RExtend "x" (TIntrinsic IString) RNil))
      (Left ECannotMatch)
  , -- type alias Self a = a -> a
    -- Self '0 >~ int32 -> int32
    -- Substitution [ 0 :=> int32 ]
    MatchTestCase
      ( TAlias
          "Self"
          [TVariable (TypeIndex KType 0)]
          (TArrow (TVariable (TypeIndex KType 0)) (TVariable (TypeIndex KType 0)))
      )
      (TArrow (TIntrinsic IInt32) (TIntrinsic IInt32))
      ( Right $
          Substitution.fromList
            [(0, TIntrinsic IInt32)]
      )
  , -- type alias Self a = a -> a
    -- Self '0 >~ int32 -> string
    -- ECannotMatch (lhs forces both args to match, rhs does not)
    MatchTestCase
      ( TAlias
          "Self"
          [TVariable (TypeIndex KType 0)]
          (TArrow (TVariable (TypeIndex KType 0)) (TVariable (TypeIndex KType 0)))
      )
      (TArrow (TIntrinsic IInt32) (TIntrinsic IString))
      (Left ECannotMatch)
  , MatchTestCase
      ( (TVariable (TypeIndex KType 312) `TArrow` TVariable (TypeIndex KType 311))
          `TArrow` applyTypeArgs KType (TVariable (TypeIndex (KArrow KType KType) 310)) (TVariable (TypeIndex KType 312) :| [])
          `TArrow` applyTypeArgs KType (TVariable (TypeIndex (KArrow KType KType) 310)) (TVariable (TypeIndex KType 311) :| [])
      )
      ( (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32)
          `TArrow` applyTypeArgs KType (TConstructor (KArrow KType KType) "Option") (TIntrinsic IInt32 :| [])
          `TArrow` applyTypeArgs KType (TConstructor (KArrow KType KType) "Option") (TIntrinsic IInt32 :| [])
      )
      ( Right $
          Substitution.fromList
            [ (310, TConstructor (KArrow KType KType) "Option")
            , (311, TIntrinsic IInt32)
            , (312, TIntrinsic IInt32)
            ]
      )
  ]

runHspecTestCase :: UnificationSpecTestCase IndexedType -> Spec
runHspecTestCase specTestCase = do
  it testDescription $
    testCase specTestCase `shouldBe` expected
 where
  expected =
    case specTestCase of
      UnifyTestCase _ _ e -> e
      MatchTestCase _ _ e -> e
  testDescription =
    case specTestCase of
      UnifyTestCase t1 t2 expected ->
        prettyType t1 ++ " ~ " ++ prettyType t2 ++ " ⇒ " ++ show expected
      MatchTestCase t1 t2 expected ->
        prettyType t1 ++ " >~ " ++ prettyType t2 ++ " ⇒ " ++ show expected

prettyType :: IndexedType -> String
prettyType = renderString . layoutPretty defaultLayoutOptions . pretty 

unificationSpec :: SpecWith ()
unificationSpec =
  describe "Unification tests" $ do
    describe "unify" $ do
      forM_ unifyTestCases runHspecTestCase
    describe "match" $ do
      forM_ matchTestCases runHspecTestCase
