{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.TypeSystem.UnificationSpec where

import Coal.Language
import Coal.TypeSystem.Substitution
import Coal.TypeSystem.Unification

import qualified Coal.Common.List1 as List1
import qualified Coal.TypeSystem.Substitution as Substitution

data UnificationSpecTestCase t = UnificationSpecTestCase t t (Either UnificationError Substitution)
  deriving (Show, Eq, Ord)

testCase :: UnificationSpecTestCase IndexedType -> Either UnificationError Substitution
testCase (UnificationSpecTestCase t1 t2 _) = evalUnifier (freshIdIn [t1, t2]) (unify t1 t2)

runTestCase :: UnificationSpecTestCase IndexedType -> Bool
runTestCase test = testCase test == s where (UnificationSpecTestCase _ _ s) = test

runAllTestCases :: [Bool]
runAllTestCases = runTestCase <$> testCases

testCases :: [UnificationSpecTestCase IndexedType]
testCases =
  [ -- '0 ~ int32
    -- Substitution [ 0 :=> int32 ]
    UnificationSpecTestCase
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
    UnificationSpecTestCase
      (TVariable (TypeIndex KType 0))
      (TVariable (TypeIndex KType 0))
      (Right mempty)
  , -- int32 ~ int32
    -- Substitution []
    UnificationSpecTestCase
      (TIntrinsic IInt32)
      (TIntrinsic IInt32)
      (Right mempty)
  , -- '0 -> '1 ~ int32 -> int32
    -- Substitution [ 0 :=> int32, 1 :=> int32 ]
    UnificationSpecTestCase
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
    UnificationSpecTestCase
      (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32)
      (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32)
      (Right mempty)
  , -- bool -> int32 ~ int32 -> int32
    -- ECannotUnify
    UnificationSpecTestCase
      (TIntrinsic IBool `TArrow` TIntrinsic IInt32)
      (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32)
      (Left ECannotUnify)
  , -- bool ~ int32
    -- ECannotUnify
    UnificationSpecTestCase
      (TIntrinsic IBool)
      (TIntrinsic IInt32)
      (Left ECannotUnify)
  , -- { | '0 } ~ { id : int32, name : string }
    -- Substitution [ 0 :=> { id : int32, name : string } ]
    UnificationSpecTestCase
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
    UnificationSpecTestCase
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
    UnificationSpecTestCase
      (TRow (RExtend "name" (TIntrinsic IString) (RExtend "id" (TIntrinsic IInt32) RNil)))
      (TRow (RExtend "id" (TIntrinsic IInt32) (RExtend "name" (TIntrinsic IString) RNil)))
      (Right mempty)
  , -- { name : string, id : int32 } ~ { name : int32, id : int32 }
    -- ECannotUnify
    UnificationSpecTestCase
      (TRow (RExtend "name" (TIntrinsic IString) (RExtend "id" (TIntrinsic IInt32) RNil)))
      (TRow (RExtend "name" (TIntrinsic IInt32) (RExtend "id" (TIntrinsic IInt32) RNil)))
      (Left ECannotUnify)
  , -- { name : '0 | '1 } ~ { name : string }
    -- [ 0 :=> string, 1 :=> {} ]
    UnificationSpecTestCase
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
    UnificationSpecTestCase
      (TVariable (TypeIndex KType 0))
      (TVariable (TypeIndex KRow 0))
      (Left EKindMismatch)
  , -- '0 ~ '0 -> int32
    -- EInfiniteType
    UnificationSpecTestCase
      (TVariable (TypeIndex KType 0))
      (TArrow (TVariable (TypeIndex KType 0)) (TIntrinsic IInt32))
      (Left EInfiniteType)
  , -- '0:Row ~ { x : int32 | '0 }
    -- EInfiniteType
    UnificationSpecTestCase
      (TVariable (TypeIndex KRow 0))
      (TRow (RExtend "x" (TIntrinsic IInt32) (RVariable (TypeIndex KRow 0))))
      (Left EInfiniteType)
  , -- string ~ int32
    -- ECannotUnify
    UnificationSpecTestCase
      (TIntrinsic IString)
      (TIntrinsic IInt32)
      (Left ECannotUnify)
  , -- record { x : int32 } ~ record { x : int32 }
    -- Substitution []
    UnificationSpecTestCase
      (TIntrinsic (IRecord (TRow (RExtend "x" (TIntrinsic IInt32) RNil))))
      (TIntrinsic (IRecord (TRow (RExtend "x" (TIntrinsic IInt32) RNil))))
      (Right mempty)
  , -- int32 -> string ~ int32 -> int32
    -- ECannotUnify
    UnificationSpecTestCase
      (TArrow (TIntrinsic IInt32) (TIntrinsic IString))
      (TArrow (TIntrinsic IInt32) (TIntrinsic IInt32))
      (Left ECannotUnify)
  , -- '0 -> ('1 -> int32) ~ bool -> (string -> int32)
    -- Substitution [ 0 :=> bool, 1 :=> string ]
    UnificationSpecTestCase
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
    UnificationSpecTestCase
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
    UnificationSpecTestCase
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
    UnificationSpecTestCase
      (TRow (RExtend "x" (TIntrinsic IInt32) RNil))
      (TRow (RExtend "x" (TIntrinsic IString) RNil))
      (Left ECannotUnify)
  , -- List '0 ~ int32
    -- ECannotUnify
    UnificationSpecTestCase
      ( TApplication
          KType
          (TConstructor KType "List")
          (List1.singleton (TVariable (TypeIndex KType 0)))
      )
      (TIntrinsic IInt32)
      (Left ECannotUnify)
  , -- type alias T a = a
    -- T int32 ~ int32
    UnificationSpecTestCase
      (TAlias "T" [TIntrinsic IInt32] (TIntrinsic IInt32))
      (TIntrinsic IInt32)
      (Right mempty)
  , -- type alias T a = a
    -- T int32 ~ string
    -- ECannotUnify
    UnificationSpecTestCase
      (TAlias "T" [TIntrinsic IInt32] (TIntrinsic IInt32))
      (TIntrinsic IString)
      (Left ECannotUnify)
  , -- type alias Id a = a
    -- Id '0 ~ int32
    -- Substitution [ 0 :=> int32 ]
    UnificationSpecTestCase
      (TAlias "Id" [TVariable (TypeIndex KType 0)] (TVariable (TypeIndex KType 0)))
      (TIntrinsic IInt32)
      ( Right $
          Substitution.fromList
            [(0, TIntrinsic IInt32)]
      )
  , -- type alias Pair a b = (a -> b)
    -- Pair '0 '1 ~ (int32 -> string)
    -- Substitution [ 0 :=> int32, 1 :=> string ]
    UnificationSpecTestCase
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
    UnificationSpecTestCase
      (TVariable (TypeIndex KType 0))
      (TVariable (TypeIndex KType 1))
      ( Right $
          Substitution.fromList
            [(0, TVariable (TypeIndex KType 1))]
      )
  , -- '0 ~ '1 -> '1, '1 ~ int32
    -- Substitution [ 1 :=> int32, 0 :=> (int32 -> int32) ]
    UnificationSpecTestCase
      (TVariable (TypeIndex KType 0))
      (TArrow (TVariable (TypeIndex KType 1)) (TVariable (TypeIndex KType 1)))
      ( Right $
          Substitution.fromList
            [(0, TArrow (TVariable (TypeIndex KType 1)) (TVariable (TypeIndex KType 1)))]
      )
  ]

unificationSpec =
  error "TODO"
