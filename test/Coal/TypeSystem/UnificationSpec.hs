{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.TypeSystem.UnificationSpec where

import Coal.Language
import Coal.TypeSystem.Substitution
import Coal.TypeSystem.Unification

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
    -- <does not unify>
    UnificationSpecTestCase
      (TIntrinsic IBool `TArrow` TIntrinsic IInt32)
      (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32)
      (Left ECannotUnify)
  , -- bool ~ int32
    -- <does not unify>
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
    -- <does not unify>
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
  ]

unificationSpec =
  error "TODO"
