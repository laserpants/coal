{-# LANGUAGE OverloadedStrings #-}

module Noll.TypeSystemExamples.Test2 where

import Data.List.NonEmpty (NonEmpty (..), (<|))
import Noll.Compiler
import Noll.Label (Label (..))
import Noll.Language (
  BinaryOperator (..),
  Binding (..),
  Choice (..),
  Clause (..),
  Constructor (..),
  Expression (..),
  Intrinsic (..),
  Kind (..),
  Pattern (..),
  Primitive (..),
  Scheme (..),
  Type (..),
  TypeIndex (..),
 )
import Noll.TypeSystemSpec.TestRunner
import Test.Hspec (Spec, describe, hspec, it)

import qualified Data.Set as Set
import qualified Noll.Lib.Environment as Environment

spec :: Spec
spec =
  describe "let gt = fn(x) => not <<< lte(x) in gt" $
    it "" $ do
      runTest fixture
        == 
          ( ELet
              ()
              ( BPattern
                  ()
                  (PVariable () (Label (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool) "gt"))
                  ( ELambda
                      ()
                      (PVariable () (Label (TVariable (TypeIndex KType 0)) "x") :| [])
                      ( EApplication
                          ()
                          (TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool)
                          (EBinaryOperator () ((TIntrinsic IBool `TArrow` TIntrinsic IBool) `TArrow` (TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool, OReverseComposition))
                          ( EVariable () (Label (TIntrinsic IBool `TArrow` TIntrinsic IBool) "not")
                              <| EApplication ()
                                (TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool)
                                (EVariable () (Label (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool) "lte"))
                                (EVariable () (Label (TVariable (TypeIndex KType 0)) "x") :| [])
                                :| []
                          )
                      )
                  )
                  :| []
              )
              (EVariable () (Label (TVariable (TypeIndex KType 1) `TArrow` TVariable (TypeIndex KType 1) `TArrow` TIntrinsic IBool) "gt"))
          )

runTest :: (Eq a) => Expression a () -> Expression a (Type TypeIndex Kind)
runTest =
  testResultExpression
    . runTypedExpressionTest
      (CompilerEnvironment env1 env2)
      [
        ( "not"
        , Forall
            mempty
            []
            ( TIntrinsic IBool `TArrow` TIntrinsic IBool )
        )
      ,
        ( "lte"
        , Forall
            (Set.fromList [TypeIndex KType 0])
            []
            ( TVariable (TypeIndex KType 0)
                `TArrow` TVariable (TypeIndex KType 0)
                `TArrow` TIntrinsic IBool
            )
        )
      ]
 where
  env1 =
    Environment.fromList
      [
      ]
  env2 =
    Environment.fromList
      [ ("Ordering", KType)
      ]

--
-- let
--   gt =
--     fn(x) =>
--       not <<< lte(x)
--   in
--     gt
--
fixture :: Expression () ()
fixture =
  ( ELet
      ()
      ( BPattern
          ()
          (PVariable () (Label () "gt"))
          ( ELambda
              ()
              (PVariable () (Label () "x") :| [])
              ( EApplication
                  ()
                  ()
                  (EBinaryOperator () ((), OReverseComposition))
                  ( EVariable () (Label () "not")
                      <| EApplication
                        ()
                        ()
                        (EVariable () (Label () "lte"))
                        (EVariable () (Label () "x") :| [])
                        :| []
                  )
              )
          )
          :| []
      )
      (EVariable () (Label () "gt"))
  )
