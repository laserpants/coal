{-# LANGUAGE OverloadedStrings #-}

module Noll.SystemFExamples.Test15 where

import Data.Data (Data)
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
import Noll.SystemFSpec.TestRunner
import Test.Hspec (Spec, describe, hspec, it)

import qualified Data.Set as Set
import qualified Noll.Common.Environment as Environment

spec :: Spec
spec =
  describe "" $
    it "" $ do
      testResultExpression (runTest fixture) == fixture1

runTest :: (Show a, Eq a, Data a) => Expression a () -> TestResult (Expression a (Type TypeIndex Kind)) a
runTest =
  runTypedExpressionTest
    (CompilerEnvironment env1 env2)
    []
 where
  env1 =
    Environment.fromList
      [
        ( "More"
        , Constructor
            "More"
            2
            (Forall mempty [] (TIntrinsic IInt32 `TArrow` TConstructor KType "Ints" `TArrow` TConstructor KType "Ints"))
        )
      ,
        ( "Nope"
        , Constructor
            "Nope"
            0
            (Forall mempty [] (TConstructor KType "Ints"))
        )
      ]
  env2 =
    Environment.fromList
      [ ("Ints", KType)
      ]

-- type Ints
--   = Nope
--   | More(int32, Ints)
--
-- fold(More(1, More(2, Nope))) {
--   | Nope =>
--       0
--   | More(m, @ms) =>
--       1 + ms
--
fixture :: Expression () ()
fixture =
  ( EFold
      ()
      ()
      ( EApplication
          ()
          ()
          (EConstructor () (Label () "More"))
          ( ELiteral () (LInt32 1)
              <| EApplication
                ()
                ()
                (EConstructor () (Label () "More"))
                ( ELiteral () (LInt32 2)
                    <| EConstructor () (Label () "Nope") :| []
                )
                :| []
          )
          :| []
      )
      ( EClause
          ()
          (PConstructor () (Label () "Nope") [])
          ( CPlain
              ()
              []
              (ELiteral () (LInt32 0))
              :| []
          )
          <| EClause
            ()
            ( PConstructor
                ()
                (Label () "More")
                [ PVariable () (Label () "m")
                , PAtVariable () (Label () "ms")
                ]
            )
            ( CPlain
                ()
                []
                ( EApplication
                    ()
                    ()
                    (EBinaryOperator () () OAddition)
                    ( ELiteral () (LInt32 0)
                        <| (EVariable () (Label () "ms"))
                          :| []
                    )
                )
                :| []
            )
            :| []
      )
      Nothing
  )

-- type Ints
--   = Nope
--   | More(int32, Ints)
--
-- fold(More(1, More(2, Nope))) {
--   | Nope =>
--       0
--   | More(m, @ms) =>
--       1 + ms
--
fixture1 :: Expression () (Type TypeIndex Kind)
fixture1 =
  ( EFold
      ()
      (TIntrinsic IInt32)
      ( EApplication
          ()
          (TConstructor KType "Ints")
          (EConstructor () (Label (TIntrinsic IInt32 `TArrow` TConstructor KType "Ints" `TArrow` TConstructor KType "Ints") "More"))
          ( ELiteral () (LInt32 1)
              <| EApplication
                ()
                (TConstructor KType "Ints")
                (EConstructor () (Label (TIntrinsic IInt32 `TArrow` TConstructor KType "Ints" `TArrow` TConstructor KType "Ints") "More"))
                ( ELiteral () (LInt32 2)
                    <| EConstructor () (Label (TConstructor KType "Ints") "Nope") :| []
                )
                :| []
          )
          :| []
      )
      ( EClause
          ()
          (PConstructor () (Label (TConstructor KType "Ints") "Nope") [])
          ( CPlain
              ()
              []
              (ELiteral () (LInt32 0))
              :| []
          )
          <| EClause
            ()
            ( PConstructor
                ()
                (Label (TConstructor KType "Ints") "More")
                [ PVariable () (Label (TIntrinsic IInt32) "m")
                , PAtVariable () (Label (TConstructor KType "Ints") "ms")
                ]
            )
            ( CPlain
                ()
                []
                ( EApplication
                    ()
                    (TIntrinsic IInt32)
                    (EBinaryOperator () (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) OAddition)
                    ( ELiteral () (LInt32 0)
                        <| (EVariable () (Label (TIntrinsic IInt32) "ms"))
                          :| []
                    )
                )
                :| []
            )
            :| []
      )
      Nothing
  )
