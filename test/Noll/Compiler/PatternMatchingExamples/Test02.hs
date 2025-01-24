{-# LANGUAGE OverloadedStrings #-}

module Noll.Compiler.PatternMatchingExamples.Test02 where

import Noll.Common.Environment (Environment (..))
import Noll.Common.List1 (NonEmpty (..), (<|))
import Noll.Compiler.PatternMatching
import Noll.Compiler.PatternMatching.Compiler
import Noll.Compiler.PatternMatching.Envelope
import Noll.Compiler.PatternMatching.Equation
import Noll.Compiler.PatternMatchingSpec.TestRunner (compilePatterns)
import Noll.Eval (Value (..), eval)
import Noll.Label (Label (..))
import Noll.Language (Expression (..), Primitive (..))
import Test.Hspec (Spec, describe, it)

import qualified Data.Map.Strict as Map
import qualified Noll.Common.Environment as Environment

spec :: Spec
spec =
  describe "" $ do
    it "" $ do
      evalWithEnv
        fixture
        ( Environment.fromList
            [
              ( "u1"
              , VLiteral (LInt32 1)
              )
            ,
              ( "u2"
              , VData "Nil" []
              )
            ,
              ( "u3"
              , VData "Nil" []
              )
            ]
        )
        == VData "A" [VLiteral (LInt32 1), VData "Nil" []]
    it "" $ do
      evalWithEnv
        fixture
        ( Environment.fromList
            [
              ( "u1"
              , VLiteral (LInt32 1)
              )
            ,
              ( "u2"
              , VData "Cons" [VLiteral (LInt32 1), VData "Nil" []]
              )
            ,
              ( "u3"
              , VData "Nil" []
              )
            ]
        )
        == VData "B" [VLiteral (LInt32 1), VData "Cons" [VLiteral (LInt32 1), VData "Nil" []]]
    it "" $ do
      evalWithEnv
        fixture
        ( Environment.fromList
            [
              ( "u1"
              , VLiteral (LInt32 1)
              )
            ,
              ( "u2"
              , VData "Cons" [VLiteral (LInt32 2), VData "Nil" []]
              )
            ,
              ( "u3"
              , VData "Cons" [VLiteral (LInt32 3), VData "Nil" []]
              )
            ]
        )
        == VData
          "C"
          [ VLiteral (LInt32 1)
          , VLiteral (LInt32 2)
          , VData "Nil" []
          , VLiteral (LInt32 3)
          , VData "Nil" []
          ]

-- match (u2, u3) {
--   | (Nil, ys) =>
--       A(u1, ys)
--   | (xs, Nil) =>
--       B(u1, xs)
--   | (Cons(x, xs), Cons(y, ys)) =>
--       C(u1, x, xs, y, ys)
-- }
fixture :: Expression () ()
fixture =
  runMatchMonad
    "match"
    0
    ( compilePatterns
        [Label () "u2", Label () "u3"]
        [ patternEquation
            [ MConstructor (Label () "Nil") []
            , MVariable (Label () "ys")
            ]
            (MExpression (EApplication () () (EConstructor () (Label () "A")) (EVariable () (Label () "u1") <| EVariable () (Label () "ys") :| [])))
        , patternEquation
            [ MVariable (Label () "xs")
            , MConstructor (Label () "Nil") []
            ]
            (MExpression (EApplication () () (EConstructor () (Label () "B")) (EVariable () (Label () "u1") <| EVariable () (Label () "xs") :| [])))
        , patternEquation
            [ MConstructor
                (Label () "Cons")
                [ MVariable (Label () "x")
                , MVariable (Label () "xs")
                ]
            , MConstructor
                (Label () "Cons")
                [ MVariable (Label () "y")
                , MVariable (Label () "ys")
                ]
            ]
            ( MExpression
                ( EApplication
                    ()
                    ()
                    (EConstructor () (Label () "C"))
                    ( EVariable () (Label () "u1")
                        <| EVariable () (Label () "x")
                        <| EVariable () (Label () "xs")
                        <| EVariable () (Label () "y")
                        <| EVariable () (Label () "ys")
                        :| []
                    )
                )
            )
        ]
        MFail
    )

evalWithEnv :: Expression () () -> Environment Value -> Value
evalWithEnv = flip eval
