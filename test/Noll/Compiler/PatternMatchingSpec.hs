{-# LANGUAGE OverloadedStrings #-}

module Noll.Compiler.PatternMatchingSpec where

import Noll.Common.Environment (Environment (..))
import Noll.Compiler.PatternMatching
import Noll.Eval (Value (..), eval)
import Noll.Label (Label (..))
import Noll.Language (Expression (..), Primitive (..))
import Test.Hspec (Spec, describe, it)

import qualified Data.Map.Strict as Map

spec :: Spec
spec =
  describe "" $ do
    testGroupByConstructor
    testCompileEnvelopeExpression

testCompileEnvelopeExpression :: Spec
testCompileEnvelopeExpression =
  describe "compileEnvelope" $ do
    it "" $
      compileEnvelope
        ( matchPatterns
            [Label () "u1", Label () "u2", Label () "u3"]
            [ patternEquation
                [ MVariable (Label () "v1")
                , MVariable (Label () "v2")
                , MVariable (Label () "v3")
                ]
                (MExpression (ELiteral () (LInt32 1)))
            ]
            MFail
        )
        == (ELiteral () (LInt32 1) :: (Expression ()) ())
    it "" $
      compileEnvelope
        ( matchPatterns
            [Label () "u1", Label () "u2", Label () "u3"]
            [ patternEquation
                [ MVariable (Label () "v1")
                , MVariable (Label () "v2")
                , MVariable (Label () "v3")
                ]
                (MExpression (EVariable () (Label () "v1")))
            ]
            MFail
        )
        == EVariable () (Label () "u1")
    it "" $
      compileEnvelope
        ( matchPatterns
            [Label () "u1", Label () "u2", Label () "u3"]
            [ patternEquation
                [ MVariable (Label () "v1")
                , MVariable (Label () "v2")
                , MVariable (Label () "v3")
                ]
                (MExpression (EVariable () (Label () "v2")))
            ]
            MFail
        )
        == EVariable () (Label () "u2")
    it "" $
      compileEnvelope
        ( matchPatterns
            [Label () "u1", Label () "u2", Label () "u3"]
            [ patternEquation
                [ MVariable (Label () "v1")
                , MVariable (Label () "v2")
                , MVariable (Label () "v3")
                ]
                (MExpression (EVariable () (Label () "v3")))
            ]
            MFail
        )
        == EVariable () (Label () "u3")
    it "" $
      eval
        ( Environment
            ( Map.fromList
                [
                  ( "u1"
                  , VData "Cons" [VLiteral (LInt32 1), VData "Nil" []]
                  )
                ,
                  ( "u2"
                  , VLiteral (LInt32 1)
                  )
                ,
                  ( "u3"
                  , VLiteral (LInt32 2)
                  )
                ]
            )
        )
        ( compileEnvelope
            ( matchPatterns
                [Label () "u1", Label () "u2", Label () "u3"]
                [ patternEquation
                    [ MConstructor (Label () "Cons") [MVariable (Label () "x"), MVariable (Label () "a")]
                    , MVariable (Label () "y")
                    , MVariable (Label () "z")
                    ]
                    (MExpression (EVariable () (Label () "y")))
                ]
                MFail
            )
        )
        == VLiteral (LInt32 1)
    it "" $
      eval
        ( Environment
            ( Map.fromList
                [
                  ( "u1"
                  , VData "Cons" [VLiteral (LInt32 100), VData "Nil" []]
                  )
                ,
                  ( "u2"
                  , VLiteral (LInt32 1)
                  )
                ,
                  ( "u3"
                  , VLiteral (LInt32 2)
                  )
                ]
            )
        )
        ( compileEnvelope
            ( matchPatterns
                [Label () "u1", Label () "u2", Label () "u3"]
                [ patternEquation
                    [ MConstructor (Label () "Cons") [MVariable (Label () "x"), MVariable (Label () "_")]
                    , MVariable (Label () "y")
                    , MVariable (Label () "z")
                    ]
                    (MExpression (EVariable () (Label () "x")))
                ]
                MFail
            )
        )
        == VLiteral (LInt32 100)
    it "" $
      eval
        ( Environment
            ( Map.fromList
                [
                  ( "u1"
                  , VData "Cons" [VLiteral (LInt32 100), VData "Nil" []]
                  )
                ,
                  ( "u2"
                  , VLiteral (LInt32 1)
                  )
                ,
                  ( "u3"
                  , VLiteral (LInt32 2)
                  )
                ]
            )
        )
        ( compileEnvelope
            ( matchPatterns
                [Label () "u1", Label () "u2", Label () "u3"]
                [ patternEquation
                    [ MConstructor (Label () "Cons") [MVariable (Label () "_"), MVariable (Label () "x")]
                    , MVariable (Label () "y")
                    , MVariable (Label () "z")
                    ]
                    (MExpression (EVariable () (Label () "x")))
                ]
                MFail
            )
        )
        == VData "Nil" []
    it "" $
      eval
        ( Environment
            ( Map.fromList
                [
                  ( "u1"
                  , VData "Cons" [VLiteral (LInt32 100), VData "Nil" []]
                  )
                ,
                  ( "u2"
                  , VLiteral (LInt32 1)
                  )
                ,
                  ( "u3"
                  , VLiteral (LInt32 2)
                  )
                ]
            )
        )
        ( compileEnvelope
            ( matchPatterns
                [Label () "u1", Label () "u2", Label () "u3"]
                [ patternEquation
                    [ MConstructor (Label () "Nil") []
                    , MVariable (Label () "y")
                    , MVariable (Label () "z")
                    ]
                    (MExpression (ELiteral () (LInt32 1)))
                , patternEquation
                    [ MConstructor (Label () "Cons") [MVariable (Label () "_"), MVariable (Label () "x")]
                    , MVariable (Label () "y")
                    , MVariable (Label () "z")
                    ]
                    (MExpression (ELiteral () (LInt32 2)))
                ]
                MFail
            )
        )
        == VLiteral (LInt32 2)
    it "" $
      eval
        ( Environment
            ( Map.fromList
                [
                  ( "u1"
                  , VData "Cons" [VLiteral (LInt32 100), VData "Nil" []]
                  )
                ,
                  ( "u2"
                  , VLiteral (LInt32 1)
                  )
                ,
                  ( "u3"
                  , VLiteral (LInt32 2)
                  )
                ]
            )
        )
        ( compileEnvelope
            ( matchPatterns
                [Label () "u1", Label () "u2", Label () "u3"]
                [ patternEquation
                    [ MConstructor (Label () "Nil") []
                    , MVariable (Label () "y")
                    , MVariable (Label () "z")
                    ]
                    (MExpression (ELiteral () (LInt32 1)))
                , patternEquation
                    [ MConstructor (Label () "Cons") [MVariable (Label () "_"), MConstructor (Label () "Cons") [MVariable (Label () "_"), MVariable (Label () "_")]]
                    , MVariable (Label () "y")
                    , MVariable (Label () "z")
                    ]
                    (MExpression (ELiteral () (LInt32 2)))
                , patternEquation
                    [ MConstructor (Label () "Cons") [MVariable (Label () "_"), MVariable (Label () "_")]
                    , MVariable (Label () "y")
                    , MVariable (Label () "z")
                    ]
                    (MExpression (ELiteral () (LInt32 3)))
                ]
                MFail
            )
        )
        == VLiteral (LInt32 3)
    it "" $
      eval
        ( Environment
            ( Map.fromList
                [
                  ( "u1"
                  , VData "Cons" [VLiteral (LInt32 100), VData "Nil" []]
                  )
                ,
                  ( "u2"
                  , VLiteral (LInt32 1)
                  )
                ,
                  ( "u3"
                  , VLiteral (LInt32 2)
                  )
                ]
            )
        )
        ( compileEnvelope
            ( matchPatterns
                [Label () "u1", Label () "u2", Label () "u3"]
                [ patternEquation
                    [ MConstructor (Label () "Nil") []
                    , MVariable (Label () "y")
                    , MVariable (Label () "z")
                    ]
                    (MExpression (ELiteral () (LInt32 1)))
                , patternEquation
                    [ MConstructor (Label () "Cons") [MLiteral () (ELiteral () (LInt32 100)), MVariable (Label () "_")]
                    , MVariable (Label () "y")
                    , MVariable (Label () "z")
                    ]
                    (MExpression (ELiteral () (LInt32 2)))
                , patternEquation
                    [ MConstructor (Label () "Cons") [MVariable (Label () "_"), MVariable (Label () "_")]
                    , MVariable (Label () "y")
                    , MVariable (Label () "z")
                    ]
                    (MExpression (ELiteral () (LInt32 3)))
                ]
                MFail
            )
        )
        == VLiteral (LInt32 2)
    it "" $
      eval
        ( Environment
            ( Map.fromList
                [
                  ( "u1"
                  , VData "Cons" [VLiteral (LInt32 100), VData "Nil" []]
                  )
                ,
                  ( "u2"
                  , VLiteral (LInt32 1)
                  )
                ,
                  ( "u3"
                  , VLiteral (LInt32 2)
                  )
                ]
            )
        )
        ( compileEnvelope
            ( matchPatterns
                [Label () "u1", Label () "u2", Label () "u3"]
                [ patternEquation
                    [ MConstructor (Label () "Nil") []
                    , MVariable (Label () "y")
                    , MVariable (Label () "z")
                    ]
                    (MExpression (ELiteral () (LInt32 1)))
                , patternEquation
                    [ MConstructor (Label () "Cons") [MLiteral () (ELiteral () (LInt32 200)), MVariable (Label () "_")]
                    , MVariable (Label () "y")
                    , MVariable (Label () "z")
                    ]
                    (MExpression (ELiteral () (LInt32 2)))
                , patternEquation
                    [ MConstructor (Label () "Cons") [MVariable (Label () "_"), MVariable (Label () "_")]
                    , MVariable (Label () "y")
                    , MVariable (Label () "z")
                    ]
                    (MExpression (ELiteral () (LInt32 3)))
                ]
                MFail
            )
        )
        == VLiteral (LInt32 3)

testGroupByConstructor :: Spec
testGroupByConstructor = do
  describe "groupByConstructor" $ do
    it "" $
      groupByHeadConstructor
        ( [ HeadConstructorEquation (Label () "A") [MVariable (Label () "0")] (PatternEquationBody [] MFail)
          , HeadConstructorEquation (Label () "B") [MVariable (Label () "1")] (PatternEquationBody [] MFail)
          , HeadConstructorEquation (Label () "A") [MVariable (Label () "2")] (PatternEquationBody [] MFail)
          ] ::
            [HeadConstructorEquation (Expression ()) ()]
        )
        == [
             [ HeadConstructorEquation (Label () "A") [MVariable (Label () "0")] (PatternEquationBody [] MFail)
             , HeadConstructorEquation (Label () "A") [MVariable (Label () "2")] (PatternEquationBody [] MFail)
             ]
           ,
             [ HeadConstructorEquation (Label () "B") [MVariable (Label () "1")] (PatternEquationBody [] MFail)
             ]
           ]
    it "" $
      groupByHeadConstructor
        ( [ HeadConstructorEquation (Label () "A") [MVariable (Label () "0")] (PatternEquationBody [] MFail)
          , HeadConstructorEquation (Label () "B") [MVariable (Label () "1")] (PatternEquationBody [] MFail)
          , HeadConstructorEquation (Label () "A") [MVariable (Label () "2")] (PatternEquationBody [] MFail)
          , HeadConstructorEquation (Label () "B") [MVariable (Label () "3")] (PatternEquationBody [] MFail)
          , HeadConstructorEquation (Label () "A") [MVariable (Label () "4")] (PatternEquationBody [] MFail)
          , HeadConstructorEquation (Label () "A") [MVariable (Label () "5")] (PatternEquationBody [] MFail)
          , HeadConstructorEquation (Label () "C") [MVariable (Label () "6")] (PatternEquationBody [] MFail)
          ] ::
            [HeadConstructorEquation (Expression ()) ()]
        )
        == [
             [ HeadConstructorEquation (Label () "A") [MVariable (Label () "0")] (PatternEquationBody [] MFail)
             , HeadConstructorEquation (Label () "A") [MVariable (Label () "2")] (PatternEquationBody [] MFail)
             , HeadConstructorEquation (Label () "A") [MVariable (Label () "4")] (PatternEquationBody [] MFail)
             , HeadConstructorEquation (Label () "A") [MVariable (Label () "5")] (PatternEquationBody [] MFail)
             ]
           ,
             [ HeadConstructorEquation (Label () "B") [MVariable (Label () "1")] (PatternEquationBody [] MFail)
             , HeadConstructorEquation (Label () "B") [MVariable (Label () "3")] (PatternEquationBody [] MFail)
             ]
           ,
             [ HeadConstructorEquation (Label () "C") [MVariable (Label () "6")] (PatternEquationBody [] MFail)
             ]
           ]
