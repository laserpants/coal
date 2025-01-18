{-# LANGUAGE OverloadedStrings #-}

module Noll.Compiler.Transform.PatternMatchingSpec where

import Noll.Language (Expression (..))
import Noll.Label (Label (..))
import Test.Hspec (Spec, describe, it)
import Noll.Compiler.Transform.PatternMatching

spec :: Spec
spec =
  describe "" $ do
    testGroupByConstructor

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

