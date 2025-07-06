{-# LANGUAGE OverloadedStrings #-}

module Noll.Compiler.PatternMatchingExamples.Test01 where

import Data.List.NonEmpty (NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Noll.Compiler.PatternMatching
import Noll.Compiler.PatternMatching.Rule
import Noll.Language (
  Binding (..),
  Choice (..),
  Clause (..),
  CompiledClause (..),
  Expression (..),
  Pattern (..),
  Primitive (..),
 )
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "" $
    it "" $ do
      evalMatchMonad "???" 0 (compileMatchExprs fixture) == fixture1

--
-- let
--   lte =
--     fn(x) =>
--        fn(y) =>
--          match(compare(x, y)) {
--            | LessThan or EqualTo => true
--            | GreaterThan => false
--          }
--   in
--     lte
--
fixture :: Expression () ()
fixture =
  ELet
    ()
    ( BPattern
        ()
        (PVariable () (Label () "lte"))
        ( ELambda
            ()
            (PVariable () (Label () "x") :| [])
            ( ELambda
                ()
                (PVariable () (Label () "y") :| [])
                ( EMatch
                    ()
                    ()
                    ( EApplication
                        ()
                        ()
                        (EVariable () (Label () "compare"))
                        (EVariable () (Label () "x") <| EVariable () (Label () "y") :| [])
                    )
                    ( EClause
                        ()
                        (PConstructor () (Label () "LessThan") [])
                        (CPlain () [] (ELiteral () (LBool True)) :| [])
                        <| EClause
                          ()
                          (PConstructor () (Label () "EqualTo") [])
                          (CPlain () [] (ELiteral () (LBool True)) :| [])
                        <| EClause
                          ()
                          (PConstructor () (Label () "GreaterThan") [])
                          (CPlain () [] (ELiteral () (LBool False)) :| [])
                          :| []
                    )
                )
            )
        )
        :| []
    )
    (EVariable () (Label () "lte"))

fixture1 :: Expression () ()
fixture1 =
  ELet
    ()
    ( BPattern
        ()
        (PVariable () (Label () "lte"))
        ( ELambda
            ()
            (PVariable () (Label () "x") :| [])
            ( ELambda
                ()
                (PVariable () (Label () "y") :| [])
                ( ECompiledMatch
                    ()
                    ()
                    ( EApplication
                        ()
                        ()
                        (EVariable () (Label () "compare"))
                        (EVariable () (Label () "x") <| EVariable () (Label () "y") :| [])
                    )
                    ( ECompiledClause
                        (Label () "EqualTo" :| [])
                        (ELiteral () (LBool True))
                        <| ECompiledClause
                          (Label () "GreaterThan" :| [])
                          (ELiteral () (LBool False))
                        <| ECompiledClause
                          (Label () "LessThan" :| [])
                          (ELiteral () (LBool True))
                          :| []
                    )
                )
            )
        )
        :| []
    )
    (EVariable () (Label () "lte"))
