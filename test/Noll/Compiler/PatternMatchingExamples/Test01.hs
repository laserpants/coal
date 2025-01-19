{-# LANGUAGE OverloadedStrings #-}

module Noll.Compiler.PatternMatchingExamples.Test01 where

import Data.List.NonEmpty (NonEmpty (..), (<|))
import Noll.Compiler
import Noll.Label (Label (..))
import Noll.Language (
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

--
-- let
--   lte =
--     fn(x) =>
--        fn(y) =>
--          match(compare(x, y)) {
--            | LessThan or EqualTo => true
--            | GreaterThan => false
--   in
--     lte
--
fixture :: Expression () ()
fixture =
  ( ELet
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
  )
