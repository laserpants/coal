{-# LANGUAGE OverloadedStrings #-}

module Noll.Language.HasFreeSpec where

import Data.List.NonEmpty (NonEmpty (..), (<|))
import Data.Set (Set)
import Noll.Label (Label (..))
import Noll.Language.Expression (Clause (..), Expression (..))
import Noll.Language.Expression.Binding (Binding (..))
import Noll.Language.Expression.Choice (Choice (..), Guard (..))
import Noll.Language.HasFree
import Noll.Language.Pattern (Pattern (..))
import Noll.Language.Primitive (Primitive (..))
import Noll.Language.Type (Type (..), TypeIndex (..))
import Noll.Language.Type.Kind (Kind)
import Test.Hspec (Spec, describe, it)

import qualified Data.Set as Set

spec :: Spec
spec =
  describe "Noll.Language.HasFree" $ do
    describe "freeIn" $ do
      describe "EVariable" $
        it "a" $
          freeIn (EVariable () (Label () "a")) == Set.fromList [Label () "a"]
      describe "ELiteral" $
        it "1" $
          freeIn (ELiteral () (LInt32 1) :: Expression () (Type TypeIndex Kind)) == (mempty :: Set (Label (Type TypeIndex Kind)))
      describe "EIf" $
        it "if a then b else c" $
          freeIn (EIf () () (EVariable () (Label () "a")) (EVariable () (Label () "b")) (EVariable () (Label () "c"))) == Set.fromList [Label () "a", Label () "b", Label () "c"]
      describe "ELambda" $ do
        it "fn(x) => x" $
          freeIn (ELambda () (PVariable () (Label () "x") :| []) (EVariable () (Label () "x"))) == (mempty :: Set (Label ()))
        it "fn(x) => y" $
          freeIn (ELambda () (PVariable () (Label () "x") :| []) (EVariable () (Label () "y"))) == Set.fromList [Label () "y"]
      describe "ELet" $ do
        it "let a = 1 in a" $
          freeIn (ELet () (BPattern () (PVariable () (Label () "a")) (ELiteral () (LInt32 1)) :| []) (EVariable () (Label () "a")) :: (Expression ()) ()) == (mempty :: Set (Label ()))
        it "let a = 1 in b" $
          freeIn (ELet () (BPattern () (PVariable () (Label () "a")) (ELiteral () (LInt32 1)) :| []) (EVariable () (Label () "b")) :: (Expression ()) ()) == Set.fromList [Label () "b"]
        it "let a = a in a" $
          freeIn (ELet () (BPattern () (PVariable () (Label () "a")) (EVariable () (Label () "a")) :| []) (EVariable () (Label () "a")) :: (Expression ()) ()) == Set.fromList [Label () "a"]
        it "let a = b in a" $
          freeIn (ELet () (BPattern () (PVariable () (Label () "a")) (EVariable () (Label () "b")) :| []) (EVariable () (Label () "a")) :: (Expression ()) ()) == Set.fromList [Label () "b"]
        it "let a = b in c" $
          freeIn (ELet () (BPattern () (PVariable () (Label () "a")) (EVariable () (Label () "b")) :| []) (EVariable () (Label () "c")) :: (Expression ()) ()) == Set.fromList [Label () "b", Label () "c"]
        it "let a = b in b" $
          freeIn (ELet () (BPattern () (PVariable () (Label () "a")) (EVariable () (Label () "b")) :| []) (EVariable () (Label () "b")) :: (Expression ()) ()) == Set.fromList [Label () "b"]
      describe "EApp" $ do
        it "f(x)" $
          freeIn (EApplication () () (EVariable () (Label () "f")) (EVariable () (Label () "x") :| [])) == Set.fromList [Label () "f", Label () "x"]
        it "(fn(x) => x)(x)" $
          freeIn (EApplication () () (ELambda () (PVariable () (Label () "x") :| []) (EVariable () (Label () "x"))) (EVariable () (Label () "x") :| [])) == Set.fromList [Label () "x"]
        it "(fn(x) => y)(x)" $
          freeIn (EApplication () () (ELambda () (PVariable () (Label () "x") :| []) (EVariable () (Label () "y"))) (EVariable () (Label () "x") :| [])) == Set.fromList [Label () "x", Label () "y"]
      describe "EConstructor" $ do
        it "Cons" $
          freeIn (EConstructor () (Label () "Cons")) == (mempty :: Set (Label ()))
      describe "EMatch" $ do
        it "match(x) { | Cons(x, xs) => z | Cons(y, ys) => z }" $
          freeIn
            ( EMatch
                ()
                ()
                (EVariable () (Label () "x"))
                ( EClause
                    ()
                    (PConstructor () (Label () "Cons") [PVariable () (Label () "x"), PVariable () (Label () "xs")])
                    ( CPlain
                        ()
                        []
                        (EVariable () (Label () "z"))
                        :| []
                    )
                    <| EClause
                      ()
                      (PConstructor () (Label () "Cons") [PVariable () (Label () "y"), PVariable () (Label () "ys")])
                      ( CPlain
                          ()
                          []
                          (EVariable () (Label () "z"))
                          :| []
                      )
                      :| []
                )
            )
            == (Set.fromList [Label () "x", Label () "z"])
        it "match(x) { | Cons(z, xs) => z | Cons(y, ys) => z }" $
          freeIn
            ( EMatch
                ()
                ()
                (EVariable () (Label () "x"))
                ( EClause
                    ()
                    (PConstructor () (Label () "Cons") [PVariable () (Label () "z"), PVariable () (Label () "xs")])
                    ( CPlain
                        ()
                        []
                        (EVariable () (Label () "z"))
                        :| []
                    )
                    <| EClause
                      ()
                      (PConstructor () (Label () "Cons") [PVariable () (Label () "y"), PVariable () (Label () "ys")])
                      ( CPlain
                          ()
                          []
                          (EVariable () (Label () "z"))
                          :| []
                      )
                      :| []
                )
            )
            == (Set.fromList [Label () "x", Label () "z"])
        it "match(x) { | Cons(z, xs) => z | Cons(z, ys) => z }" $
          freeIn
            ( EMatch
                ()
                ()
                (EVariable () (Label () "x"))
                ( EClause
                    ()
                    (PConstructor () (Label () "Cons") [PVariable () (Label () "z"), PVariable () (Label () "xs")])
                    ( CPlain
                        ()
                        []
                        (EVariable () (Label () "z"))
                        :| []
                    )
                    <| EClause
                      ()
                      (PConstructor () (Label () "Cons") [PVariable () (Label () "z"), PVariable () (Label () "ys")])
                      ( CPlain
                          ()
                          []
                          (EVariable () (Label () "z"))
                          :| []
                      )
                      :| []
                )
            )
            == (Set.fromList [Label () "x"])
        it "match(z) { | Cons(z, xs) => z | Cons(z, ys) => z }" $
          freeIn
            ( EMatch
                ()
                ()
                (EVariable () (Label () "z"))
                ( EClause
                    ()
                    (PConstructor () (Label () "Cons") [PVariable () (Label () "z"), PVariable () (Label () "xs")])
                    ( CPlain
                        ()
                        []
                        (EVariable () (Label () "z"))
                        :| []
                    )
                    <| EClause
                      ()
                      (PConstructor () (Label () "Cons") [PVariable () (Label () "z"), PVariable () (Label () "ys")])
                      ( CPlain
                          ()
                          []
                          (EVariable () (Label () "z"))
                          :| []
                      )
                      :| []
                )
            )
            == (Set.fromList [Label () "z"])
        it "match(1) { | Cons(z, xs) => z | Cons(z, ys) => z }" $
          freeIn
            ( EMatch
                ()
                ()
                (ELiteral () (LInt32 1))
                ( EClause
                    ()
                    (PConstructor () (Label () "Cons") [PVariable () (Label () "z"), PVariable () (Label () "xs")])
                    ( CPlain
                        ()
                        []
                        (EVariable () (Label () "z"))
                        :| []
                    )
                    <| EClause
                      ()
                      (PConstructor () (Label () "Cons") [PVariable () (Label () "z"), PVariable () (Label () "ys")])
                      ( CPlain
                          ()
                          []
                          (EVariable () (Label () "z"))
                          :| []
                      )
                      :| []
                )
            )
            == (mempty :: Set (Label ()))
        it "match(1) { | Cons(z, xs) => z | Cons(z, ys) => a }" $
          freeIn
            ( EMatch
                ()
                ()
                (ELiteral () (LInt32 1))
                ( EClause
                    ()
                    (PConstructor () (Label () "Cons") [PVariable () (Label () "z"), PVariable () (Label () "xs")])
                    ( CPlain
                        ()
                        []
                        (EVariable () (Label () "z"))
                        :| []
                    )
                    <| EClause
                      ()
                      (PConstructor () (Label () "Cons") [PVariable () (Label () "z"), PVariable () (Label () "ys")])
                      ( CPlain
                          ()
                          []
                          (EVariable () (Label () "a"))
                          :| []
                      )
                      :| []
                )
            )
            == (Set.fromList [Label () "a"])




--      describe "EUnaryOperator" $
--        it "!x" $
--          freeIn (EApplication () (EUnaryOperator ((), OLogicalNot)) (EVariable (Label () "x") :| [])) == Set.fromList [Label () "x"]
--      describe "EBinaryOperator" $
--        it "x + y" $
--          freeIn (EApplication () (EBinaryOperator ((), OAddition)) (EVariable (Label () "x") <| EVariable (Label () "y") :| [])) == Set.fromList [Label () "x", Label () "y"]
--
