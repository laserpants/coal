{-# LANGUAGE OverloadedStrings #-}

module Noll.Compiler.Transform.TreeSpec where

import Data.List.NonEmpty (NonEmpty (..), (<|))
import Noll.Compiler.Transform.Tree
import Noll.Label (Label (..))
import Noll.Language
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "Noll.Compiler.Transform.Tree" $ do
    describe "rename" $ do
      it "x" $
        rename "x" "y" (EVariable () (Label () "x")) == EVariable () (Label () "y")
      it "y" $
        rename "x" "y" (EVariable () (Label () "y")) == EVariable () (Label () "y")
      it "z" $
        rename "x" "y" (EVariable () (Label () "z")) == EVariable () (Label () "z")
      it "1" $
        rename "x" "y" (ELiteral () (LInt32 1)) == (ELiteral () (LInt32 1) :: Expression () (Type TypeIndex ()))
      it "if x then x else x" $
        rename "x" "y" (EIf () () (EVariable () (Label () "x")) (EVariable () (Label () "x")) (EVariable () (Label () "x"))) == EIf () () (EVariable () (Label () "y")) (EVariable () (Label () "y")) (EVariable () (Label () "y"))
      it "fn(x) => x" $
        rename "x" "y" (ELambda () (PVariable () (Label () "x") :| []) (EVariable () (Label () "x"))) == ELambda () (PVariable () (Label () "x") :| []) (EVariable () (Label () "x"))
      it "fn(y) => x" $
        rename "x" "y" (ELambda () (PVariable () (Label () "y") :| []) (EVariable () (Label () "x"))) == ELambda () (PVariable () (Label () "y") :| []) (EVariable () (Label () "y"))
      it "let a = a; b = b in a" $
        rename
          "a"
          "x"
          ( ELet
              ()
              ( BPattern () (PVariable () (Label () "a")) (EVariable () (Label () "a"))
                  <| BPattern () (PVariable () (Label () "b")) (EVariable () (Label () "b"))
                    :| []
              )
              (EVariable () (Label () "a"))
          )
          == ( ELet
                ()
                ( BPattern () (PVariable () (Label () "a")) (EVariable () (Label () "x"))
                    <| BPattern () (PVariable () (Label () "b")) (EVariable () (Label () "b"))
                      :| []
                )
                (EVariable () (Label () "a"))
             )
      it "let a = z; b = z in a" $
        rename
          "a"
          "x"
          ( ELet
              ()
              ( BPattern () (PVariable () (Label () "a")) (EVariable () (Label () "z"))
                  <| BPattern () (PVariable () (Label () "b")) (EVariable () (Label () "z"))
                    :| []
              )
              (EVariable () (Label () "a"))
          )
          == ( ELet
                ()
                ( BPattern () (PVariable () (Label () "a")) (EVariable () (Label () "z"))
                    <| BPattern () (PVariable () (Label () "b")) (EVariable () (Label () "z"))
                      :| []
                )
                (EVariable () (Label () "a"))
             )
      it "let c = z; d = z in a" $
        rename
          "a"
          "x"
          ( ELet
              ()
              ( BPattern () (PVariable () (Label () "c")) (EVariable () (Label () "z"))
                  <| BPattern () (PVariable () (Label () "d")) (EVariable () (Label () "z"))
                    :| []
              )
              (EVariable () (Label () "a"))
          )
          == ( ELet
                ()
                ( BPattern () (PVariable () (Label () "c")) (EVariable () (Label () "z"))
                    <| BPattern () (PVariable () (Label () "d")) (EVariable () (Label () "z"))
                      :| []
                )
                (EVariable () (Label () "x"))
             )
