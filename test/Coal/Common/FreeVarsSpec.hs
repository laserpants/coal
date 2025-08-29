{-# LANGUAGE OverloadedStrings #-}

module Coal.Common.FreeVarsSpec where

import Coal.Common.FreeVars
import Coal.Common.Label (Label (..))
import Coal.Common.List1
import Coal.Common.List1 (NonEmpty (..))
import Coal.Language
import Coal.TypeSystem.Substitution
import Coal.TypeSystem.Unification
import Control.Monad (forM_)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Extra (Name)
import Prettyprinter
import Prettyprinter.Render.String (renderString)
import Test.Hspec

import qualified Coal.Common.List1 as List1
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

freeVarsSpec :: Spec
freeVarsSpec = do
  describe "BoundVars" $ do
    it "collects bound vars from a Label" $ do
      boundIn (Label () "x") `shouldBe` Set.singleton "x"

    it "collects bound vars from Maybe" $ do
      boundIn (Just (Label () "y")) `shouldBe` Set.singleton "y"

    it "collects bound vars from []" $ do
      boundIn [Label () "a", Label () "b"] `shouldBe` Set.fromList ["a", "b"]

    it "collects bound vars from NonEmpty" $ do
      let xs = Label () "m" :| [Label () "n"]
      boundIn xs `shouldBe` Set.fromList ["m", "n"]

  describe "FreeVars" $ do
    it "collects free vars from a Label" $ do
      freeIn (Label () "z") `shouldBe` Set.singleton (Label () "z")

    it "collects free vars from Maybe" $ do
      freeIn (Just (Label () "q")) `shouldBe` Set.singleton (Label () "q")

    it "collects free vars from []" $ do
      freeIn [Label () "u", Label () "v"] `shouldBe` Set.fromList [Label () "u", Label () "v"]

    it "collects free vars from NonEmpty" $ do
      let xs = Label () "s" :| [Label () "t"]
      freeIn xs `shouldBe` Set.fromList [Label () "s", Label () "t"]

    it "collects free vars from Map" $ do
      let m = Map.fromList [(1, Label () "k"), (2, Label () "l")]
      freeIn m `shouldBe` Set.fromList [Label () "k", Label () "l"]

    it "collects free vars from Set" $ do
      let s = Set.fromList [Label () "p", Label () "r"]
      freeIn s `shouldBe` Set.fromList [Label () "p", Label () "r"]

  describe "exceptNames" $ do
    it "filters out bound names from free vars" $ do
      let frees = Set.fromList [Label () "a", Label () "b", Label () "c"]
          bound = ["b", "c"]
      exceptNames frees bound `shouldBe` Set.singleton (Label () "a")

    it "keeps all free vars if no names are bound" $ do
      let frees = Set.fromList [Label () "x", Label () "y"]
      exceptNames frees [] `shouldBe` frees

  describe "notOneOf" $ do
    it "returns True if label not in set" $ do
      Label () "foo" `notOneOf` ["bar", "baz"] `shouldBe` True

    it "returns False if label is in set" $ do
      Label () "foo" `notOneOf` ["foo", "baz"] `shouldBe` False

  describe "notConstructor" $ do
    it "filters out constructor-like names" $ do
      notConstructor (Label () "Xyz") `shouldBe` False
      notConstructor (Label () "abc") `shouldBe` True

  describe "freeSet" $ do
    it "removes constructors from free vars" $ do
      let frees = [Label () "Foo", Label () "bar"]
      freeSet [] frees `shouldBe` Set.singleton (Label () "bar")

    it "removes bound names from free vars" $ do
      let frees = [Label () "m", Label () "n"]
      freeSet ["n"] frees `shouldBe` Set.singleton (Label () "m")

    it "removes both constructors and bound names" $ do
      let frees = [Label () "Aaa", Label () "bbb", Label () "ccc"]
      freeSet ["ccc"] frees `shouldBe` Set.singleton (Label () "bbb")

  describe "Tricky recursive cases" $ do
    it "nested Maybe of Lists of Labels" $ do
      let obj = Just [Label () "a", Label () "b"]
      freeIn obj `shouldBe` Set.fromList [Label () "a", Label () "b"]
      boundIn obj `shouldBe` Set.fromList ["a", "b"]

    it "exceptNames filters deeply nested freeIn results" $ do
      let obj = Just [Label () "foo", Label () "bar"]
      let frees = freeIn obj
      exceptNames frees ["foo"] `shouldBe` Set.singleton (Label () "bar")

    it "freeSet with overlapping constructors and normal vars" $ do
      let obj = [Label () "Alpha", Label () "beta", Label () "Gamma"]
      freeSet [] obj `shouldBe` Set.singleton (Label () "beta")

  it "lambda binds its variable" $ do
    let expr =
          ELambda
            ()
            (PVariable () (Label () "x") :| [])
            (EVariable () (Label () "x"))
    freeIn expr `shouldBe` (Set.empty :: Set (Label ()))

  it "lambda does not bind external variables" $ do
    let expr =
          ELambda
            ()
            (PVariable () (Label () "x") :| [])
            (EVariable () (Label () "y"))
    freeIn expr `shouldBe` Set.singleton (Label () "y")

  it "let binding introduces bound var, expr may use external var" $ do
    let binding = BPattern () (PVariable () (Label () "x")) (EVariable () (Label () "y"))
        expr =
          ELet
            ()
            (binding :| [])
            (EVariable () (Label () "x"))
    freeIn expr `shouldBe` Set.singleton (Label () "y")

  it "recursive let binds the pattern in both rhs and body" $ do
    let expr =
          ERecursiveLet
            ()
            (PVariable () (Label () "f"))
            (EVariable () (Label () "f"))
            (EVariable () (Label () "g"))
    freeIn expr `shouldBe` Set.singleton (Label () "g")

  it "pattern match binds its pattern vars" $ do
    let clause =
          EClause
            ()
            (PVariable () (Label () "y"))
            (CPlain () [] (EVariable () (Label () "y")) :| [])
        expr = EMatch () () (EVariable () (Label () "x")) (clause :| [])
    freeIn expr `shouldBe` Set.singleton (Label () "x")

  it "constructor is not free" $ do
    let expr = EConstructor () (Label () "Just")
    freeIn expr `shouldBe` (Set.empty :: Set (Label ()))

  it "record literal includes free vars from fields" $ do
    let expr =
          ERecord
            ()
            ()
            (Map.fromList [("a", EVariable () (Label () "x"))])
            (Just (EConstructor () (Label () "Nothing")))
    freeIn expr `shouldBe` Set.singleton (Label () "x")

--  it "choice lambda binds its params" $ do
--    let choice =
--          CLambda
--            ()
--            (PVariable () (Label () "y") :| [])
--            [CGuard (EVariable () (Label () "y"))]
--            (EVariable () (Label () "z"))
--        clause = EClause () (PVariable () (Label () "z")) (choice :| [])
--        expr = EMatch () () (EVariable () (Label () "q")) (clause :| [])
--    freeIn expr `shouldBe` Set.singleton (Label () "q")
