{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Kernel.Pipeline.Pass.LetBindingSimplificationSpec (spec) where

import Coal.Kernel.Language.Expr (Binding (..), Expr (..))
import Coal.Kernel.Language.Module (Module (..), moduleObjects)
import Coal.Kernel.Language.Object (FunctionScope (..), Object (..))
import Coal.Kernel.Language.Type (Type (..))
import Coal.Kernel.Pipeline.Invariant (checkLetBindingsSimplified)
import Coal.Kernel.Pipeline.Pass.LetBindingSimplification (letBindingSimplification)
import Coal.Kernel.Pipeline.Pass.TestHelpers (lbl, mkModule, runPass, unit_)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Test.Hspec (Spec, describe, it, shouldBe)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

ne :: [a] -> NonEmpty a
ne (x : xs) = x :| xs
ne [] = error "ne: empty list"

var :: Text -> Expr Type
var name = EVar (lbl name)

letBind :: [(Text, Expr Type)] -> Expr Type -> Expr Type
letBind pairs = ELet (ne (map (\(n, e) -> Binding (lbl n) e) pairs))

modWith :: Expr Type -> Module Type
modWith e = mkModule [DConstant "x" e]

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "letBindingSimplification" $ do
    describe "eliminates pure-alias lets" $ do
      it "let x = y in x  =>  y  (let dropped entirely)" $
        let input = modWith (letBind [("x", var "y")] (var "x"))
            expected = modWith (var "y")
         in runPass letBindingSimplification input `shouldBe` Right expected

      it "let x = y in z  =>  z  (x unused; alias dropped)" $
        let input = modWith (letBind [("x", var "y")] (var "z"))
            expected = modWith (var "z")
         in runPass letBindingSimplification input `shouldBe` Right expected

      it "let x = y; z = q in x  =>  y  (two aliases; only x used)" $
        let input = modWith (letBind [("x", var "y"), ("z", var "q")] (var "x"))
            expected = modWith (var "y")
         in runPass letBindingSimplification input `shouldBe` Right expected

    describe "chains aliases to final target" $ do
      it "let x = y; y = z in x  =>  z  (chain x->y->z)" $
        let input = modWith (letBind [("x", var "y"), ("y", var "z")] (var "x"))
            expected = modWith (var "z")
         in runPass letBindingSimplification input `shouldBe` Right expected

    describe "preserves non-alias bindings" $ do
      it "let x = unit in x  =>  let x = unit in x  (RHS is not EVar)" $
        let m = modWith (letBind [("x", unit_)] (var "x"))
         in runPass letBindingSimplification m `shouldBe` Right m

      it "let a = y; b = unit in a  =>  let b = unit in y  (alias a dropped; real b kept)" $
        let input = modWith (letBind [("a", var "y"), ("b", unit_)] (var "a"))
            expected = modWith (ELet (ne [Binding (lbl "b") unit_]) (var "y"))
         in runPass letBindingSimplification input `shouldBe` Right expected

    describe "patches variable references throughout" $ do
      it "alias reference inside let RHS is resolved" $
        -- let a = x; b = a in b  =>  let b = x in x (a alias resolved in b's RHS)
        -- Actually a = x (alias), b = a (alias to a which maps to x), body = b
        -- Both a and b are aliases: a->x, b->x, body becomes x
        let input = modWith (letBind [("a", var "x"), ("b", var "a")] (var "b"))
            expected = modWith (var "x")
         in runPass letBindingSimplification input `shouldBe` Right expected

    describe "invariant: checkLetBindingsSimplified [] on every object body" $ do
      it "after eliminating a trivial alias" $
        let input = mkModule [DConstant "x" (letBind [("a", var "b")] (var "a"))]
         in case runPass letBindingSimplification input of
              Left err -> fail (show err)
              Right m ->
                concatMap
                  ( \case
                      DFunction _ _ _ body -> checkLetBindingsSimplified body
                      DConstant _ body -> checkLetBindingsSimplified body
                      _ -> []
                  )
                  (moduleObjects m)
                  `shouldBe` []

      it "after eliminating a mixed alias+real let" $
        let input = mkModule [DConstant "x" (letBind [("a", var "b"), ("c", unit_)] (var "a"))]
         in case runPass letBindingSimplification input of
              Left err -> fail (show err)
              Right m ->
                concatMap
                  ( \case
                      DFunction _ _ _ body -> checkLetBindingsSimplified body
                      DConstant _ body -> checkLetBindingsSimplified body
                      _ -> []
                  )
                  (moduleObjects m)
                  `shouldBe` []
