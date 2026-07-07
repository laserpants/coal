{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Kernel.Pipeline.Pass.LocalNameCanonicalizationSpec (spec) where

import Coal.Kernel.Language.Expr (Binding (..), Clause (..), Expr (..), Label (..))
import Coal.Kernel.Language.Module (Module (..), moduleObjects)
import Coal.Kernel.Language.Object (FunctionScope (..), Object (..))
import Coal.Kernel.Language.Type (Type (..))
import Coal.Kernel.Pipeline.Invariant (checkLocalNamesUnique)
import Coal.Kernel.Pipeline.Pass.LocalNameCanonicalization (localNameCanonicalization)
import Coal.Kernel.Pipeline.Pass.TestHelpers (lbl, mkModule, runPass, unit_)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Test.Hspec (Spec, describe, it, shouldBe)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------
-- Fresh names follow the pattern "<original>.<counter>" where the counter
-- starts at 0 and increments globally (not per-binding).

ne :: [a] -> NonEmpty a
ne (x : xs) = x :| xs
ne [] = error "ne: empty list"

-- | Variable reference by label.
var :: Text -> Expr Type
var n = EVar (lbl n)

letBind :: [(Text, Expr Type)] -> Expr Type -> Expr Type
letBind pairs = ELet (ne (map (\(n, e) -> Binding (lbl n) e) pairs))

letBindRenamed :: [(Text, Expr Type)] -> Expr Type -> Expr Type
letBindRenamed pairs = ELet (ne (map (\(n, e) -> Binding (Label TOpq n) e) pairs))

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "localNameCanonicalization" $ do
    describe "renames let-bound names" $ do
      it "let x = unit in x  =>  let x.0 = unit in EVar x.0" $
        let input = mkModule [DConstant "c" (letBind [("x", unit_)] (var "x"))]
            expected =
              mkModule
                [ DConstant
                    "c"
                    (ELet (ne [Binding (Label TOpq "x.0") unit_]) (EVar (Label TOpq "x.0")))
                ]
         in runPass localNameCanonicalization input `shouldBe` Right expected

      it "two nested lets: x (counter 0) then y (counter 1)" $
        let input = mkModule [DConstant "c" (letBind [("x", unit_)] (letBind [("y", unit_)] (var "y")))]
            expected =
              mkModule
                [ DConstant
                    "c"
                    ( ELet
                        (ne [Binding (Label TOpq "x.0") unit_])
                        ( ELet
                            (ne [Binding (Label TOpq "y.1") unit_])
                            (EVar (Label TOpq "y.1"))
                        )
                    )
                ]
         in runPass localNameCanonicalization input `shouldBe` Right expected

    describe "renames lambda parameters" $ do
      it "fn(x) => x  =>  fn(x.0) => x.0" $
        let input = mkModule [DConstant "c" (ELam (ne [lbl "x"]) (var "x"))]
            expected =
              mkModule
                [ DConstant
                    "c"
                    (ELam (ne [Label TOpq "x.0"]) (EVar (Label TOpq "x.0")))
                ]
         in runPass localNameCanonicalization input `shouldBe` Right expected

      it "fn(x, y) => x  =>  fn(x.0, y.1) => x.0  (sequential counters)" $
        let input = mkModule [DConstant "c" (ELam (ne [lbl "x", lbl "y"]) (var "x"))]
            expected =
              mkModule
                [ DConstant
                    "c"
                    ( ELam
                        (ne [Label TOpq "x.0", Label TOpq "y.1"])
                        (EVar (Label TOpq "x.0"))
                    )
                ]
         in runPass localNameCanonicalization input `shouldBe` Right expected

    describe "renames case pattern variables" $ do
      it "pattern variable in a clause is renamed; constructor label unchanged" $
        -- case unit of | Cons(head, tail) => unit
        -- becomes: case unit of | Cons(head.0, tail.1) => unit
        let input =
              mkModule
                [ DConstant
                    "c"
                    (ECase TOpq unit_ (ne [Clause (ne [lbl "Cons", lbl "head", lbl "tail"]) unit_]))
                ]
            expected =
              mkModule
                [ DConstant
                    "c"
                    (ECase TOpq unit_ (ne [Clause (ne [lbl "Cons", Label TOpq "head.0", Label TOpq "tail.1"]) unit_]))
                ]
         in runPass localNameCanonicalization input `shouldBe` Right expected

    describe "does NOT rename top-level or constructor names" $ do
      it "top-level function name is unchanged" $
        let input = mkModule [DFunction Exported "myFunc" [] unit_]
         in case runPass localNameCanonicalization input of
              Left e -> fail (show e)
              Right m -> case moduleObjects m of
                [DFunction _ name _ _] -> name `shouldBe` "myFunc"
                _ -> fail "unexpected module structure"

      it "top-level function parameters are NOT renamed" $
        -- DFunction params are top-level bound names, not local binders
        let input = mkModule [DFunction Exported "f" [lbl "x"] (var "x")]
         in case runPass localNameCanonicalization input of
              Left e -> fail (show e)
              Right m -> case moduleObjects m of
                [DFunction _ _ params _] ->
                  map (\(Label _ n) -> n) params `shouldBe` ["x"]
                _ -> fail "unexpected module structure"

      it "constructor name in ECon is unchanged" $
        let input = mkModule [DConstant "c" (ECon (lbl "Just"))]
         in runPass localNameCanonicalization input
              `shouldBe` Right (mkModule [DConstant "c" (ECon (lbl "Just"))])

    describe "invariant: checkLocalNamesUnique [] on every object body (after two distinct binders)" $ do
      it "two lets with same original name get distinct fresh names" $
        let input =
              mkModule
                [ DConstant
                    "c"
                    (letBind [("x", unit_)] (letBind [("x", unit_)] (var "x")))
                ]
         in case runPass localNameCanonicalization input of
              Left e -> fail (show e)
              Right m ->
                concatMap
                  ( \case
                      DFunction _ _ _ body -> checkLocalNamesUnique body
                      DConstant _ body -> checkLocalNamesUnique body
                      _ -> []
                  )
                  (moduleObjects m)
                  `shouldBe` []

      it "lambda param shadowing a let binder becomes unique after canonicalization" $
        let input =
              mkModule
                [ DConstant
                    "c"
                    (letBind [("x", unit_)] (ELam (ne [lbl "x"]) (var "x")))
                ]
         in case runPass localNameCanonicalization input of
              Left e -> fail (show e)
              Right m ->
                concatMap
                  ( \case
                      DFunction _ _ _ body -> checkLocalNamesUnique body
                      DConstant _ body -> checkLocalNamesUnique body
                      _ -> []
                  )
                  (moduleObjects m)
                  `shouldBe` []
