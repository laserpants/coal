{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Kernel.Pipeline.Pass.AdministrativeNormalFormSpec (spec) where

import Coal.Kernel.Language.Expr (Binding (..), Clause (..), Expr (..), Label (..))
import Coal.Kernel.Language.Module (Module (..), moduleObjects)
import Coal.Kernel.Language.Object (FunctionScope (..), Object (..))
import Coal.Kernel.Language.Op (Op (..))
import Coal.Kernel.Language.Type (Type (..))
import Coal.Kernel.Pipeline.Invariant (checkAdministrativeNormalForm)
import Coal.Kernel.Pipeline.Pass.AdministrativeNormalForm (administrativeNormalForm)
import Coal.Kernel.Pipeline.Pass.TestHelpers (lbl, mkModule, ne, runPass, unit_, var)
import Test.Hspec (Spec, describe, it, shouldBe)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | A function application expression (non-atomic).
app :: Expr Type -> [Expr Type] -> Expr Type
app f args = EApp TOpq f (ne args)

-- | An addition-op expression (non-atomic).
addExpr :: Expr Type -> Expr Type -> Expr Type
addExpr a b = EOp (OAddInt32 a b)

-- | Module with a single zero-param function body.
modWith :: Expr Type -> Module Type
modWith e = mkModule [DFunction Local "f" [] e]

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "administrativeNormalForm" $ do
    describe "leaves already-atomic operands unchanged" $ do
      it "variable (atomic): unchanged" $
        let m = modWith (var "x")
         in runPass administrativeNormalForm m `shouldBe` Right m

      it "literal unit (atomic): unchanged" $
        let m = modWith unit_
         in runPass administrativeNormalForm m `shouldBe` Right m

      it "EApp with atomic function and atomic args: unchanged" $
        let m = modWith (EApp TOpq (var "f") (ne [var "x", var "y"]))
         in runPass administrativeNormalForm m `shouldBe` Right m

      it "if with atomic cond and atomic branches: unchanged" $
        let m = modWith (EIf (var "b") (var "x") (var "y"))
         in runPass administrativeNormalForm m `shouldBe` Right m

    describe "extracts non-atomic operands into let-bindings" $ do
      it "f(g(x))  =>  let anf.0 = g(x) in f(anf.0)" $
        let gx = app (var "g") [var "x"]
            input = modWith (app (var "f") [gx])
            a0 = Label TOpq "anf.0"
            expected =
              modWith
                ( ELet
                    (ne [Binding a0 gx])
                    (EApp TOpq (var "f") (ne [EVar a0]))
                )
         in runPass administrativeNormalForm input `shouldBe` Right expected

      it "f(g(x), h(y))  =>  let anf.0 = g(x) in let anf.1 = h(y) in f(anf.0, anf.1)" $
        -- Each non-atomic arg is extracted into its own single-binding let.
        let gx = app (var "g") [var "x"]
            hy = app (var "h") [var "y"]
            input = modWith (app (var "f") [gx, hy])
            a0 = Label TOpq "anf.0"
            a1 = Label TOpq "anf.1"
            expected =
              modWith
                ( ELet
                    (ne [Binding a0 gx])
                    ( ELet
                        (ne [Binding a1 hy])
                        (EApp TOpq (var "f") (ne [EVar a0, EVar a1]))
                    )
                )
         in runPass administrativeNormalForm input `shouldBe` Right expected

    describe "extracts non-atomic if-conditions" $ do
      it "if (f(x)) then y else z  =>  let anf.0 = f(x) in if anf.0 then y else z" $
        let cond = app (var "f") [var "x"]
            input = modWith (EIf cond (var "y") (var "z"))
            a0 = Label TOpq "anf.0"
            expected =
              modWith
                ( ELet
                    (ne [Binding a0 cond])
                    (EIf (EVar a0) (var "y") (var "z"))
                )
         in runPass administrativeNormalForm input `shouldBe` Right expected

    describe "extracts non-atomic operator operands" $ do
      it "add(f(x), y)  =>  let anf.0 = f(x) in add(anf.0, y)" $
        let fx = app (var "f") [var "x"]
            input = modWith (addExpr fx (var "y"))
            a0 = Label TOpq "anf.0"
            expected =
              modWith
                ( ELet
                    (ne [Binding a0 fx])
                    (addExpr (EVar a0) (var "y"))
                )
         in runPass administrativeNormalForm input `shouldBe` Right expected

    describe "binds control flow in let-binding RHS (no floating)" $ do
      it "let x = if b then a else c in body  stays as  let x = if b then a else c in body" $
        let body = var "body"
            input =
              modWith
                ( ELet
                    (ne [Binding (lbl "x") (EIf (var "b") (var "a") (var "c"))])
                    body
                )
         in runPass administrativeNormalForm input `shouldBe` Right input

      it "let x = case e of Foo => a; Bar => b in body  stays as  let x = case e of Foo => a; Bar => b in body" $
        let body = var "body"
            scrutinee = var "e"
            clauseFoo = Clause (ne [lbl "Foo"]) (var "a")
            clauseBar = Clause (ne [lbl "Bar"]) (var "b")
            input =
              modWith
                ( ELet
                    (ne [Binding (lbl "x") (ECase TOpq scrutinee (ne [clauseFoo, clauseBar]))])
                    body
                )
         in runPass administrativeNormalForm input `shouldBe` Right input

    describe "binds control flow in function argument position (wr a fresh let)" $ do
      it "f(if b then a else c): atomic branches  =>  let anf.0 = if b then a else c in f(anf.0)" $
        -- Control flow is bound to a fresh variable; the continuation (f(...)) is
        -- applied exactly once, not duplicated into each branch.
        let a0 = Label TOpq "anf.0"
            input = modWith (app (var "f") [EIf (var "b") (var "a") (var "c")])
            expected =
              modWith
                ( ELet
                    (ne [Binding a0 (EIf (var "b") (var "a") (var "c"))])
                    (EApp TOpq (var "f") (ne [EVar a0]))
                )
         in runPass administrativeNormalForm input `shouldBe` Right expected

      it "f(if b then g(a) else h(c))  =>  let anf.0 = if b then g(a) else h(c) in f(anf.0)" $
        let ga = app (var "g") [var "a"]
            hc = app (var "h") [var "c"]
            a0 = Label TOpq "anf.0"
            input = modWith (app (var "f") [EIf (var "b") ga hc])
            expected =
              modWith
                ( ELet
                    (ne [Binding a0 (EIf (var "b") ga hc)])
                    (EApp TOpq (var "f") (ne [EVar a0]))
                )
         in runPass administrativeNormalForm input `shouldBe` Right expected

    describe "invariant: checkAdministrativeNormalForm [] on every object body" $ do
      it "after ANF-ing f(g(x))" $
        let input = mkModule [DFunction Local "f" [] (app (var "f") [app (var "g") [var "x"]])]
         in case runPass administrativeNormalForm input of
              Left err -> fail (show err)
              Right m ->
                concatMap
                  ( \case
                      DFunction _ _ _ body -> checkAdministrativeNormalForm body
                      DConstant _ body -> checkAdministrativeNormalForm body
                      _ -> []
                  )
                  (moduleObjectsList m)
                  `shouldBe` []

      it "after ANF-ing if (f(x)) then g(y) else z" $
        let cond = app (var "f") [var "x"]
            th = app (var "g") [var "y"]
            input = mkModule [DFunction Local "f" [] (EIf cond th (var "z"))]
         in case runPass administrativeNormalForm input of
              Left err -> fail (show err)
              Right m ->
                concatMap
                  ( \case
                      DFunction _ _ _ body -> checkAdministrativeNormalForm body
                      DConstant _ body -> checkAdministrativeNormalForm body
                      _ -> []
                  )
                  (moduleObjectsList m)
                  `shouldBe` []

      it "after ANF-ing add(f(x), g(y))" $
        let input = mkModule [DConstant "c" (addExpr (app (var "f") [var "x"]) (app (var "g") [var "y"]))]
         in case runPass administrativeNormalForm input of
              Left err -> fail (show err)
              Right m ->
                concatMap
                  ( \case
                      DFunction _ _ _ body -> checkAdministrativeNormalForm body
                      DConstant _ body -> checkAdministrativeNormalForm body
                      _ -> []
                  )
                  (moduleObjectsList m)
                  `shouldBe` []

      it "after ANF-ing let x = if b then a else c in body" $
        let input =
              mkModule
                [ DFunction
                    Local
                    "f"
                    []
                    ( ELet
                        (ne [Binding (lbl "x") (EIf (var "b") (var "a") (var "c"))])
                        (var "body")
                    )
                ]
         in case runPass administrativeNormalForm input of
              Left err -> fail (show err)
              Right m ->
                concatMap
                  ( \case
                      DFunction _ _ _ body -> checkAdministrativeNormalForm body
                      DConstant _ body -> checkAdministrativeNormalForm body
                      _ -> []
                  )
                  (moduleObjectsList m)
                  `shouldBe` []

      it "after ANF-ing f(if b then a else c)" $
        let input = mkModule [DFunction Local "f" [] (app (var "f") [EIf (var "b") (var "a") (var "c")])]
         in case runPass administrativeNormalForm input of
              Left err -> fail (show err)
              Right m ->
                concatMap
                  ( \case
                      DFunction _ _ _ body -> checkAdministrativeNormalForm body
                      DConstant _ body -> checkAdministrativeNormalForm body
                      _ -> []
                  )
                  (moduleObjectsList m)
                  `shouldBe` []

-- helpers
moduleObjectsList :: Module t -> [Object t]
moduleObjectsList = moduleObjects
