{-# LANGUAGE OverloadedStrings #-}

module Coal.Kernel.FreeVarsSpec (spec) where

import Coal.Kernel.FreeVars (freeVars)
import Coal.Kernel.Language.Expr (Binding (..), Clause (..), Expr (..), Label (..))
import Coal.Kernel.Language.Op (Op (..))
import Coal.Kernel.Language.Prim (Prim (..))
import Coal.Kernel.Language.Type (Type (..))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set
import Data.Text (Text)
import Test.Hspec (Spec, describe, it, shouldBe)

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | A placeholder expression.
unit_ :: Expr Type
unit_ = ELit PUnit

-- | Construct a label with an opaque type annotation.
lbl :: Text -> Label Type
lbl = Label TOpq

-- | @let name = rhs in body@
letOne :: Text -> Expr Type -> Expr Type -> Expr Type
letOne name rhs = ELet (Binding (lbl name) rhs :| [])

-- | @fn(p) => body@
lam1 :: Text -> Expr Type -> Expr Type
lam1 p = ELam (lbl p :| [])

-- | @fn(p, q) => body@
lam2 :: Text -> Text -> Expr Type -> Expr Type
lam2 p q = ELam (lbl p :| [lbl q])

-- ---------------------------------------------------------------------------
-- Tests
-- ---------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "freeVars" $ do
    describe "base cases" $ do
      it "literal has no free variables" $
        freeVars unit_
          `shouldBe` Set.empty

      it "empty record has no free variables" $
        freeVars (ENil :: Expr Type)
          `shouldBe` Set.empty

      it "constructor has no free variables" $
        freeVars (ECon (lbl "Just"))
          `shouldBe` Set.empty

      it "variable reference is free" $
        freeVars (EVar (lbl "x"))
          `shouldBe` Set.singleton (lbl "x")

    describe "operators" $ do
      it "operator operands contribute free variables" $
        freeVars (EOp (OAddInt32 (EVar (lbl "x")) (EVar (lbl "y"))))
          `shouldBe` Set.fromList [lbl "x", lbl "y"]

      it "nested operators accumulate free variables" $
        freeVars (EOp (OAddInt32 (EOp (OMulInt32 (EVar (lbl "x")) (EVar (lbl "y")))) (EVar (lbl "z"))))
          `shouldBe` Set.fromList [lbl "x", lbl "y", lbl "z"]

    describe "application" $ do
      it "function and arguments contribute free variables" $
        freeVars (EApp TOpq (EVar (lbl "f")) (EVar (lbl "x") :| [EVar (lbl "y")]))
          `shouldBe` Set.fromList [lbl "f", lbl "x", lbl "y"]

      it "duplicate variable appears only once in result" $
        freeVars (EApp TOpq (EVar (lbl "f")) (EVar (lbl "x") :| [EVar (lbl "x")]))
          `shouldBe` Set.fromList [lbl "f", lbl "x"]

    describe "if-then-else" $ do
      it "all three branches contribute free variables" $
        freeVars (EIf (EVar (lbl "c")) (EVar (lbl "t")) (EVar (lbl "f")))
          `shouldBe` Set.fromList [lbl "c", lbl "t", lbl "f"]

    describe "records" $ do
      it "record extension: both expressions contribute free variables" $
        freeVars (EExt "field" (EVar (lbl "x")) (EVar (lbl "y")))
          `shouldBe` Set.fromList [lbl "x", lbl "y"]

      it "record projection: field name is not a variable" $
        freeVars (EGet (lbl "field") (EVar (lbl "r")))
          `shouldBe` Set.singleton (lbl "r")

    describe "lambda" $ do
      it "parameter is bound in body" $
        -- fn(x) => x
        freeVars (lam1 "x" (EVar (lbl "x")))
          `shouldBe` Set.empty

      it "free variable in body is preserved" $
        -- fn(x) => y
        freeVars (lam1 "x" (EVar (lbl "y")))
          `shouldBe` Set.singleton (lbl "y")

      it "Example 2 from spec: fn(x) => add(x, y)" $
        freeVars
          ( lam1
              "x"
              (EApp TOpq (EVar (lbl "add")) (EVar (lbl "x") :| [EVar (lbl "y")]))
          )
          `shouldBe` Set.fromList [lbl "add", lbl "y"]

      it "multiple parameters are all bound" $
        -- fn(x, y) => add(x, y)
        freeVars
          ( lam2
              "x"
              "y"
              (EApp TOpq (EVar (lbl "add")) (EVar (lbl "x") :| [EVar (lbl "y")]))
          )
          `shouldBe` Set.singleton (lbl "add")

      it "parameter used at a different type annotation is still bound" $
        -- fn(x : T1) => x viewed as T2  (T1 /= T2)
        -- The body contains EVar (Label TCon "Bool" [] "x") but the
        -- binder declares x as TOpq.  x must not appear as a free variable.
        freeVars
          (ELam (Label TOpq "x" :| []) (EVar (Label (TCon "Bool" []) "x")))
          `shouldBe` Set.empty

      it "Example 5 from spec: nested lambdas" $
        -- fn(x) => fn(y) => add(x, y, z)
        freeVars
          ( lam1
              "x"
              ( lam1
                  "y"
                  ( EApp
                      TOpq
                      (EVar (lbl "add"))
                      (EVar (lbl "x") :| [EVar (lbl "y"), EVar (lbl "z")])
                  )
              )
          )
          `shouldBe` Set.fromList [lbl "add", lbl "z"]

    describe "let" $ do
      it "bound variable is not free" $
        -- let x = () in x
        freeVars (letOne "x" unit_ (EVar (lbl "x")))
          `shouldBe` Set.empty

      it "free variable in binding expression" $
        -- let x = y in ()
        freeVars (letOne "x" (EVar (lbl "y")) unit_)
          `shouldBe` Set.singleton (lbl "y")

      it "free variable in body" $
        -- let x = () in y
        freeVars (letOne "x" unit_ (EVar (lbl "y")))
          `shouldBe` Set.singleton (lbl "y")

      it "Example 3 from spec: let x = y in add(x, z)" $
        freeVars
          ( letOne
              "x"
              (EVar (lbl "y"))
              (EApp TOpq (EVar (lbl "add")) (EVar (lbl "x") :| [EVar (lbl "z")]))
          )
          `shouldBe` Set.fromList [lbl "add", lbl "y", lbl "z"]

      it "recursive let: bound name shadows in binding expression" $
        -- let x = x in ()
        freeVars (letOne "x" (EVar (lbl "x")) unit_)
          `shouldBe` Set.empty

      it "let-bound variable used at a different type annotation is still bound" $
        -- let x : T1 = () in x viewed as T2
        freeVars
          (ELet (Binding (Label TOpq "x") unit_ :| []) (EVar (Label (TCon "Bool" []) "x")))
          `shouldBe` Set.empty

      it "multiple bindings: all names bound in all bindings and body" $
        freeVars
          ( ELet
              ( Binding (lbl "x") (EVar (lbl "y"))
                  :| [Binding (lbl "y") (EVar (lbl "x"))]
              )
              (EApp TOpq (EVar (lbl "f")) (EVar (lbl "x") :| [EVar (lbl "y")]))
          )
          `shouldBe` Set.singleton (lbl "f")

    describe "case" $ do
      it "scrutinee contributes free variables" $
        freeVars
          ( ECase
              TOpq
              (EVar (lbl "xs"))
              (Clause (lbl "Nil" :| []) unit_ :| [])
          )
          `shouldBe` Set.singleton (lbl "xs")

      it "constructor is not a free variable" $
        freeVars
          ( ECase
              TOpq
              unit_
              (Clause (lbl "Cons" :| [lbl "x", lbl "xs"]) (EVar (lbl "x")) :| [])
          )
          `shouldBe` Set.empty

      it "pattern variables are bound in clause body" $
        freeVars
          ( ECase
              TOpq
              unit_
              (Clause (lbl "Cons" :| [lbl "x", lbl "xs"]) (EVar (lbl "x")) :| [])
          )
          `shouldBe` Set.empty

      it "free variable in clause body" $
        freeVars
          ( ECase
              TOpq
              unit_
              (Clause (lbl "Cons" :| [lbl "x", lbl "xs"]) (EVar (lbl "y")) :| [])
          )
          `shouldBe` Set.singleton (lbl "y")

      it "Example 4 from spec" $
        -- case xs of
        --   Cons x xs1 -> add(x, y)
        --   Nil -> z
        freeVars
          ( ECase
              TOpq
              (EVar (lbl "xs"))
              ( Clause
                  (lbl "Cons" :| [lbl "x", lbl "xs1"])
                  (EApp TOpq (EVar (lbl "add")) (EVar (lbl "x") :| [EVar (lbl "y")]))
                  :| [ Clause
                         (lbl "Nil" :| [])
                         (EVar (lbl "z"))
                     ]
              )
          )
          `shouldBe` Set.fromList [lbl "xs", lbl "add", lbl "y", lbl "z"]

      it "multiple clauses with different pattern variables" $
        freeVars
          ( ECase
              TOpq
              unit_
              ( Clause (lbl "A" :| [lbl "x"]) (EVar (lbl "x"))
                  :| [Clause (lbl "B" :| [lbl "y"]) (EVar (lbl "y"))]
              )
          )
          `shouldBe` Set.empty

      it "pattern variable used at a different type annotation is still bound" $
        -- case () of A x -> (x viewed as Bool)
        freeVars
          ( ECase
              TOpq
              unit_
              (Clause (lbl "A" :| [Label TOpq "x"]) (EVar (Label (TCon "Bool" []) "x")) :| [])
          )
          `shouldBe` Set.empty
