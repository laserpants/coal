{-# LANGUAGE OverloadedStrings #-}

module Coal.Kernel.Parser.ExprSpec (spec) where

import Coal.Kernel.Language.Expr (Binding (..), Clause (..), Expr (..), Label (..))
import Coal.Kernel.Language.Op (Op (..))
import Coal.Kernel.Language.Prim (Prim (..))
import Coal.Kernel.Language.Type (Type (..))
import qualified Coal.Kernel.Language.Type.Constructors as T
import Coal.Kernel.Parser.Expr (expr, label)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as Text
import Data.Void (Void)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec (ParseErrorBundle, parse)

-- | Helper to parse an expression
parseExpr :: Text.Text -> Either (ParseErrorBundle Text.Text Void) (Expr Type)
parseExpr = parse expr ""

-- | Helper to parse a label
parseLabel :: Text.Text -> Either (ParseErrorBundle Text.Text Void) (Label Type)
parseLabel = parse label ""

-- | Helper to check if result is Left
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False

-- | Test specification for the Expr parser
spec :: Spec
spec = do
  describe "expr parser" $ do
    describe "labels" $ do
      it "parses 'x : int32'" $
        parseLabel "x : int32" `shouldBe` Right (Label T.int32 "x")

      it "parses 'name : bool'" $
        parseLabel "name : bool" `shouldBe` Right (Label T.bool "name")

      it "parses '`field-name` : string'" $
        parseLabel "`field-name` : string" `shouldBe` Right (Label T.string "field-name")

      it "parses 'Constructor : int32'" $
        parseLabel "Constructor : int32" `shouldBe` Right (Label T.int32 "Constructor")

    describe "primitive literals" $ do
      it "parses '42' as int32 literal" $
        parseExpr "42" `shouldBe` Right (ELit (PInt32 42))

      it "parses 'true' as bool literal" $
        parseExpr "true" `shouldBe` Right (ELit (PBool True))

      it "parses 'false' as bool literal" $
        parseExpr "false" `shouldBe` Right (ELit (PBool False))

      it "parses '\"hello\"' as string literal" $
        parseExpr "\"hello\"" `shouldBe` Right (ELit (PString "hello"))

      it "parses '3.14' as double literal" $
        parseExpr "3.14" `shouldBe` Right (ELit (PDouble 3.14))

      it "parses '()' as unit literal" $
        parseExpr "()" `shouldBe` Right (ELit PUnit)

    describe "variables" $ do
      it "parses 'x : int32' as variable" $
        parseExpr "x : int32" `shouldBe` Right (EVar (Label T.int32 "x"))

      it "parses 'value : bool' as variable" $
        parseExpr "value : bool" `shouldBe` Right (EVar (Label T.bool "value"))

      it "parses '`field-name` : string' as variable" $
        parseExpr "`field-name` : string" `shouldBe` Right (EVar (Label T.string "field-name"))

    describe "constructors" $ do
      it "parses 'True : bool' as constructor" $
        parseExpr "True : bool" `shouldBe` Right (ECon (Label T.bool "True"))

      it "parses 'Nothing : Maybe (int32)' as constructor" $
        parseExpr "Nothing : Maybe (int32)"
          `shouldBe` Right (ECon (Label (TCon "Maybe" [T.int32]) "Nothing"))

    describe "qualified name disambiguation" $ do
      -- Bare names
      it "parses bare uppercase name as constructor" $
        parseExpr "Node : int32" `shouldBe` Right (ECon (Label T.int32 "Node"))

      it "parses bare lowercase name as variable" $
        parseExpr "node : int32" `shouldBe` Right (EVar (Label T.int32 "node"))

      -- Qualified names: last component determines constructor vs variable
      it "parses qualified name with uppercase last component as constructor" $
        parseExpr "Main.Node : int32" `shouldBe` Right (ECon (Label T.int32 "Main.Node"))

      it "parses qualified name with lowercase last component as variable" $
        parseExpr "Main.some_function : int32" `shouldBe` Right (EVar (Label T.int32 "Main.some_function"))

      it "parses qualified name with lowercase last component as variable (max)" $
        parseExpr "Main.max : int32 / int32 / int32"
          `shouldBe` Right (EVar (Label (T.arrow T.int32 (T.arrow T.int32 T.int32)) "Main.max"))

      it "parses multi-level qualified name with uppercase last as constructor" $
        parseExpr "My.Data.Node : int32" `shouldBe` Right (ECon (Label T.int32 "My.Data.Node"))

      it "parses multi-level qualified name with lowercase last as variable" $
        parseExpr "My.Utilities.find_min : int32" `shouldBe` Right (EVar (Label T.int32 "My.Utilities.find_min"))

      it "parses dollar-prefixed uppercase name as constructor" $
        parseExpr "$Cons : int32" `shouldBe` Right (ECon (Label T.int32 "$Cons"))

      it "parses qualified name with $instance segment as variable" $
        parseExpr "Ordered.compare__$instance.f377c7c1cf28bc72 : int32"
          `shouldBe` Right (EVar (Label T.int32 "Ordered.compare__$instance.f377c7c1cf28bc72"))

      it "parses sample_tree reference as variable" $
        parseExpr "Main.sample_tree : int32" `shouldBe` Right (EVar (Label T.int32 "Main.sample_tree"))

      -- Label parser tests for qualified names
      it "label: 'Main.Node : int32' has constructor name" $
        parseLabel "Main.Node : int32" `shouldBe` Right (Label T.int32 "Main.Node")

      it "label: 'Main.sort : int32' has function name" $
        parseLabel "Main.sort : int32" `shouldBe` Right (Label T.int32 "Main.sort")

      it "label: 'My.Utilities.find_min : int32' has function name" $
        parseLabel "My.Utilities.find_min : int32" `shouldBe` Right (Label T.int32 "My.Utilities.find_min")

    describe "let bindings" $ do
      it "parses 'let x : int32 = 42 in x : int32'" $
        parseExpr "let x : int32 = 42 in x : int32"
          `shouldBe` Right
            ( ELet
                (NE.fromList [Binding (Label T.int32 "x") (ELit (PInt32 42))])
                (EVar (Label T.int32 "x"))
            )

      it "parses 'let x : int32 = 1; y : int32 = 2 in x : int32'" $
        parseExpr "let x : int32 = 1; y : int32 = 2 in x : int32"
          `shouldBe` Right
            ( ELet
                ( NE.fromList
                    [ Binding (Label T.int32 "x") (ELit (PInt32 1))
                    , Binding (Label T.int32 "y") (ELit (PInt32 2))
                    ]
                )
                (EVar (Label T.int32 "x"))
            )

      it "parses let with three bindings" $
        parseExpr "let a : int32 = 1; b : int32 = 2; c : int32 = 3 in a : int32"
          `shouldBe` Right
            ( ELet
                ( NE.fromList
                    [ Binding (Label T.int32 "a") (ELit (PInt32 1))
                    , Binding (Label T.int32 "b") (ELit (PInt32 2))
                    , Binding (Label T.int32 "c") (ELit (PInt32 3))
                    ]
                )
                (EVar (Label T.int32 "a"))
            )

    describe "if expressions" $ do
      it "parses 'if (true) then 1 else 2'" $
        parseExpr "if (true) then 1 else 2"
          `shouldBe` Right
            ( EIf
                (ELit (PBool True))
                (ELit (PInt32 1))
                (ELit (PInt32 2))
            )

      it "parses 'if (x : bool) then y : int32 else z : int32'" $
        parseExpr "if (x : bool) then y : int32 else z : int32"
          `shouldBe` Right
            ( EIf
                (EVar (Label T.bool "x"))
                (EVar (Label T.int32 "y"))
                (EVar (Label T.int32 "z"))
            )

      it "parses nested if" $
        parseExpr "if (a : bool) then if (b : bool) then 1 else 2 else 3"
          `shouldBe` Right
            ( EIf
                (EVar (Label T.bool "a"))
                ( EIf
                    (EVar (Label T.bool "b"))
                    (ELit (PInt32 1))
                    (ELit (PInt32 2))
                )
                (ELit (PInt32 3))
            )

    describe "lambda expressions" $ do
      it "parses 'fn (x : int32) => x : int32'" $
        parseExpr "fn (x : int32) => x : int32"
          `shouldBe` Right
            ( ELam
                (NE.fromList [Label T.int32 "x"])
                (EVar (Label T.int32 "x"))
            )

      it "parses 'fn (x : int32, y : int32) => x : int32'" $
        parseExpr "fn (x : int32, y : int32) => x : int32"
          `shouldBe` Right
            ( ELam
                (NE.fromList [Label T.int32 "x", Label T.int32 "y"])
                (EVar (Label T.int32 "x"))
            )

      it "parses lambda with three parameters" $
        parseExpr "fn (a : int32, b : bool, c : string) => a : int32"
          `shouldBe` Right
            ( ELam
                (NE.fromList [Label T.int32 "a", Label T.bool "b", Label T.string "c"])
                (EVar (Label T.int32 "a"))
            )

    describe "function application" $ do
      it "parses '@ <int32> (f : int32 / int32, 42)'" $
        parseExpr "@ <int32> (f : int32 / int32, 42)"
          `shouldBe` Right
            ( EApp
                T.int32
                (EVar (Label (T.arrow T.int32 T.int32) "f"))
                (NE.fromList [ELit (PInt32 42)])
            )

      it "parses application with three arguments" $
        parseExpr "@ <int32> (f : int32, 1, 2)"
          `shouldBe` Right
            ( EApp
                T.int32
                (EVar (Label T.int32 "f"))
                (NE.fromList [ELit (PInt32 1), ELit (PInt32 2)])
            )

    describe "case expressions" $ do
      it "parses simple case with one clause" $
        parseExpr "case <bool> (x : int32) { | (y : int32) => true }"
          `shouldBe` Right
            ( ECase
                T.bool
                (EVar (Label T.int32 "x"))
                ( NE.fromList
                    [Clause (NE.fromList [Label T.int32 "y"]) (ELit (PBool True))]
                )
            )

      it "parses case with two clauses" $
        parseExpr "case <int32> (x : bool) { | (y : bool) => 1 | (z : bool) => 2 }"
          `shouldBe` Right
            ( ECase
                T.int32
                (EVar (Label T.bool "x"))
                ( NE.fromList
                    [ Clause (NE.fromList [Label T.bool "y"]) (ELit (PInt32 1))
                    , Clause (NE.fromList [Label T.bool "z"]) (ELit (PInt32 2))
                    ]
                )
            )

      it "parses case with multi-pattern clause" $
        parseExpr "case <int32> (x : bool) { | (a : bool, b : int32) => 42 }"
          `shouldBe` Right
            ( ECase
                T.int32
                (EVar (Label T.bool "x"))
                ( NE.fromList
                    [ Clause
                        (NE.fromList [Label T.bool "a", Label T.int32 "b"])
                        (ELit (PInt32 42))
                    ]
                )
            )

    describe "empty record" $ do
      it "parses '{}'" $
        parseExpr "{}" `shouldBe` Right ENil

    describe "record extension" $ do
      it "parses '{ x = 42 | {} }'" $
        parseExpr "{ x = 42 | {} }"
          `shouldBe` Right (EExt "x" (ELit (PInt32 42)) ENil)

      it "parses record with backtick field" $
        parseExpr "{ `field-name` = 42 | {} }"
          `shouldBe` Right (EExt "field-name" (ELit (PInt32 42)) ENil)

      it "parses record ending with variable" $
        parseExpr "{ x = 1 | rest : record ({}) }"
          `shouldBe` Right
            ( EExt
                "x"
                (ELit (PInt32 1))
                (EVar (Label (TCon "record" [RNil]) "rest"))
            )

    describe "projection expressions" $ do
      it "parses 'get?_foo<int32>(r : record ({}))'" $
        parseExpr "get?_foo<int32>(r : record ({}))"
          `shouldBe` Right
            ( EGet
                (Label T.int32 "foo")
                (EVar (Label (TCon "record" [RNil]) "r"))
            )
      it "parses 'get?_from_int32<int32/*>(r : record ({}))'" $
        parseExpr "get?_from_int32<int32/*>(r : record ({}))"
          `shouldBe` Right
            ( EGet
                (Label (T.arrow T.int32 T.opaque) "from_int32")
                (EVar (Label (TCon "record" [RNil]) "r"))
            )

    describe "operator expressions" $ do
      it "parses '[+ int32] (1, 2)'" $
        parseExpr "[+ int32] (1, 2)"
          `shouldBe` Right (EOp (OAddInt32 (ELit (PInt32 1)) (ELit (PInt32 2))))

      it "parses '[== bool] (true, false)'" $
        parseExpr "[== bool] (true, false)"
          `shouldBe` Right (EOp (OEqBool (ELit (PBool True)) (ELit (PBool False))))

      it "parses '[!] (x : bool)'" $
        parseExpr "[!] (x : bool)"
          `shouldBe` Right (EOp (ONot (EVar (Label T.bool "x"))))

    describe "parenthesized expressions" $ do
      it "parses '(42)'" $
        parseExpr "(42)" `shouldBe` Right (ELit (PInt32 42))

      it "parses '((x : int32))'" $
        parseExpr "((x : int32))" `shouldBe` Right (EVar (Label T.int32 "x"))

    describe "whitespace handling" $ do
      it "handles leading whitespace" $
        parseExpr "  42" `shouldBe` Right (ELit (PInt32 42))

      it "handles trailing whitespace" $
        parseExpr "42  " `shouldBe` Right (ELit (PInt32 42))

      it "handles whitespace in let" $
        parseExpr "let  x : int32 = 42  in  x : int32"
          `shouldBe` Right
            ( ELet
                (NE.fromList [Binding (Label T.int32 "x") (ELit (PInt32 42))])
                (EVar (Label T.int32 "x"))
            )

      it "handles newlines in lambda" $
        parseExpr "fn\n(x : int32)\n=>\nx : int32"
          `shouldBe` Right
            ( ELam
                (NE.fromList [Label T.int32 "x"])
                (EVar (Label T.int32 "x"))
            )

    describe "complex expressions" $ do
      it "parses lambda returning if" $
        parseExpr "fn (x : bool) => if (x : bool) then 1 else 2"
          `shouldBe` Right
            ( ELam
                (NE.fromList [Label T.bool "x"])
                ( EIf
                    (EVar (Label T.bool "x"))
                    (ELit (PInt32 1))
                    (ELit (PInt32 2))
                )
            )

      it "parses let with lambda body" $
        parseExpr "let f : int32 = 42 in fn (x : int32) => x : int32"
          `shouldBe` Right
            ( ELet
                (NE.fromList [Binding (Label T.int32 "f") (ELit (PInt32 42))])
                ( ELam
                    (NE.fromList [Label T.int32 "x"])
                    (EVar (Label T.int32 "x"))
                )
            )

      it "parses application of lambda" $
        parseExpr "@ <int32> (fn (x : int32) => x : int32, 42)"
          `shouldBe` Right
            ( EApp
                T.int32
                ( ELam
                    (NE.fromList [Label T.int32 "x"])
                    (EVar (Label T.int32 "x"))
                )
                (NE.fromList [ELit (PInt32 42)])
            )

    describe "error cases" $ do
      it "fails on empty input" $
        parseExpr "" `shouldSatisfy` isLeft

      it "fails on invalid keyword" $
        parseExpr "invalid" `shouldSatisfy` isLeft

      it "fails on unclosed parenthesis" $
        parseExpr "(42" `shouldSatisfy` isLeft

      it "fails on lambda without arrow" $
        parseExpr "fn (x : int32) x : int32" `shouldSatisfy` isLeft

      it "fails on let without in" $
        parseExpr "let x : int32 = 42" `shouldSatisfy` isLeft

      it "fails on application with one argument" $
        parseExpr "@ <int32> (f : int32)" `shouldSatisfy` isLeft

      it "fails on case without clauses" $
        parseExpr "case <int32> (x : bool) {}" `shouldSatisfy` isLeft

      it "fails on '{ x = 1 | { y = 2 | {} } }'" $
        parseExpr "{ x = 1 | { y = 2 | {} } }" `shouldSatisfy` isLeft
