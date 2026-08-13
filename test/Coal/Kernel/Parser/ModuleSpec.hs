{-# LANGUAGE OverloadedStrings #-}

module Coal.Kernel.Parser.ModuleSpec (spec) where

import Coal.Kernel.Language.Expr (Expr (..), Label (..))
import Coal.Kernel.Language.Module (Module (..))
import Coal.Kernel.Language.Object (FunctionScope (..), Object (..))
import Coal.Kernel.Language.Op (Op (..))
import Coal.Kernel.Language.Prim (Prim (..))
import Coal.Kernel.Language.Type (Type (..))
import qualified Coal.Kernel.Language.Type.Constructors as T
import Coal.Kernel.Parser.Module (module_)
import qualified Data.Text as Text
import Data.Void (Void)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec (ParseErrorBundle, parse)

-- | Helper to parse a module
parseModule :: Text.Text -> Either (ParseErrorBundle Text.Text Void) (Module Type)
parseModule = parse module_ ""

-- | Helper to check if result is Right
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

-- | Helper to check if result is Left
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

-- | Test specification for the Module parser
spec :: Spec
spec = do
  describe "module_ parser" $ do
    describe "minimal module" $ do
      it "parses a module with one constant" $
        parseModule "module Test { Answer = 42 }"
          `shouldBe` Right
            ( Module
                "Test"
                []
                [DConstant "Answer" (ELit (PInt32 42))]
            )

      it "parses a module with one data declaration" $
        parseModule "module Test { data Point = Point(int32, int32) }"
          `shouldBe` Right
            ( Module
                "Test"
                []
                [DData "Point" [("Point", TCon "/" [T.int32, TCon "/" [T.int32, TCon "Point" []]])]]
            )

      it "parses a module with one function" $
        parseModule "module Test { Identity (x : int32) = x : int32 }"
          `shouldBe` Right
            ( Module
                "Test"
                []
                [DFunction Exported "Identity" [Label T.int32 "x"] (EVar (Label T.int32 "x"))]
            )

    describe "module with imports" $ do
      it "parses a module with one import" $
        parseModule "module Test { import Prelude Answer = 42 }"
          `shouldBe` Right
            ( Module
                "Test"
                ["Prelude"]
                [DConstant "Answer" (ELit (PInt32 42))]
            )

      it "parses a module with multiple imports" $
        parseModule "module Test { import Prelude import List import Maybe Answer = 42 }"
          `shouldBe` Right
            ( Module
                "Test"
                ["Prelude", "List", "Maybe"]
                [DConstant "Answer" (ELit (PInt32 42))]
            )

      it "parses a module with imports before objects" $
        parseModule "module Test { import Prelude import Data data Bool = Bool True = true }"
          `shouldBe` Right
            ( Module
                "Test"
                ["Prelude", "Data"]
                [ DData "Bool" [("Bool", TCon "Bool" [])]
                , DConstant "True" (ELit (PBool True))
                ]
            )

    describe "module with multiple objects" $ do
      it "parses a module with multiple constants" $
        parseModule "module Test { X = 1 Y = 2 Z = 3 }"
          `shouldBe` Right
            ( Module
                "Test"
                []
                [ DConstant "X" (ELit (PInt32 1))
                , DConstant "Y" (ELit (PInt32 2))
                , DConstant "Z" (ELit (PInt32 3))
                ]
            )

      it "parses a module with multiple data declarations" $
        parseModule "module Test { data Point = Point(float, float) data Color = Color(int32, int32, int32) }"
          `shouldBe` Right
            ( Module
                "Test"
                []
                [ DData "Point" [("Point", TCon "/" [T.float, TCon "/" [T.float, TCon "Point" []]])]
                , DData "Color" [("Color", TCon "/" [T.int32, TCon "/" [T.int32, TCon "/" [T.int32, TCon "Color" []]]])]
                ]
            )

      it "parses a module with mixed object types" $
        parseModule
          "module Test { \
          \data Point = Point(float, float) \
          \Pi = 3.14 \
          \Add (x : int32, y : int32) = [+ int32] (x : int32, y : int32) \
          \}"
          `shouldBe` Right
            ( Module
                "Test"
                []
                [ DData "Point" [("Point", TCon "/" [T.float, TCon "/" [T.float, TCon "Point" []]])]
                , DConstant "Pi" (ELit (PDouble 3.14))
                , DFunction
                    Exported
                    "Add"
                    [Label T.int32 "x", Label T.int32 "y"]
                    (EOp (OAddInt32 (EVar (Label T.int32 "x")) (EVar (Label T.int32 "y"))))
                ]
            )

    describe "data declarations" $ do
      it "parses data with arity 0" $
        parseModule "module Test { data Unit = Unit }"
          `shouldBe` Right
            ( Module
                "Test"
                []
                [DData "Unit" [("Unit", TCon "Unit" [])]]
            )

      it "parses data with large arity" $
        parseModule "module Test { data Tuple8 = Tuple8(*, *, *, *, *, *, *, *) }"
          `shouldBe` Right
            ( Module
                "Test"
                []
                [DData "Tuple8" [("Tuple8", TCon "/" [T.opaque, TCon "/" [T.opaque, TCon "/" [T.opaque, TCon "/" [T.opaque, TCon "/" [T.opaque, TCon "/" [T.opaque, TCon "/" [T.opaque, TCon "/" [T.opaque, TCon "Tuple8" []]]]]]]]])]]
            )

      it "parses data with complex type" $
        parseModule "module Test { data Maybe = Just(int32) }"
          `shouldBe` Right
            ( Module
                "Test"
                []
                [DData "Maybe" [("Just", TCon "/" [T.int32, TCon "Maybe" []])]]
            )

    describe "function declarations" $ do
      it "parses function with one parameter" $
        parseModule "module Test { Square (x : int32) = [* int32] (x : int32, x : int32) }"
          `shouldBe` Right
            ( Module
                "Test"
                []
                [ DFunction
                    Exported
                    "Square"
                    [Label T.int32 "x"]
                    (EOp (OMulInt32 (EVar (Label T.int32 "x")) (EVar (Label T.int32 "x"))))
                ]
            )

      it "parses function with multiple parameters" $
        parseModule "module Test { Add (x : int32, y : int32, z : int32) = x : int32 }"
          `shouldBe` Right
            ( Module
                "Test"
                []
                [ DFunction
                    Exported
                    "Add"
                    [Label T.int32 "x", Label T.int32 "y", Label T.int32 "z"]
                    (EVar (Label T.int32 "x"))
                ]
            )

      it "parses function with backtick-quoted parameter names" $
        parseModule "module Test { Get (`field-name` : string) = `field-name` : string }"
          `shouldBe` Right
            ( Module
                "Test"
                []
                [ DFunction
                    Exported
                    "Get"
                    [Label T.string "field-name"]
                    (EVar (Label T.string "field-name"))
                ]
            )

    describe "constant declarations" $ do
      it "parses constant with int32" $
        parseModule "module Test { Answer = 42 }"
          `shouldBe` Right
            ( Module
                "Test"
                []
                [DConstant "Answer" (ELit (PInt32 42))]
            )

      it "parses constant with bool" $
        parseModule "module Test { Flag = true }"
          `shouldBe` Right
            ( Module
                "Test"
                []
                [DConstant "Flag" (ELit (PBool True))]
            )

      it "parses constant with variable expression" $
        parseModule "module Test { Value = x : int32 }"
          `shouldBe` Right
            ( Module
                "Test"
                []
                [DConstant "Value" (EVar (Label T.int32 "x"))]
            )

    describe "whitespace handling" $ do
      it "parses module with minimal whitespace" $
        parseModule "module Test{X=1}"
          `shouldBe` Right
            ( Module
                "Test"
                []
                [DConstant "X" (ELit (PInt32 1))]
            )

      it "parses module with extra whitespace" $
        parseModule "module   Test   {   X   =   1   }"
          `shouldBe` Right
            ( Module
                "Test"
                []
                [DConstant "X" (ELit (PInt32 1))]
            )

      it "parses module with newlines" $
        parseModule
          "module Test {\n\
          \  import Prelude\n\
          \  X = 1\n\
          \  Y = 2\n\
          \}"
          `shouldBe` Right
            ( Module
                "Test"
                ["Prelude"]
                [ DConstant "X" (ELit (PInt32 1))
                , DConstant "Y" (ELit (PInt32 2))
                ]
            )

    describe "error cases" $ do
      it "fails on module with no objects" $
        parseModule "module Test { }" `shouldSatisfy` isLeft

      it "fails on module with only imports" $
        parseModule "module Test { import Prelude }" `shouldSatisfy` isLeft

      it "fails on lowercase module name" $
        parseModule "module test { X = 1 }" `shouldSatisfy` isLeft

      it "fails on missing braces" $
        parseModule "module Test X = 1" `shouldSatisfy` isLeft

      it "fails on lowercase object name" $
        parseModule "module Test { x = 1 }" `shouldSatisfy` isLeft

      it "fails on data with missing arity" $
        parseModule "module Test { data Point <int32> }" `shouldSatisfy` isLeft

      it "fails on function with no parameters" $
        parseModule "module Test { F () = 1 }" `shouldSatisfy` isLeft

    describe "full module examples from files" $ do
      it "parses simple module with tuple2 type constructor" $
        parseModule "module Test { X = $Tuple2 : tuple2(int32,int32) }"
          `shouldSatisfy` isRight

    -- NOTE: File-based tests temporarily disabled. Some .corn example files
    -- under test/examples have not yet been migrated to the current grouped
    -- data syntax. Re-enable once the migration is complete.
    -- let examplesDir = "test/examples"
    -- cornFiles <- runIO $ findCornFiles examplesDir
    -- forM_ cornFiles $ \filePath -> do
    --   content <- runIO $ TextIO.readFile filePath
    --   it ("parses " ++ filePath) $
    --     shouldParseSuccessfully (parseModule content)

    describe "qualified/dotted identifiers" $ do
      it "parses module with dotted import" $
        parseModule "module Test { import My.Module X = 1 }"
          `shouldBe` Right
            ( Module
                "Test"
                ["My.Module"]
                [DConstant "X" (ELit (PInt32 1))]
            )

      it "parses module with multi-level dotted import" $
        parseModule "module Test { import My.Utilities.List X = 1 }"
          `shouldBe` Right
            ( Module
                "Test"
                ["My.Utilities.List"]
                [DConstant "X" (ELit (PInt32 1))]
            )

      it "parses constant with qualified name" $
        parseModule "module Test { Main.value = 42 }"
          `shouldBe` Right
            ( Module
                "Test"
                []
                [DConstant "Main.value" (ELit (PInt32 42))]
            )

      it "parses function with qualified name and qualified parameter" $
        parseModule "module Test { My.Module.fun (x.y : int32) = x.y : int32 }"
          `shouldBe` Right
            ( Module
                "Test"
                []
                [DFunction Exported "My.Module.fun" [Label T.int32 "x.y"] (EVar (Label T.int32 "x.y"))]
            )

      it "parses constant with $-prefixed constructor" $
        parseModule "module Test { Nil = $Nil : list(int32) }"
          `shouldBe` Right
            ( Module
                "Test"
                []
                [DConstant "Nil" (ECon (Label (TCon "list" [T.int32]) "$Nil"))]
            )

    describe "qualified names with lowercase last component" $ do
      it "parses function with qualified lowercase-last name" $
        parseModule "module Test { Main.max (a : int32, b : int32) = a : int32 }"
          `shouldBe` Right
            ( Module
                "Test"
                []
                [DFunction Exported "Main.max" [Label T.int32 "a", Label T.int32 "b"] (EVar (Label T.int32 "a"))]
            )

      it "parses constant with qualified lowercase-last name" $
        parseModule "module Test { Main.sample_tree = 42 }"
          `shouldBe` Right
            ( Module
                "Test"
                []
                [DConstant "Main.sample_tree" (ELit (PInt32 42))]
            )

      it "parses multi-level qualified lowercase-last function" $
        parseModule "module Test { My.Utilities.find_min (x : int32) = x : int32 }"
          `shouldBe` Right
            ( Module
                "Test"
                []
                [DFunction Exported "My.Utilities.find_min" [Label T.int32 "x"] (EVar (Label T.int32 "x"))]
            )

      it "parses data with qualified uppercase-last name" $
        parseModule "module Test { data Main.Node = Main.Node(*, *, *) }"
          `shouldBe` Right
            ( Module
                "Test"
                []
                [DData "Main.Node" [("Main.Node", TCon "/" [T.opaque, TCon "/" [T.opaque, TCon "/" [T.opaque, TCon "Main.Node" []]]])]]
            )

    describe "import names with lowercase last component" $ do
      it "parses import with qualified lowercase-last name" $
        parseModule "module Test { import My.Utilities.find_min X = 1 }"
          `shouldBe` Right
            ( Module
                "Test"
                ["My.Utilities.find_min"]
                [DConstant "X" (ELit (PInt32 1))]
            )

      it "parses import with complex $instance-style name" $
        parseModule "module Test { import Ordered.compare__$instance.f377c7c1cf28bc72 X = 1 }"
          `shouldBe` Right
            ( Module
                "Test"
                ["Ordered.compare__$instance.f377c7c1cf28bc72"]
                [DConstant "X" (ELit (PInt32 1))]
            )

      it "parses multiple imports with mixed last-component cases" $
        parseModule "module Test { import Ordering.LessThan import Tree.flatten import Tree.from_list X = 1 }"
          `shouldBe` Right
            ( Module
                "Test"
                ["Ordering.LessThan", "Tree.flatten", "Tree.from_list"]
                [DConstant "X" (ELit (PInt32 1))]
            )
