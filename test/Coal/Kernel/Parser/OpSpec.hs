{-# LANGUAGE OverloadedStrings #-}

module Coal.Kernel.Parser.OpSpec (spec) where

import Coal.Kernel.Language.Op (Op (..))
import Coal.Kernel.Parser (Parser, lexeme)
import Coal.Kernel.Parser.Op (op)
import qualified Data.Text as T
import Data.Void (Void)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec (ParseErrorBundle, parse, (<|>))
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as C

-- | Dummy expression type for testing
type Expr = String

-- | Simple parser for test expressions (identifiers)
parseExpr :: Parser Expr
parseExpr = lexeme $ do
  first <- C.letterChar <|> C.char '_'
  rest <- P.many (C.alphaNumChar <|> C.char '_')
  return (first : rest)

-- | Helper to parse an operator with string expressions
parseOp :: T.Text -> Either (ParseErrorBundle T.Text Void) (Op Expr)
parseOp = parse (op parseExpr) ""

-- | Test specification for the Op parser
spec :: Spec
spec = do
  describe "op parser" $ do
    describe "equality operators" $ do
      it "parses [== int32] (x, y)" $
        parseOp "[== int32] (x, y)" `shouldBe` Right (OEqInt32 "x" "y")

      it "parses [== int64] (a, b)" $
        parseOp "[== int64] (a, b)" `shouldBe` Right (OEqInt64 "a" "b")

      it "parses [== float] (p, q)" $
        parseOp "[== float] (p, q)" `shouldBe` Right (OEqFloat "p" "q")

      it "parses [== double] (m, n)" $
        parseOp "[== double] (m, n)" `shouldBe` Right (OEqDouble "m" "n")

      it "parses [== bool] (t, f)" $
        parseOp "[== bool] (t, f)" `shouldBe` Right (OEqBool "t" "f")

      it "parses [== char] (c1, c2)" $
        parseOp "[== char] (c1, c2)" `shouldBe` Right (OEqChar "c1" "c2")

    describe "inequality operators" $ do
      it "parses [!= int32] (x, y)" $
        parseOp "[!= int32] (x, y)" `shouldBe` Right (ONeInt32 "x" "y")

      it "parses [!= int64] (a, b)" $
        parseOp "[!= int64] (a, b)" `shouldBe` Right (ONeInt64 "a" "b")

      it "parses [!= float] (p, q)" $
        parseOp "[!= float] (p, q)" `shouldBe` Right (ONeFloat "p" "q")

      it "parses [!= double] (m, n)" $
        parseOp "[!= double] (m, n)" `shouldBe` Right (ONeDouble "m" "n")

      it "parses [!= bool] (t, f)" $
        parseOp "[!= bool] (t, f)" `shouldBe` Right (ONeBool "t" "f")

      it "parses [!= char] (c1, c2)" $
        parseOp "[!= char] (c1, c2)" `shouldBe` Right (ONeChar "c1" "c2")

    describe "less than operators" $ do
      it "parses [< int32] (x, y)" $
        parseOp "[< int32] (x, y)" `shouldBe` Right (OLtInt32 "x" "y")

      it "parses [< int64] (a, b)" $
        parseOp "[< int64] (a, b)" `shouldBe` Right (OLtInt64 "a" "b")

      it "parses [< float] (p, q)" $
        parseOp "[< float] (p, q)" `shouldBe` Right (OLtFloat "p" "q")

      it "parses [< double] (m, n)" $
        parseOp "[< double] (m, n)" `shouldBe` Right (OLtDouble "m" "n")

    describe "greater than operators" $ do
      it "parses [> int32] (x, y)" $
        parseOp "[> int32] (x, y)" `shouldBe` Right (OGtInt32 "x" "y")

      it "parses [> int64] (a, b)" $
        parseOp "[> int64] (a, b)" `shouldBe` Right (OGtInt64 "a" "b")

      it "parses [> float] (p, q)" $
        parseOp "[> float] (p, q)" `shouldBe` Right (OGtFloat "p" "q")

      it "parses [> double] (m, n)" $
        parseOp "[> double] (m, n)" `shouldBe` Right (OGtDouble "m" "n")

    describe "less than or equal operators" $ do
      it "parses [<= int32] (x, y)" $
        parseOp "[<= int32] (x, y)" `shouldBe` Right (OLteInt32 "x" "y")

      it "parses [<= int64] (a, b)" $
        parseOp "[<= int64] (a, b)" `shouldBe` Right (OLteInt64 "a" "b")

      it "parses [<= float] (p, q)" $
        parseOp "[<= float] (p, q)" `shouldBe` Right (OLteFloat "p" "q")

      it "parses [<= double] (m, n)" $
        parseOp "[<= double] (m, n)" `shouldBe` Right (OLteDouble "m" "n")

    describe "greater than or equal operators" $ do
      it "parses [>= int32] (x, y)" $
        parseOp "[>= int32] (x, y)" `shouldBe` Right (OGteInt32 "x" "y")

      it "parses [>= int64] (a, b)" $
        parseOp "[>= int64] (a, b)" `shouldBe` Right (OGteInt64 "a" "b")

      it "parses [>= float] (p, q)" $
        parseOp "[>= float] (p, q)" `shouldBe` Right (OGteFloat "p" "q")

      it "parses [>= double] (m, n)" $
        parseOp "[>= double] (m, n)" `shouldBe` Right (OGteDouble "m" "n")

    describe "addition operators" $ do
      it "parses [+ int32] (x, y)" $
        parseOp "[+ int32] (x, y)" `shouldBe` Right (OAddInt32 "x" "y")

      it "parses [+ int64] (a, b)" $
        parseOp "[+ int64] (a, b)" `shouldBe` Right (OAddInt64 "a" "b")

      it "parses [+ float] (p, q)" $
        parseOp "[+ float] (p, q)" `shouldBe` Right (OAddFloat "p" "q")

      it "parses [+ double] (m, n)" $
        parseOp "[+ double] (m, n)" `shouldBe` Right (OAddDouble "m" "n")

    describe "subtraction operators" $ do
      it "parses [- int32] (x, y)" $
        parseOp "[- int32] (x, y)" `shouldBe` Right (OSubInt32 "x" "y")

      it "parses [- int64] (a, b)" $
        parseOp "[- int64] (a, b)" `shouldBe` Right (OSubInt64 "a" "b")

      it "parses [- float] (p, q)" $
        parseOp "[- float] (p, q)" `shouldBe` Right (OSubFloat "p" "q")

      it "parses [- double] (m, n)" $
        parseOp "[- double] (m, n)" `shouldBe` Right (OSubDouble "m" "n")

    describe "multiplication operators" $ do
      it "parses [* int32] (x, y)" $
        parseOp "[* int32] (x, y)" `shouldBe` Right (OMulInt32 "x" "y")

      it "parses [* int64] (a, b)" $
        parseOp "[* int64] (a, b)" `shouldBe` Right (OMulInt64 "a" "b")

      it "parses [* float] (p, q)" $
        parseOp "[* float] (p, q)" `shouldBe` Right (OMulFloat "p" "q")

      it "parses [* double] (m, n)" $
        parseOp "[* double] (m, n)" `shouldBe` Right (OMulDouble "m" "n")

    describe "division operators" $ do
      it "parses [/ int32] (x, y)" $
        parseOp "[/ int32] (x, y)" `shouldBe` Right (ODivInt32 "x" "y")

      it "parses [/ int64] (a, b)" $
        parseOp "[/ int64] (a, b)" `shouldBe` Right (ODivInt64 "a" "b")

      it "parses [/ float] (p, q)" $
        parseOp "[/ float] (p, q)" `shouldBe` Right (ODivFloat "p" "q")

      it "parses [/ double] (m, n)" $
        parseOp "[/ double] (m, n)" `shouldBe` Right (ODivDouble "m" "n")

    describe "logical operators" $ do
      it "parses [||] (x, y) without type annotation" $
        parseOp "[||] (x, y)" `shouldBe` Right (OOr "x" "y")

      it "parses [&&] (a, b) without type annotation" $
        parseOp "[&&] (a, b)" `shouldBe` Right (OAnd "a" "b")

    describe "unary NOT operator" $ do
      it "parses [!] (x)" $
        parseOp "[!] (x)" `shouldBe` Right (ONot "x")

      it "parses [!] (cond) without type annotation" $
        parseOp "[!] (cond)" `shouldBe` Right (ONot "cond")

    describe "unary negation operators" $ do
      it "parses [neg int32] (x)" $
        parseOp "[neg int32] (x)" `shouldBe` Right (ONegInt32 "x")

      it "parses [neg int64] (a)" $
        parseOp "[neg int64] (a)" `shouldBe` Right (ONegInt64 "a")

      it "parses [neg float] (f)" $
        parseOp "[neg float] (f)" `shouldBe` Right (ONegFloat "f")

      it "parses [neg double] (d)" $
        parseOp "[neg double] (d)" `shouldBe` Right (ONegDouble "d")

    describe "whitespace handling" $ do
      it "handles extra spaces in [+ int32] ( x , y )" $
        parseOp "[+ int32] ( x , y )" `shouldBe` Right (OAddInt32 "x" "y")

      it "handles no spaces in [+int32](x,y)" $
        parseOp "[+int32](x,y)" `shouldBe` Right (OAddInt32 "x" "y")

      it "handles newlines in [\n+\nint32\n]\n(\nx\n,\ny\n)" $
        parseOp "[\n+\nint32\n]\n(\nx\n,\ny\n)" `shouldBe` Right (OAddInt32 "x" "y")

    describe "error cases" $ do
      it "fails on [< bool] (x, y) - invalid operator for bool" $
        parseOp "[< bool] (x, y)" `shouldSatisfy` isLeft

      it "fails on [+] (x, y) - missing required type annotation" $
        parseOp "[+] (x, y)" `shouldSatisfy` isLeft

      it "fails on [== unknown] (x, y) - unknown type" $
        parseOp "[== unknown] (x, y)" `shouldSatisfy` isLeft

      it "fails on [neg bool] (x) - invalid type for negation" $
        parseOp "[neg bool] (x)" `shouldSatisfy` isLeft

      it "fails on [!= int32] (x) - wrong number of arguments" $
        parseOp "[!= int32] (x)" `shouldSatisfy` isLeft

      it "fails on [!] (x, y) - wrong number of arguments" $
        parseOp "[!] (x, y)" `shouldSatisfy` isLeft

      it "fails on unclosed bracket [+ int32 (x, y)" $
        parseOp "[+ int32 (x, y)" `shouldSatisfy` isLeft

      it "fails on unclosed paren [+ int32] (x, y" $
        parseOp "[+ int32] (x, y" `shouldSatisfy` isLeft

      it "fails on missing comma [+ int32] (x y)" $
        parseOp "[+ int32] (x y)" `shouldSatisfy` isLeft

-- Helper function to check if result is Left
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False
