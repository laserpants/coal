{-# LANGUAGE OverloadedStrings #-}

module Coal.Kernel.Parser.TypeSpec (spec) where

import Coal.Kernel.Language.Type (Type (..))
import qualified Coal.Kernel.Language.Type.Constructors as T
import Coal.Kernel.Parser.Type (type_)
import qualified Data.Text as Text
import Data.Void (Void)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec (ParseErrorBundle, parse)

-- | Helper to parse a type
parseType :: Text.Text -> Either (ParseErrorBundle Text.Text Void) Type
parseType = parse type_ ""

-- | Test specification for the Type parser
spec :: Spec
spec = do
  describe "type_ parser" $ do
    describe "primitive types" $ do
      it "parses 'bool'" $
        parseType "bool" `shouldBe` Right T.bool

      it "parses 'char'" $
        parseType "char" `shouldBe` Right T.char

      it "parses 'int32'" $
        parseType "int32" `shouldBe` Right T.int32

      it "parses 'int64'" $
        parseType "int64" `shouldBe` Right T.int64

      it "parses 'float'" $
        parseType "float" `shouldBe` Right T.float

      it "parses 'double'" $
        parseType "double" `shouldBe` Right T.double

      it "parses 'bignum'" $
        parseType "bignum" `shouldBe` Right T.bignum

      it "parses 'string'" $
        parseType "string" `shouldBe` Right T.string

      it "parses 'unit'" $
        parseType "unit" `shouldBe` Right T.unit

    describe "opaque type" $ do
      it "parses '*' as opaque type" $
        parseType "*" `shouldBe` Right T.opaque

    describe "list types" $ do
      it "parses 'list (int32)'" $
        parseType "list (int32)" `shouldBe` Right (TCon "list" [T.int32])

      it "parses 'list (bool)'" $
        parseType "list (bool)" `shouldBe` Right (TCon "list" [T.bool])

      it "parses nested 'list (list (string))'" $
        parseType "list (list (string))" `shouldBe` Right (TCon "list" [TCon "list" [T.string]])

      it "parses 'list (*)'" $
        parseType "list (*)" `shouldBe` Right (TCon "list" [T.opaque])

    describe "tuple types" $ do
      it "parses 'tuple2(int32, bool)'" $
        parseType "tuple2(int32, bool)" `shouldBe` Right (TCon "tuple2" [T.int32, T.bool])

      it "parses 'tuple3(string, float, char)'" $
        parseType "tuple3(string, float, char)"
          `shouldBe` Right (TCon "tuple3" [T.string, T.float, T.char])

      it "parses 'tuple4(int32, int64, float, double)'" $
        parseType "tuple4(int32, int64, float, double)"
          `shouldBe` Right (TCon "tuple4" [T.int32, T.int64, T.float, T.double])

      it "fails on 'tuple1(int32)' - size too small" $
        parseType "tuple1(int32)" `shouldSatisfy` isLeft

      it "fails on 'tuple0()' - size too small" $
        parseType "tuple0()" `shouldSatisfy` isLeft

      it "fails on mismatched count 'tuple2(int32, bool, string)'" $
        parseType "tuple2(int32, bool, string)" `shouldSatisfy` isLeft

      it "fails on mismatched count 'tuple3(int32, bool)'" $
        parseType "tuple3(int32, bool)" `shouldSatisfy` isLeft

    describe "type constructors" $ do
      it "parses 'Maybe'" $
        parseType "Maybe" `shouldBe` Right (TCon "Maybe" [])

      it "parses 'Either'" $
        parseType "Either" `shouldBe` Right (TCon "Either" [])

      it "parses 'Maybe (int32)'" $
        parseType "Maybe (int32)" `shouldBe` Right (TCon "Maybe" [T.int32])

      it "parses 'Either (string, int32)'" $
        parseType "Either (string, int32)" `shouldBe` Right (TCon "Either" [T.string, T.int32])

      it "parses 'Result (bool, string, float)'" $
        parseType "Result (bool, string, float)"
          `shouldBe` Right (TCon "Result" [T.bool, T.string, T.float])

      it "parses nested 'Maybe (Maybe (int32))'" $
        parseType "Maybe (Maybe (int32))"
          `shouldBe` Right (TCon "Maybe" [TCon "Maybe" [T.int32]])

    describe "row types" $ do
      it "parses empty row '{}'" $
        parseType "{}" `shouldBe` Right RNil

      it "parses opaque row '*' in row context" $
        parseType "*" `shouldBe` Right T.opaque

      it "parses single field row '{x: int32 | {}}'" $
        parseType "{x: int32 | {}}" `shouldBe` Right (RExt "x" T.int32 RNil)

      it "parses two field row '{x: int32 | y: bool | {}}'" $
        parseType "{x: int32 | y: bool | {}}"
          `shouldBe` Right (RExt "x" T.int32 (RExt "y" T.bool RNil))

      it "parses three field row '{a: string | b: float | c: char | {}}'" $
        parseType "{a: string | b: float | c: char | {}}"
          `shouldBe` Right (RExt "a" T.string (RExt "b" T.float (RExt "c" T.char RNil)))

      it "parses row with opaque tail '{x: int32 | *}'" $
        parseType "{x: int32 | *}" `shouldBe` Right (RExt "x" T.int32 T.opaque)

      it "parses row with backtick field '{`field-name`: int32 | {}}'" $
        parseType "{`field-name`: int32 | {}}" `shouldBe` Right (RExt "field-name" T.int32 RNil)

      it "parses row with multiple backtick fields '{`x-coord`: float | `y-coord`: float | {}}'" $
        parseType "{`x-coord`: float | `y-coord`: float | {}}"
          `shouldBe` Right (RExt "x-coord" T.float (RExt "y-coord" T.float RNil))

      it "parses row with complex types '{x: list (int32) | y: Maybe (bool) | {}}'" $
        parseType "{x: list (int32) | y: Maybe (bool) | {}}"
          `shouldBe` Right (RExt "x" (TCon "list" [T.int32]) (RExt "y" (TCon "Maybe" [T.bool]) RNil))

    describe "record types" $ do
      it "parses 'record ({})'" $
        parseType "record ({})" `shouldBe` Right (TCon "record" [RNil])

      it "parses 'record ({x: int32 | {}})'" $
        parseType "record ({x: int32 | {}})"
          `shouldBe` Right (TCon "record" [RExt "x" T.int32 RNil])

      it "parses 'record ({name: string | age: int32 | {}})'" $
        parseType "record ({name: string | age: int32 | {}})"
          `shouldBe` Right (TCon "record" [RExt "name" T.string (RExt "age" T.int32 RNil)])

      it "parses 'record ({x: float | y: float | z: float | {}})'" $
        parseType "record ({x: float | y: float | z: float | {}})"
          `shouldBe` Right (TCon "record" [RExt "x" T.float (RExt "y" T.float (RExt "z" T.float RNil))])

      it "parses record with opaque tail 'record ({x: int32 | *})'" $
        parseType "record ({x: int32 | *})"
          `shouldBe` Right (TCon "record" [RExt "x" T.int32 T.opaque])

    describe "function types" $ do
      it "parses 'int32 / bool'" $
        parseType "int32 / bool" `shouldBe` Right (T.arrow T.int32 T.bool)

      it "parses 'string / int32'" $
        parseType "string / int32" `shouldBe` Right (T.arrow T.string T.int32)

      it "parses right-associative 'int32 / bool / string'" $
        parseType "int32 / bool / string"
          `shouldBe` Right (T.arrow T.int32 (T.arrow T.bool T.string))

      it "parses 'int32 / int32 / int32 / bool'" $
        parseType "int32 / int32 / int32 / bool"
          `shouldBe` Right (T.arrow T.int32 (T.arrow T.int32 (T.arrow T.int32 T.bool)))

      it "parses function with list 'list (int32) / bool'" $
        parseType "list (int32) / bool"
          `shouldBe` Right (T.arrow (TCon "list" [T.int32]) T.bool)

      it "parses function to list 'int32 / list (bool)'" $
        parseType "int32 / list (bool)"
          `shouldBe` Right (T.arrow T.int32 (TCon "list" [T.bool]))

      it "parses function with tuple 'tuple2(int32, bool) / string'" $
        parseType "tuple2(int32, bool) / string"
          `shouldBe` Right (T.arrow (TCon "tuple2" [T.int32, T.bool]) T.string)

      it "parses function with record 'record ({x: int32 | {}}) / bool'" $
        parseType "record ({x: int32 | {}}) / bool"
          `shouldBe` Right (T.arrow (TCon "record" [RExt "x" T.int32 RNil]) T.bool)

    describe "parenthesized types" $ do
      it "parses '(int32)'" $
        parseType "(int32)" `shouldBe` Right T.int32

      it "parses '(bool)'" $
        parseType "(bool)" `shouldBe` Right T.bool

      it "parses nested '((string))'" $
        parseType "((string))" `shouldBe` Right T.string

      it "parses '(int32 / bool)'" $
        parseType "(int32 / bool)" `shouldBe` Right (T.arrow T.int32 T.bool)

      it "parses left-associative with parens '(int32 / bool) / string'" $
        parseType "(int32 / bool) / string"
          `shouldBe` Right (T.arrow (T.arrow T.int32 T.bool) T.string)

    describe "complex type combinations" $ do
      it "parses 'list (int32 / bool)'" $
        parseType "list (int32 / bool)"
          `shouldBe` Right (TCon "list" [T.arrow T.int32 T.bool])

      it "parses 'list (list (int32)) / bool'" $
        parseType "list (list (int32)) / bool"
          `shouldBe` Right (T.arrow (TCon "list" [TCon "list" [T.int32]]) T.bool)

      it "parses 'Maybe (int32) / Maybe (bool)'" $
        parseType "Maybe (int32) / Maybe (bool)"
          `shouldBe` Right (T.arrow (TCon "Maybe" [T.int32]) (TCon "Maybe" [T.bool]))

      it "parses 'record ({x: int32 | {}}) / record ({y: bool | {}})'" $
        parseType "record ({x: int32 | {}}) / record ({y: bool | {}})"
          `shouldBe` Right
            ( T.arrow
                (TCon "record" [RExt "x" T.int32 RNil])
                (TCon "record" [RExt "y" T.bool RNil])
            )

      it "parses 'tuple2(int32, bool) / tuple2(string, float)'" $
        parseType "tuple2(int32, bool) / tuple2(string, float)"
          `shouldBe` Right
            ( T.arrow
                (TCon "tuple2" [T.int32, T.bool])
                (TCon "tuple2" [T.string, T.float])
            )

      it "parses 'list (Maybe (int32))'" $
        parseType "list (Maybe (int32))"
          `shouldBe` Right (TCon "list" [TCon "Maybe" [T.int32]])

      it "parses 'Maybe (list (int32))'" $
        parseType "Maybe (list (int32))"
          `shouldBe` Right (TCon "Maybe" [TCon "list" [T.int32]])

    describe "whitespace handling" $ do
      it "handles extra spaces 'int32 / bool'" $
        parseType "  int32   /   bool  " `shouldBe` Right (T.arrow T.int32 T.bool)

      it "handles no spaces 'int32/bool'" $
        parseType "int32/bool" `shouldBe` Right (T.arrow T.int32 T.bool)

      it "handles newlines in 'list\n(\nint32\n)'" $
        parseType "list\n(\nint32\n)" `shouldBe` Right (TCon "list" [T.int32])

      it "handles spaces in row '{  x  :  int32  |  {}  }'" $
        parseType "{  x  :  int32  |  {}  }" `shouldBe` Right (RExt "x" T.int32 RNil)

    describe "error cases" $ do
      it "fails on empty input" $
        parseType "" `shouldSatisfy` isLeft

      it "fails on lowercase type constructor 'null'" $
        parseType "null" `shouldSatisfy` isLeft

      it "fails on lowercase type constructor 'maybe'" $
        parseType "maybe" `shouldSatisfy` isLeft

      it "parses lowercase type constructor with args 'tuple2(int32, bool)'" $
        parseType "tuple2(int32, bool)" `shouldBe` Right (TCon "tuple2" [T.int32, T.bool])

      it "fails on unclosed list 'list (int32'" $
        parseType "list (int32" `shouldSatisfy` isLeft

      it "fails on unclosed row '{x: int32 | {}'" $
        parseType "{x: int32 | {}" `shouldSatisfy` isLeft

      it "fails on row without tail '{x: int32}'" $
        parseType "{x: int32}" `shouldSatisfy` isLeft

      it "fails on tuple without size 'tuple (int32, bool)'" $
        parseType "tuple (int32, bool)" `shouldSatisfy` isLeft

      it "fails on unclosed parenthesis '(int32'" $
        parseType "(int32" `shouldSatisfy` isLeft

-- Helper function to check if result is Left
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False
