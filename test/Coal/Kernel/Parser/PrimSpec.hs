{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Kernel.Parser.PrimSpec (spec) where

import Coal.Kernel.Language.Prim (Prim (..))
import Coal.Kernel.Parser.Prim (prim)
import qualified Data.Text.Encoding as TE
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldSatisfy)
import Text.Megaparsec (parse)

-- | Test specification for the Prim parser
spec :: Spec
spec = do
  describe "prim parser" $ do
    describe "unit value" $ do
      it "parses '()' as PUnit" $
        parse prim "" "()" `shouldBe` Right PUnit

      it "fails on incomplete unit '('" $
        parse prim "" "(" `shouldSatisfy` isLeft

      it "fails on incomplete unit ')'" $
        parse prim "" ")" `shouldSatisfy` isLeft

    describe "boolean values" $ do
      it "parses 'true' as PBool True" $
        parse prim "" "true" `shouldBe` Right (PBool True)

      it "parses 'false' as PBool False" $
        parse prim "" "false" `shouldBe` Right (PBool False)

      it "fails on 'True' (capital T)" $
        parse prim "" "True" `shouldSatisfy` isLeft

      it "fails on 'FALSE' (all caps)" $
        parse prim "" "FALSE" `shouldSatisfy` isLeft

    describe "int32 values" $ do
      it "parses positive integer '42'" $
        parse prim "" "42" `shouldBe` Right (PInt32 42)

      it "parses zero '0'" $
        parse prim "" "0" `shouldBe` Right (PInt32 0)

      it "parses negative integer '-42'" $
        parse prim "" "-42" `shouldBe` Right (PInt32 (-42))

      it "parses large int32 '2147483647' (max Int32)" $
        parse prim "" "2147483647" `shouldBe` Right (PInt32 2147483647)

      it "parses min int32 '-2147483648'" $
        parse prim "" "-2147483648" `shouldBe` Right (PInt32 (-2147483648))

      it "correctly identifies '42.0' as double, not int32" $
        parse prim "" "42.0" `shouldBe` Right (PDouble 42.0)

      it "correctly identifies '%42' as int64, not int32" $
        parse prim "" "%42" `shouldBe` Right (PInt64 42)

    describe "int64 values" $ do
      it "parses '%42' as PInt64" $
        parse prim "" "%42" `shouldBe` Right (PInt64 42)

      it "parses '%0' as zero" $
        parse prim "" "%0" `shouldBe` Right (PInt64 0)

      it "parses negative int64 '%-999'" $
        parse prim "" "%-999" `shouldBe` Right (PInt64 (-999))

      it "parses large int64 '%9223372036854775807' (max Int64)" $
        parse prim "" "%9223372036854775807" `shouldBe` Right (PInt64 9223372036854775807)

      it "parses min int64 '%-9223372036854775808'" $
        parse prim "" "%-9223372036854775808" `shouldBe` Right (PInt64 (-9223372036854775808))

      it "does not parse double percent '%%42' as int64" $
        case parse prim "" "%%42" of
          Right (PInt64 _) -> expectationFailure "Should not parse as PInt64"
          _ -> return ()

    describe "bignum values" $ do
      it "parses '%%123' as PBignum" $
        parse prim "" "%%123" `shouldBe` Right (PBignum 123)

      it "parses '%%0' as zero" $
        parse prim "" "%%0" `shouldBe` Right (PBignum 0)

      it "parses negative bignum '%%-999'" $
        parse prim "" "%%-999" `shouldBe` Right (PBignum (-999))

      it "parses very large bignum '%%123456789012345678901234567890'" $
        parse prim "" "%%123456789012345678901234567890"
          `shouldBe` Right (PBignum 123456789012345678901234567890)

      it "parses large negative bignum '%%-999999999999999999999'" $
        parse prim "" "%%-999999999999999999999"
          `shouldBe` Right (PBignum (-999999999999999999999))

      it "requires two percent signs" $
        parse prim "" "%123" `shouldSatisfy` \case
          Right (PBignum _) -> False
          _ -> True

    describe "double values" $ do
      it "parses '3.14' as PDouble" $
        parse prim "" "3.14" `shouldBe` Right (PDouble 3.14)

      it "parses '0.0' as zero" $
        parse prim "" "0.0" `shouldBe` Right (PDouble 0.0)

      it "parses negative double '-3.14'" $
        parse prim "" "-3.14" `shouldBe` Right (PDouble (-3.14))

      it "parses double with many decimals '3.141592653589793'" $
        parse prim "" "3.141592653589793" `shouldBe` Right (PDouble 3.141592653589793)

      it "parses scientific notation '1.5e10'" $
        parse prim "" "1.5e10" `shouldBe` Right (PDouble 1.5e10)

      it "parses negative exponent '2.5e-3'" $
        parse prim "" "2.5e-3" `shouldBe` Right (PDouble 2.5e-3)

      it "does not parse double when 'f' suffix present '3.14f'" $
        case parse prim "" "3.14f" of
          Right (PDouble _) -> expectationFailure "Should not parse as PDouble with 'f' suffix"
          _ -> return ()

    describe "float values" $ do
      it "parses '2.5f' as PFloat" $
        parse prim "" "2.5f" `shouldBe` Right (PFloat 2.5)

      it "parses '5.0F' with capital F" $
        parse prim "" "5.0F" `shouldBe` Right (PFloat 5.0)

      it "parses '0.0f' as zero" $
        parse prim "" "0.0f" `shouldBe` Right (PFloat 0.0)

      it "parses negative float '-2.5f'" $
        parse prim "" "-2.5f" `shouldBe` Right (PFloat (-2.5))

      it "parses float with many decimals '3.141592f'" $
        parse prim "" "3.141592f" `shouldBe` Right (PFloat 3.141592)

      it "parses scientific notation with f suffix '1.5e10f'" $
        parse prim "" "1.5e10f" `shouldBe` Right (PFloat 1.5e10)

      it "requires 'f' or 'F' suffix" $
        parse prim "" "2.5" `shouldSatisfy` \case
          Right (PFloat _) -> False
          _ -> True

    describe "string values" $ do
      it "parses empty string \"\"" $
        parse prim "" "\"\"" `shouldBe` Right (PString "")

      it "parses simple string \"hello\"" $
        parse prim "" "\"hello\"" `shouldBe` Right (PString "hello")

      it "parses string with spaces \"hello world\"" $
        parse prim "" "\"hello world\"" `shouldBe` Right (PString "hello world")

      it "parses string with newline escape \"test\\nline\"" $
        parse prim "" "\"test\\nline\"" `shouldBe` Right (PString "test\nline")

      it "parses string with tab escape \"tab\\there\"" $
        parse prim "" "\"tab\\there\"" `shouldBe` Right (PString "tab\there")

      it "parses string with backslash escape \"path\\\\file\"" $
        parse prim "" "\"path\\\\file\"" `shouldBe` Right (PString "path\\file")

      it "parses string with quote escape \"say \\\"hi\\\"\"" $
        parse prim "" "\"say \\\"hi\\\"\"" `shouldBe` Right (PString "say \"hi\"")

      it "parses string with carriage return \"line\\rreturn\"" $
        parse prim "" "\"line\\rreturn\"" `shouldBe` Right (PString "line\rreturn")

      it "parses string with null character \"null\\0char\"" $
        parse prim "" "\"null\\0char\"" `shouldBe` Right (PString "null\0char")

      it "parses Unicode string \"Hello 世界\"" $
        parse prim "" "\"Hello 世界\"" `shouldBe` Right (PString (TE.encodeUtf8 "Hello 世界"))

      it "fails on unclosed string \"hello" $
        parse prim "" "\"hello" `shouldSatisfy` isLeft

    describe "char values" $ do
      it "parses 'x' as character" $
        parse prim "" "'x'" `shouldBe` Right (PChar 120)

      it "parses 'A' as uppercase character" $
        parse prim "" "'A'" `shouldBe` Right (PChar 65)

      it "parses space ' '" $
        parse prim "" "' '" `shouldBe` Right (PChar 32)

      it "parses newline escape '\\n'" $
        parse prim "" "'\\n'" `shouldBe` Right (PChar 10)

      it "parses tab escape '\\t'" $
        parse prim "" "'\\t'" `shouldBe` Right (PChar 9)

      it "parses carriage return '\\r'" $
        parse prim "" "'\\r'" `shouldBe` Right (PChar 13)

      it "parses backspace '\\b'" $
        parse prim "" "'\\b'" `shouldBe` Right (PChar 8)

      it "parses form feed '\\f'" $
        parse prim "" "'\\f'" `shouldBe` Right (PChar 12)

      it "parses backslash '\\\\'" $
        parse prim "" "'\\\\'" `shouldBe` Right (PChar 92)

      it "parses single quote '\\''" $
        parse prim "" "'\\''" `shouldBe` Right (PChar 39)

      it "parses double quote '\\\"'" $
        parse prim "" "'\\\"'" `shouldBe` Right (PChar 34)

      it "parses null character '\\0'" $
        parse prim "" "'\\0'" `shouldBe` Right (PChar 0)

      it "parses Unicode character '世'" $
        parse prim "" "'世'" `shouldBe` Right (PChar 19990)

      it "fails on empty char ''" $
        parse prim "" "''" `shouldSatisfy` isLeft

      it "fails on unclosed char 'x" $
        parse prim "" "'x" `shouldSatisfy` isLeft

    describe "parser precedence and disambiguation" $ do
      it "parses %% as bignum, not int64" $
        case parse prim "" "%%42" of
          Right (PBignum 42) -> return ()
          Right other -> expectationFailure $ "Expected PBignum, got " <> show other
          Left _ -> expectationFailure "Parse failed"

      it "parses single % as int64, not int32" $
        case parse prim "" "%42" of
          Right (PInt64 42) -> return ()
          Right other -> expectationFailure $ "Expected PInt64, got " <> show other
          Left _ -> expectationFailure "Parse failed"

      it "parses number without prefix as int32" $
        case parse prim "" "42" of
          Right (PInt32 42) -> return ()
          Right other -> expectationFailure $ "Expected PInt32, got " <> show other
          Left _ -> expectationFailure "Parse failed"

      it "parses decimal with f as float, not double" $
        case parse prim "" "3.14f" of
          Right (PFloat _) -> return ()
          Right other -> expectationFailure $ "Expected PFloat, got " <> show other
          Left _ -> expectationFailure "Parse failed"

      it "parses decimal without suffix as double, not float" $
        case parse prim "" "3.14" of
          Right (PDouble _) -> return ()
          Right other -> expectationFailure $ "Expected PDouble, got " <> show other
          Left _ -> expectationFailure "Parse failed"

    describe "whitespace handling" $ do
      it "handles trailing whitespace '42 '" $
        parse prim "" "42 " `shouldBe` Right (PInt32 42)

      it "handles multiple trailing spaces '42   '" $
        parse prim "" "42   " `shouldBe` Right (PInt32 42)

      it "handles trailing newline '42\\n'" $
        parse prim "" "42\n" `shouldBe` Right (PInt32 42)

      it "handles trailing tab '42\\t'" $
        parse prim "" "42\t" `shouldBe` Right (PInt32 42)

    describe "edge cases and error conditions" $ do
      it "fails on empty input" $
        parse prim "" "" `shouldSatisfy` isLeft

      it "fails on whitespace only '   '" $
        parse prim "" "   " `shouldSatisfy` isLeft

      it "fails on invalid keyword 'null'" $
        parse prim "" "null" `shouldSatisfy` isLeft

      it "parses valid prefix from '123abc', consuming '123'" $
        parse prim "" "123abc" `shouldBe` Right (PInt32 123)

      it "parses valid prefix from '3.14.15', consuming '3.14'" $
        parse prim "" "3.14.15" `shouldBe` Right (PDouble 3.14)

-- Helper function to check if result is Left
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
