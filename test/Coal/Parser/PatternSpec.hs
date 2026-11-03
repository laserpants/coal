{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Tests for constructor patterns with an empty argument list.

@Foo()@ is shorthand for @Foo(())@: an empty argument list means a single
@unit@ argument, exactly as it does in expressions. The shorthand is accepted
wherever a pattern is expected, so this spec pins the behaviour via a @match@
clause and via a lambda parameter.
-}
module Coal.Parser.PatternSpec (patternSpec) where

import Coal.Common.Label (Label (..))
import Coal.Compiler.Metadata (Metadata)
import Coal.Language (
  Clause (..),
  Definition (..),
  Expression (..),
  FunctionDefinition (..),
  Module (..),
  Pattern (..),
  Primitive (..),
 )
import Coal.Parser (parseSourceFile)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Extras (Name)
import Test.Hspec (Spec, describe, it, shouldBe)
import Text.Megaparsec (runParser)

-- | Parse @match(x) { | <pat> => 0 }@ and return the clause pattern.
parseMatchPattern :: Text -> Either String (Pattern Metadata () ())
parseMatchPattern pat =
  case runParser parseSourceFile "" src of
    Left err -> Left (show err)
    Right Module{moduleDefinitions} ->
      case moduleDefinitions of
        [DFunction _ _ FunctionDefinition{functionDefinitionExpression = EMatch _ _ _ (EClause _ p _ :| [])}] ->
          Right p
        _ -> Left "expected a single-clause match expression"
 where
  src = "module M { fun f(x) = match(x) { | " <> pat <> " => 0 } }"

-- | Parse @fn(<pat>) => 0@ and return the parameter pattern.
parseLambdaPattern :: Text -> Either String (Pattern Metadata () ())
parseLambdaPattern pat =
  case runParser parseSourceFile "" src of
    Left err -> Left (show err)
    Right Module{moduleDefinitions} ->
      case moduleDefinitions of
        [DFunction _ _ FunctionDefinition{functionDefinitionExpression = ELambda _ (p :| _) _}] ->
          Right p
        _ -> Left "expected a lambda expression"
 where
  src = "module M { fun f() = fn(" <> pat <> ") => 0 }"

{- | A metadata-free view of the pattern shapes these tests care about, so that
expectations do not depend on source positions.
-}
data SimplePattern
  = SConstructor Name [SimplePattern]
  | STuple [SimplePattern]
  | SUnit
  deriving (Eq, Show)

simplePattern :: Pattern Metadata () () -> SimplePattern
simplePattern = \case
  PConstructor _ (Label _ n) ps -> SConstructor n (simplePattern <$> ps)
  PTuple _ _ ps -> STuple (simplePattern <$> toList ps)
  PLiteral _ LUnit -> SUnit
  _ -> SConstructor "<other>" []

patternSpec :: Spec
patternSpec = do
  describe "empty constructor argument lists" $ do
    it "parses Foo() as Foo(())" $
      (simplePattern <$> parseMatchPattern "Foo()")
        `shouldBe` Right (SConstructor "Foo" [SUnit])

    it "parses Foo(()) identically" $
      (simplePattern <$> parseMatchPattern "Foo(())")
        `shouldBe` Right (SConstructor "Foo" [SUnit])

    it "leaves a nullary constructor unchanged" $
      (simplePattern <$> parseMatchPattern "Foo")
        `shouldBe` Right (SConstructor "Foo" [])

    it "parses a qualified constructor with an empty argument list" $
      (simplePattern <$> parseMatchPattern "Mod.Foo()")
        `shouldBe` Right (SConstructor "Mod.Foo" [SUnit])

    it "nests inside a constructor argument" $
      (simplePattern <$> parseMatchPattern "Foo(Bar())")
        `shouldBe` Right (SConstructor "Foo" [SConstructor "Bar" [SUnit]])

    it "is accepted in a lambda parameter" $
      (simplePattern <$> parseLambdaPattern "Bar()")
        `shouldBe` Right (SConstructor "Bar" [SUnit])

  describe "unaffected unit patterns" $ do
    it "still parses a bare unit literal" $
      (simplePattern <$> parseMatchPattern "()")
        `shouldBe` Right SUnit

    it "still parses a unit literal inside a tuple" $
      (simplePattern <$> parseMatchPattern "(Foo, ())")
        `shouldBe` Right (STuple [SConstructor "Foo" [], SUnit])
