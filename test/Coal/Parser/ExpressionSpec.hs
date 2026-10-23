{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Tests for the surface expression parser's record-selection rules.

A selector field must be a lowercase (or underscore-initial) name. An uppercase
name after a dot is reserved for a module-qualified name, which the atom parsers
consume before any selector is considered:

@
List.map   -- qualified variable
@

Uppercase /fields/ were only ever produced by the removed @cotype@ / @unfold@
codata syntax, whose capitalized fields were selected with @x.Foo@. That syntax
no longer exists, so @x.Foo@ is now a parse error.
-}
module Coal.Parser.ExpressionSpec (expressionSpec) where

import Coal.Common.Label (Label (..))
import Coal.Compiler.Metadata (Metadata)
import Coal.Language (
  Definition (..),
  Expression (..),
  FunctionDefinition (..),
  Module (..),
  Operator (..),
  Pattern (..),
 )
import Coal.Parser (parseSourceFile)
import Data.Either (isLeft)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Extras (Name)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Text.Megaparsec (runParser)

-- | Parse @fun f() = <expr>@ inside a throwaway module and return the body.
parseBody :: Text -> Either String (Expression Metadata () ())
parseBody expr =
  case runParser parseSourceFile "" ("module M { fun f() = " <> expr <> " }") of
    Left err ->
      Left (show err)
    Right Module{moduleDefinitions} ->
      case moduleDefinitions of
        [DFunction _ _ FunctionDefinition{functionDefinitionExpression}] ->
          Right functionDefinitionExpression
        ds ->
          Left ("expected exactly one definition, got " <> show (length ds))

-- | A plain variable name, or @Nothing@ for any other expression.
variableName :: Expression Metadata () () -> Maybe Name
variableName = \case
  EVariable _ (Label _ n) -> Just n
  _ -> Nothing

{- | @Just (receiver, fields)@ for a selector chain rooted at a plain variable,
with the field names listed outermost last. Metadata-free, so expectations do
not depend on source positions.
-}
selectChain :: Expression Metadata () () -> Maybe (Name, [Name])
selectChain = \case
  EVariable _ (Label _ n) -> Just (n, [])
  ESelect _ (Label _ f) e -> do
    (n, fs) <- selectChain e
    pure (n, fs <> [f])
  _ -> Nothing

-- | The operator of a binary application, used to pin @.|@ and @|.@.
binaryOperatorName :: Expression Metadata () () -> Maybe Operator
binaryOperatorName = \case
  EApplication _ _ (EOperator _ _ op) _ -> Just op
  _ -> Nothing

-- | The name of a constructor expression, or @Nothing@ for any other expression.
constructorName :: Expression Metadata () () -> Maybe Name
constructorName = \case
  EConstructor _ (Label _ n) -> Just n
  _ -> Nothing

-- | @Just name@ for the constructor name of a lambda's first parameter pattern.
lambdaParamConstructor :: Expression Metadata () () -> Maybe Name
lambdaParamConstructor = \case
  ELambda _ (p :| _) _ ->
    case p of
      PConstructor _ (Label _ n) _ -> Just n
      _ -> Nothing
  _ -> Nothing

expressionSpec :: Spec
expressionSpec = do
  describe "record field selection" $ do
    it "accepts a lowercase field name" $
      (selectChain <$> parseBody "x.foo") `shouldBe` Right (Just ("x", ["foo"]))

    it "accepts a chain of lowercase field names" $
      (selectChain <$> parseBody "x.foo.bar") `shouldBe` Right (Just ("x", ["foo", "bar"]))

    it "accepts an underscore-initial field name" $
      (selectChain <$> parseBody "x._foo") `shouldBe` Right (Just ("x", ["_foo"]))

    it "rejects an uppercase field name" $
      parseBody "x.Foo" `shouldSatisfy` isLeft

    it "rejects an uppercase field name later in a chain" $
      parseBody "x.foo.Bar" `shouldSatisfy` isLeft

  describe "module-qualified names" $ do
    it "still parses a qualified variable" $
      (variableName <$> parseBody "List.map") `shouldBe` Right (Just "List.map")

  describe "qualified constructor names" $ do
    it "parses an unqualified constructor" $
      (constructorName <$> parseBody "Foo") `shouldBe` Right (Just "Foo")

    it "parses a two-segment qualified constructor" $
      (constructorName <$> parseBody "Foo.Bar") `shouldBe` Right (Just "Foo.Bar")

    it "parses a three-segment qualified constructor" $
      (constructorName <$> parseBody "Data.Map.Empty") `shouldBe` Right (Just "Data.Map.Empty")

    it "prefers a qualified variable when the last segment is lowercase" $
      (variableName <$> parseBody "Data.map") `shouldBe` Right (Just "Data.map")

    it "rejects a trailing dot" $
      parseBody "Data.Map." `shouldSatisfy` isLeft

    it "leaves the .| operator alone" $
      (binaryOperatorName <$> parseBody "A.B .| f") `shouldBe` Right (Just OForwardApplication)

  describe "qualified constructor patterns" $ do
    it "parses a qualified constructor in a lambda parameter" $
      (lambdaParamConstructor <$> parseBody "fn(Shapes.Circle(r)) => r")
        `shouldBe` Right (Just "Shapes.Circle")

  describe "pipeline operators" $ do
    it "still parses forward application" $
      (binaryOperatorName <$> parseBody "x .| f") `shouldBe` Right (Just OForwardApplication)

    it "still parses reverse application" $
      (binaryOperatorName <$> parseBody "x |. f") `shouldBe` Right (Just OReverseApplication)
