{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{- | Tests for do-notation statement parsing.

A do-block statement is one of:

@
pat <- expr;             -- monadic bind
pat = expr;              -- pure let binding
f(pat, ...) = expr;      -- pure let binding (function form)
expr;                    -- bare expression statement
@

The first three are distinguished only by the token after the pattern or
binding head (@<-@ versus @=@), so the parser has to backtrack out of the bind
alternative before trying the let alternative. A bare expression statement is
represented in the AST as a bind whose pattern is a wildcard, which keeps the
desugaring uniform.
-}
module Coal.Parser.DoNotationSpec (doNotationSpec) where

import Coal.Common.Label (Label (..))
import Coal.Compiler.Metadata (Metadata)
import Coal.Language (
  Binding (..),
  Definition (..),
  DoStatement (..),
  Expression (..),
  FunctionDefinition (..),
  Module (..),
  Pattern (..),
 )
import Coal.Parser (parseSourceFile)
import Data.Either (isLeft)
import Data.List.NonEmpty (toList)
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

{- | The name of a variable, or of the head of an application.

The right-hand side of the statements the tests use is either a bare variable
(@b = a@) or a nullary application (@a <- f()@).
-}
headName :: Expression Metadata () () -> Maybe Name
headName = \case
  EVariable _ (Label _ n) -> Just n
  EApplication _ _ e _ -> headName e
  _ -> Nothing

{- | A metadata-free view of a statement, so expectations do not depend on
source positions. Only the shapes the tests care about are recognised.
-}
data SimpleStmt
  = -- | @pat <- expr@: the bound variable and the bound expression's head
    SBind Name Name
  | -- | @expr;@ (bare statement): the expression's head
    SVoid Name
  | -- | @x = y@
    SLet Name Name
  | -- | @f(x, y, ...) = ...@: the function name and its parameter count
    SLetFunction Name Int
  | -- | @(x, y, ...) = z@: the pattern's arity and the bound expression's head
    SLetTuple Int Name
  deriving (Eq, Show)

simpleStmt :: DoStatement Metadata () () -> Maybe SimpleStmt
simpleStmt = \case
  DoBind (PVariable _ (Label _ n)) e -> SBind n <$> headName e
  DoBind (PAny _ ()) e -> SVoid <$> headName e
  DoBind _ _ -> Nothing
  DoLet (BPattern _ (PVariable _ (Label _ n)) e) -> SLet n <$> headName e
  DoLet (BPattern _ (PTuple _ _ ps) e) -> SLetTuple (length (toList ps)) <$> headName e
  DoLet (BFunction _ n ps _) -> Just (SLetFunction n (length (toList ps)))
  DoLet _ -> Nothing

-- | The statements of a do-block, or @Nothing@ for any other expression.
doStatements :: Expression Metadata () () -> Maybe [DoStatement Metadata () ()]
doStatements = \case
  EDoBlock _ stmts -> Just (toList stmts)
  _ -> Nothing

-- | Parse a do-block and view its statements without metadata.
parseStatements :: Text -> Either String [Maybe SimpleStmt]
parseStatements src = do
  e <- parseBody src
  case doStatements e of
    Nothing -> Left "expected a do-block"
    Just stmts -> Right (simpleStmt <$> stmts)

doNotationSpec :: Spec
doNotationSpec = do
  describe "do-notation statements" $ do
    it "parses a bind, a let and a bare statement in order" $
      parseStatements "do { a <- f(); b = g(); h(); }"
        `shouldBe` Right [Just (SBind "a" "f"), Just (SLet "b" "g"), Just (SVoid "h")]

    it "leaves existing bind-only blocks unchanged" $
      parseStatements "do { a <- f(); b <- g(); h(); }"
        `shouldBe` Right [Just (SBind "a" "f"), Just (SBind "b" "g"), Just (SVoid "h")]

    it "parses a let statement referring to an earlier bind result" $
      parseStatements "do { a <- f(); b = a; h(); }"
        `shouldBe` Right [Just (SBind "a" "f"), Just (SLet "b" "a"), Just (SVoid "h")]

    it "parses a function-form let statement" $
      parseStatements "do { f(a, b) = g(); h(); }"
        `shouldBe` Right [Just (SLetFunction "f" 2), Just (SVoid "h")]

    it "parses a tuple-pattern let statement" $
      parseStatements "do { (a, b) = p; h(); }"
        `shouldBe` Right [Just (SLetTuple 2 "p"), Just (SVoid "h")]

    it "parses a let statement as the last statement" $
      parseStatements "do { a <- f(); b = c; }"
        `shouldBe` Right [Just (SBind "a" "f"), Just (SLet "b" "c")]

    it "parses a let statement as the only statement" $
      parseStatements "do { b = c; }"
        `shouldBe` Right [Just (SLet "b" "c")]

    it "rejects a let statement with no right-hand side" $
      parseStatements "do { x = ; }" `shouldSatisfy` isLeft

    it "rejects a let statement with no terminator" $
      parseStatements "do { x = y }" `shouldSatisfy` isLeft

    it "rejects a bind statement with no right-hand side" $
      parseStatements "do { x <- ; }" `shouldSatisfy` isLeft
