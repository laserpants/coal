{-# LANGUAGE OverloadedStrings #-}

{- |
Module: Coal.Parser.Operator

Binary operator parsing utilities.

Provides helper functions to eliminate code duplication in operator parsers.
All binary operators follow the same pattern: they desugar to function
applications of the operator name.
-}
module Coal.Parser.Operator (
  binaryOperator,
  parseAdditionOperator,
  parseSubtractionOperator,
  parseMultiplicationOperator,
  parseDivisionOperator,
  parseModulusOperator,
  parseExponentiationOperator,
  parseSemigroupOperator,
  parseEqualityOperator,
  parseInequalityOperator,
  parseLessThanOrEqualOperator,
  parseGreaterThanOrEqualOperator,
  parseLessThanOperator,
  parseGreaterThanOperator,
) where

import Coal.AST.Metadata (Metadata)
import Coal.Common.Label (Label (..))
import Coal.Language (Expression (..))
import Coal.Parser.Core (Parser)
import Coal.Parser.Metadata (withMetadata)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)

{- | Generic binary operator parser

Creates a parser for a binary operator that desugars to a function application.
For example, @binaryOperator "(+)"@ creates a parser that transforms @a + b@
into @(+)(a, b)@.
-}
binaryOperator ::
  Text ->
  Parser (Expression Metadata () () -> Expression Metadata () () -> Expression Metadata () ())
binaryOperator opName =
  withMetadata $
    pure
      ( \loc lhs rhs ->
          EApplication
            loc
            ()
            (EVariable loc (Label () opName))
            (lhs :| [rhs])
      )

-- | Parser for addition operator: @+@
parseAdditionOperator ::
  Parser (Expression Metadata () () -> Expression Metadata () () -> Expression Metadata () ())
parseAdditionOperator = binaryOperator "(+)"

-- | Parser for subtraction operator: @-@
parseSubtractionOperator ::
  Parser (Expression Metadata () () -> Expression Metadata () () -> Expression Metadata () ())
parseSubtractionOperator = binaryOperator "(-)"

-- | Parser for multiplication operator: @*@
parseMultiplicationOperator ::
  Parser (Expression Metadata () () -> Expression Metadata () () -> Expression Metadata () ())
parseMultiplicationOperator = binaryOperator "(*)"

-- | Parser for division operator: @/@
parseDivisionOperator ::
  Parser (Expression Metadata () () -> Expression Metadata () () -> Expression Metadata () ())
parseDivisionOperator = binaryOperator "(/)"

-- | Parser for modulus operator: @%@
parseModulusOperator ::
  Parser (Expression Metadata () () -> Expression Metadata () () -> Expression Metadata () ())
parseModulusOperator = binaryOperator "(%)"

-- | Parser for exponentiation operator: @^@
parseExponentiationOperator ::
  Parser (Expression Metadata () () -> Expression Metadata () () -> Expression Metadata () ())
parseExponentiationOperator = binaryOperator "(^)"

-- | Parser for semigroup concatenation operator: @<>@
parseSemigroupOperator ::
  Parser (Expression Metadata () () -> Expression Metadata () () -> Expression Metadata () ())
parseSemigroupOperator = binaryOperator "(<>)"

-- | Parser for equality operator: @==@
parseEqualityOperator ::
  Parser (Expression Metadata () () -> Expression Metadata () () -> Expression Metadata () ())
parseEqualityOperator = binaryOperator "(==)"

-- | Parser for inequality operator: @!=@
parseInequalityOperator ::
  Parser (Expression Metadata () () -> Expression Metadata () () -> Expression Metadata () ())
parseInequalityOperator = binaryOperator "(!=)"

-- | Parser for less-than-or-equal operator: @<=@
parseLessThanOrEqualOperator ::
  Parser (Expression Metadata () () -> Expression Metadata () () -> Expression Metadata () ())
parseLessThanOrEqualOperator = binaryOperator "(<=)"

-- | Parser for greater-than-or-equal operator: @>=@
parseGreaterThanOrEqualOperator ::
  Parser (Expression Metadata () () -> Expression Metadata () () -> Expression Metadata () ())
parseGreaterThanOrEqualOperator = binaryOperator "(>=)"

-- | Parser for less-than operator: @<@
parseLessThanOperator ::
  Parser (Expression Metadata () () -> Expression Metadata () () -> Expression Metadata () ())
parseLessThanOperator = binaryOperator "(<)"

-- | Parser for greater-than operator: @>@
parseGreaterThanOperator ::
  Parser (Expression Metadata () () -> Expression Metadata () () -> Expression Metadata () ())
parseGreaterThanOperator = binaryOperator "(>)"
