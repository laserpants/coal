{-# LANGUAGE OverloadedStrings #-}

{- |
Module: Coal.Parser.Type

Type and kind expression parsers.

Parses type expressions including arrows, applications, records, rows,
intrinsic types, and kind annotations.
-}
module Coal.Parser.Type (parseType, parseKind) where

import Coal.Language
import Coal.Parser.Core (Parser, nonEmpty)
import Coal.Parser.Identifier (constructor, name)
import Coal.Parser.Symbol
import Coal.Parser.Type.Intrinsic (parseIntrinsic)
import Coal.Parser.Utils (fieldList)
import Control.Monad.Combinators.Expr (Operator (InfixR), makeExprParser)
import qualified Control.Monad.Combinators.Expr as Combinators
import Data.Functor (($>))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Text.Megaparsec (option, optional, try, (<|>))

parseIntrinsicType :: Parser (Type Parameter ())
parseIntrinsicType = TIntrinsic <$> parseIntrinsic

parseTypeAtom :: Parser (Type Parameter ())
parseTypeAtom =
  try parseTypeApplication
    <|> try parseTupleType
    <|> parseRecordType
    <|> parseIntrinsicType
    <|> parseTypeParameter
    <|> parens parseType

parseType :: Parser (Type Parameter ())
parseType = makeExprParser parseTypeAtom typeOperator

{-# INLINE parseTypeParameter #-}
parseTypeParameter :: Parser (Type Parameter ())
parseTypeParameter = TVariable . Parameter () <$> name

{-# INLINE parseTypeConstructor #-}
parseTypeConstructor :: Parser (Type Parameter ())
parseTypeConstructor = TConstructor () <$> constructor

parseTupleType :: Parser (Type Parameter ())
parseTupleType = do
  ts <- parens (nonEmpty (commaSep2 parseType))
  pure $ applyTypeArgs () (TConstructor () (tupleTypeConstructor (length ts))) ts

parseRecordType :: Parser (Type Parameter ())
parseRecordType =
  braces $ do
    fields <- optional (fieldList parseType ":")
    let dict = maybe mempty Map.fromList fields
    -- Optional row variable for polymorphic records (e.g., { x : int32 | r })
    param <- optional rest
    pure (TRecord (TRow (fromDictionary dict (maybe RNil RVariable param))))
 where
  rest = pipe >> Parameter () <$> name

parseTypeApplication :: Parser (Type Parameter ())
parseTypeApplication = do
  t0 <- parseTypeConstructor <|> parseTypeParameter
  ts <- option [] (angleBrackets (commaSep1 parseType))
  case ts of
    t : ts1 ->
      pure (applyTypeArgs () t0 (t :| ts1))
    [] ->
      pure t0

typeOperator :: [[Combinators.Operator Parser (Type Parameter ())]]
typeOperator = [[InfixR (TArrow <$ symbol "->")]]

parseKind :: Parser Kind
parseKind = makeExprParser (symbol "*" $> KType) kindOperator

kindOperator :: [[Combinators.Operator Parser Kind]]
kindOperator = [[InfixR (KArrow <$ symbol "->")]]
