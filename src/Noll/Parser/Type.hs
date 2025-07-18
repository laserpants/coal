{-# LANGUAGE OverloadedStrings #-}

module Noll.Parser.Type where

import Lang.Common.List1 (NonEmpty (..))
import Control.Monad.Combinators.Expr
import Data.Functor (($>))
import Lang.Label (Label (..))
import Noll.Language
import Noll.Parser
import Noll.Parser.Identifier
import Noll.Parser.Symbol
import Text.Megaparsec (option, optional, (<|>), try)
import Text.Megaparsec.Char (char)

intrinsicParser :: Parser (Intrinsic (Type Parameter ()))
intrinsicParser = lexeme "int32" $> IInt32

typeConstructor = 
  lexeme "list" $> TConstructor () "list"

typeApplication = do 
  t0 <- typeConstructor
  t : ts <- parens (commaSep1 typeParser)
  pure (TApplication () t0 (t :| ts))

-- TODO
typeParser :: Parser (Type Parameter ())
typeParser = 
  try typeApplication
    <|> (TIntrinsic <$> intrinsicParser)
