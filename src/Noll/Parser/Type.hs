{-# LANGUAGE OverloadedStrings #-}

module Noll.Parser.Type where

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Lang.Label (Label (..))
import Noll.Language
import Noll.Parser
import Noll.Parser.Identifier
import Noll.Parser.Symbol
import Text.Megaparsec (option, optional, (<|>))
import Text.Megaparsec.Char (char)

typeParser :: Parser (Type Parameter ())
-- TODO
typeParser = lexeme "int32" *> pure (TIntrinsic IInt32)
