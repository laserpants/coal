{-# LANGUAGE OverloadedStrings #-}

module Noll.Parser.Type where

import Control.Monad (void)
import Control.Monad.Combinators.Expr
import Lang.Label (Label (..))
import Noll.Language
import Noll.Parser
import Data.Functor (($>))
import Noll.Parser.Identifier
import Noll.Parser.Symbol
import Text.Megaparsec (option, optional, (<|>))
import Text.Megaparsec.Char (char)

intrinsicParser :: Parser (Intrinsic (Type Parameter ()))
intrinsicParser = lexeme "int32" $> IInt32

-- TODO
typeParser :: Parser (Type Parameter ())
typeParser = TIntrinsic <$> intrinsicParser 
