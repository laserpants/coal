{-# LANGUAGE OverloadedStrings #-}

module Noll.Parser.Pattern where

import Noll.Language
import Noll.Parser
import Text.Megaparsec
import Noll.Parser.Identifier

patternParser :: Parser (Pattern () ())
patternParser = undefined

variablePattern :: Parser (Pattern () ())
variablePattern = undefined

literalPattern :: Parser (Pattern () ())
literalPattern = undefined

atVariablePattern :: Parser (Pattern () ())
atVariablePattern = undefined

asPattern :: Parser (Pattern () ())
asPattern = undefined
