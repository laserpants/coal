{- |
Module: Coal.Parser

Public API for the Coal parser.

This module provides the minimal public interface for parsing Coal source files.
Internal parser modules (Core, Expression, Pattern, etc.) are not exposed.
-}
module Coal.Parser (
  parseSourceFile,
  ParserError,
) where

import Coal.Parser.Core (ParserError)
import Coal.Parser.Module (parseSourceFile)
