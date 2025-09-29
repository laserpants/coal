{-# LANGUAGE StrictData #-}

module Coal.Compiler.Error (ErrorLocation (..), CompilerError (..), CompilerFailureMode (..)) where

import Coal.Parser (ParserError)
import Data.Text (Text)
import Extra (Name)

-- TODO
-- data FoldError a
--  = FoldPatternOutsideConstructor a
--  | FoldPatternInRegularMatch a
--  deriving (Show, Eq, Ord, Read)

data ErrorLocation a = ErrorLocation Name a
  deriving (Show, Eq)

data CompilerError a
  = ParserError FilePath ParserError
  | MisplacedImportStatement (ErrorLocation a)
  | ModuleNotFound Name (ErrorLocation a)
  | NonExhaustivePatterns (ErrorLocation a)
  | TODO
  deriving (Show, Eq)

data CompilerFailureMode
  = ParserFailure
  | PreflightFailure
  | PatternAnomaly
  | CompilerError Text
  deriving (Show, Eq, Ord, Read)
