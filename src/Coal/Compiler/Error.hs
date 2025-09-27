{-# LANGUAGE StrictData #-}

module Coal.Compiler.Error (CompilerError (..), CompilerFailureMode (..)) where

import Coal.Parser (ParserError)
import Data.Text (Text)
import Extra (Name)

-- TODO
-- data FoldError a
--  = FoldPatternOutsideConstructor a
--  | FoldPatternInRegularMatch a
--  deriving (Show, Eq, Ord, Read)

data CompilerError a
  = ParserError FilePath ParserError
  | MisplacedImportStatement Name a
  | TODO
  deriving (Show, Eq)

data CompilerFailureMode
  = ParserFailure
  | PreflightFailure
  | CompilerError Text
  deriving (Show, Eq, Ord, Read)
