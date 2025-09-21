{-# LANGUAGE StrictData #-}

module Coal.Compiler.Error (CompilerError (..), CompilerFailureMode (..)) where

import Coal.Parser (ParserError)
import Data.Text (Text)

-- TODO
-- data FoldError a
--  = FoldPatternOutsideConstructor a
--  | FoldPatternInRegularMatch a
--  deriving (Show, Eq, Ord, Read)

data CompilerError
  = ParserError ParserError
  | TODO
  deriving (Show, Eq)

data CompilerFailureMode
  = ParserFailure
  | CompilerError Text
  deriving (Show, Eq, Ord, Read)
