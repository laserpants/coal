{-# LANGUAGE StrictData #-}

module Coal.Compiler.Error (CompilerError (..)) where

import Data.Text (Text)

-- TODO
-- data FoldError a
--  = FoldPatternOutsideConstructor a
--  | FoldPatternInRegularMatch a
--  deriving (Show, Eq, Ord, Read)

-- TODO
newtype CompilerError = CompilerError Text
  deriving (Show, Eq, Ord, Read)
