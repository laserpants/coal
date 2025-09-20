{-# LANGUAGE StrictData #-}

module Coal.Compiler.Error (CompilerError (..)) where

import Data.Text (Text)

newtype CompilerError = CompilerError Text
  deriving (Show, Eq, Ord, Read)
