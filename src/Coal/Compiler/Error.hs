{-# LANGUAGE StrictData #-}

module Coal.Compiler.Error (CompilerError (..)) where

import Data.Text (Text)

-- TODO
newtype CompilerError = CompilerError Text
  deriving (Show, Eq, Ord, Read)
