{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Coal.Kernel.LLVM.IRError (
  IRGenError (..),
  prettyIRError,
) where

import Coal.Kernel.LLVM.IRType (IRType)
import Data.Text (Text)
import qualified Data.Text as Text
import Extras (Name)
import GHC.Generics (Generic)

-- | Errors that can occur during LLVM IR generation
data IRGenError
  = -- | Variable referenced but not in scope
    UnboundVariable Name
  | -- | Constructor name not found in constructor environment
    UnboundConstructor Name
  | -- | External declaration is not a function type
    InvalidExternalType Name IRType
  | -- | Struct field index out of bounds
    InvalidStructField IRType Int
  | -- | Expression form not supported in IR generation
    UnsupportedExpression Text
  | -- | Implementation error (should not occur in valid programs)
    ImplementationError Text
  deriving (Show, Eq, Generic)

-- | Pretty-print an IR generation error
prettyIRError :: IRGenError -> Text
prettyIRError = 
  \case
    UnboundVariable name ->
      "Name not in scope: '" <> name <> "'"
    UnboundConstructor name ->
      "No constructor '" <> name <> "'"
    InvalidExternalType name ty ->
      "External declaration '" <> name <> "' has non-function type: " <> Text.pack (show ty)
    InvalidStructField ty idx ->
      "Struct field index " <> Text.pack (show idx) <> " out of bounds for type: " <> Text.pack (show ty)
    UnsupportedExpression msg ->
      "Unsupported expression in IR generation: " <> msg
    ImplementationError msg ->
      "Implementation error: " <> msg
