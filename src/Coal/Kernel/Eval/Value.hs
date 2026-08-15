{-# LANGUAGE LambdaCase #-}

{- |
Runtime value representation.

Defines the value types produced by the interpreter:

  * Primitive values (unit, bool, integers, floats, chars, strings)
  * Data constructors (fully or partially applied)
  * Records (ordered field mappings)
  * Closures (function values with captured environments)
  * External function handles

Includes a simple 'showValue' function for debugging output.
-}
module Coal.Kernel.Eval.Value (
  Value (..),
  Closure (..),
  showValue,
) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Int (Int32, Int64)
import Data.List (intercalate)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Coal.Common.Name (Name)
import Coal.Kernel.Language.Expr (Expr, Label)
import Coal.Kernel.Language.Type (Type)

-- | Runtime values produced by evaluating language expressions.
data Value
  = VUnit
  | VBool Bool
  | VInt32 Int32
  | VInt64 Int64
  | VBignum Integer
  | VFloat Float
  | VDouble Double
  | VChar Int32
  | VString ByteString
  | {- | A fully-applied or partially-applied data constructor.
    The Name is the constructor's qualified name (e.g. "Main.Node").
    The Int is the declared constructor index (from DData).
    -}
    VConstructor Name Int [Value]
  | -- | A record value: ordered mapping from field name to value.
    VRecord (Map Name Value)
  | -- | A closure: a function waiting for arguments.
    VClosure Closure
  | -- | A reference to a host-provided external function (coal_* etc.).
    VExtern Name
  deriving (Eq, Ord)

{- | A closure packages the evaluation environment, parameter list, body,
and any arguments already accumulated from partial application.
-}
data Closure = Closure
  { closureName :: Name
  -- ^ Qualified name of the underlying function (for error messages).
  , closureParams :: [Label Type]
  -- ^ Remaining unapplied parameter labels (head = next expected).
  , closureBody :: Expr Type
  -- ^ Body expression to evaluate when fully saturated.
  , closureEnv :: Map Name Value
  -- ^ Captured variable bindings at closure-creation time.
  }
  deriving (Eq, Ord)

showValue :: Value -> String
showValue =
  \case
    VUnit ->
      "()"
    VBool True ->
      "true"
    VBool False ->
      "false"
    VInt32 n ->
      show n
    VInt64 n ->
      show n
    VBignum n ->
      show n
    VFloat f ->
      show f
    VDouble d ->
      show d
    VChar c ->
      show (toEnum (fromIntegral c) :: Char)
    VString bs ->
      show (BS.unpack bs)
    VConstructor name _ [] ->
      show name
    VConstructor name _ args ->
      show name <> "(" <> intercalate ", " (showValue <$> args) <> ")"
    VRecord m ->
      "{ "
        <> intercalate
          " | "
          [ show k <> " = " <> showValue v
          | (k, v) <- Map.toAscList m
          ]
        <> " }"
    VClosure c ->
      "<closure:" <> show (closureName c) <> "/" <> show (length (closureParams c)) <> ">"
    VExtern name ->
      "<extern:" <> show name <> ">"
