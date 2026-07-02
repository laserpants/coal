{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
External function bindings.

Provides the mechanism for calling host (Haskell) functions from Coal kernel
language programs. External functions are declared in source code with
@external@ and bound to Haskell implementations via the 'ExternTable'.

= Default externals

Includes 'defaultExterns' with handlers for:

  * @coal_print_*@ and @coal_println_*@ for all eight primitive types:
    @int32@, @int64@, @bool@, @string@, @char@, @float@, @double@, @bignum@

All handlers run in 'IO' and can perform real side effects.
-}
module Coal.Kernel.Eval.External (
  ExternTable,
  defaultExterns,
  lookupExtern,
  callExtern,
) where

import qualified Data.ByteString.Char8 as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Coal.Kernel.Eval.State (EvalError (..), EvalM, getExterns, liftIO, throwEval)
import Coal.Kernel.Eval.Value (Value (..))
import Common (Name)

{- | A map from external function names to host handlers.

Handlers run in 'IO' so they can perform real side effects.
-}
type ExternTable = Map Name ([Value] -> IO (Either EvalError Value))

-- ---------------------------------------------------------------------------
-- Default extern implementations for the coal_* family
-- ---------------------------------------------------------------------------

{- | A default set of handlers covering the externals used in tests and
examples.

Side effects go to stdout. Return value is always 'VUnit' (the opaque void).
-}
defaultExterns :: ExternTable
defaultExterns =
  Map.fromList
    [
      ( "coal_print_int32"
      , \case
          [VInt32 n] -> do
            putStr (show n)
            return (Right VUnit)
          args -> return (Left (ArityMismatch "coal_print_int32" 1 (length args)))
      )
    ,
      ( "coal_print_int64"
      , \case
          [VInt64 n] -> do
            putStr (show n)
            return (Right VUnit)
          args -> return (Left (ArityMismatch "coal_print_int64" 1 (length args)))
      )
    ,
      ( "coal_print_bool"
      , \case
          [VBool b] -> do
            putStr (if b then "true" else "false")
            return (Right VUnit)
          args -> return (Left (ArityMismatch "coal_print_bool" 1 (length args)))
      )
    ,
      ( "coal_print_string"
      , \case
          [VString bs] -> do
            putStr (BS.unpack bs)
            return (Right VUnit)
          args -> return (Left (ArityMismatch "coal_print_string" 1 (length args)))
      )
    ,
      ( "coal_print_char"
      , \case
          [VChar c] -> do
            putChar (toEnum (fromIntegral c))
            return (Right VUnit)
          args -> return (Left (ArityMismatch "coal_print_char" 1 (length args)))
      )
    ,
      ( "coal_print_float"
      , \case
          [VFloat f] -> do
            putStr (show f)
            return (Right VUnit)
          args -> return (Left (ArityMismatch "coal_print_float" 1 (length args)))
      )
    ,
      ( "coal_println_float"
      , \case
          [VFloat f] -> do
            print f
            return (Right VUnit)
          args -> return (Left (ArityMismatch "coal_println_float" 1 (length args)))
      )
    ,
      ( "coal_print_double"
      , \case
          [VDouble d] -> do
            putStr (show d)
            return (Right VUnit)
          args -> return (Left (ArityMismatch "coal_print_double" 1 (length args)))
      )
    ,
      ( "coal_println_double"
      , \case
          [VDouble d] -> do
            print d
            return (Right VUnit)
          args -> return (Left (ArityMismatch "coal_println_double" 1 (length args)))
      )
    ,
      ( "coal_println_int32"
      , \case
          [VInt32 n] -> do
            putStrLn (show n)
            return (Right VUnit)
          args -> return (Left (ArityMismatch "coal_println_int32" 1 (length args)))
      )
    ,
      ( "coal_println_int64"
      , \case
          [VInt64 n] -> do
            putStrLn (show n)
            return (Right VUnit)
          args -> return (Left (ArityMismatch "coal_println_int64" 1 (length args)))
      )
    ,
      ( "coal_println_bool"
      , \case
          [VBool b] -> do
            putStrLn (if b then "true" else "false")
            return (Right VUnit)
          args -> return (Left (ArityMismatch "coal_println_bool" 1 (length args)))
      )
    ,
      ( "coal_println_string"
      , \case
          [VString bs] -> do
            putStrLn (BS.unpack bs)
            return (Right VUnit)
          args -> return (Left (ArityMismatch "coal_println_string" 1 (length args)))
      )
    ,
      ( "coal_println_char"
      , \case
          [VChar c] -> do
            putStrLn [toEnum (fromIntegral c)]
            return (Right VUnit)
          args -> return (Left (ArityMismatch "coal_println_char" 1 (length args)))
      )
    ,
      ( "coal_print_bignum"
      , \case
          [VBignum n] -> do
            putStr (show n)
            return (Right VUnit)
          args -> return (Left (ArityMismatch "coal_print_bignum" 1 (length args)))
      )
    ,
      ( "coal_println_bignum"
      , \case
          [VBignum n] -> do
            print n
            return (Right VUnit)
          args -> return (Left (ArityMismatch "coal_println_bignum" 1 (length args)))
      )
    ]

-- ---------------------------------------------------------------------------
-- Dispatch helpers
-- ---------------------------------------------------------------------------

lookupExtern :: Name -> ExternTable -> Maybe ([Value] -> IO (Either EvalError Value))
lookupExtern = Map.lookup

-- | Look up and call an external function by name inside EvalM.
callExtern :: Name -> [Value] -> EvalM Value
callExtern name args = do
  externs <- getExterns
  case Map.lookup name externs of
    Nothing -> throwEval (UnboundExternal name)
    Just handler -> do
      result <- liftIO (handler args)
      case result of
        Left err -> throwEval err
        Right v -> return v
