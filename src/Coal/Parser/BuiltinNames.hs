{-# LANGUAGE OverloadedStrings #-}

{- |
Module: Coal.Parser.BuiltinNames

Registry of special built-in function names.

These are compiler-provided functions that have special names with dollar
signs (e.g., @nat$_pack@, @io$_println_string@). This module provides a
centralized registry to make it easy to add new built-ins without modifying
parser code.
-}
module Coal.Parser.BuiltinNames (
  BuiltinCategory (..),
  builtinNames,
  isBuiltinName,
) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)

-- | Categories of built-in functions
data BuiltinCategory
  = -- | Natural number operations (pack/unpack)
    Nat
  | -- | IO operations (print, println, eval, return)
    IO
  | -- | String operations (conversion, manipulation)
    String
  | -- | Number parsing operations
    Number
  | -- | Character operations (ord, chr)
    Char
  | -- | Codata operations
    Codata
  deriving (Eq, Ord, Show)

-- | Registry of all built-in function names with their categories
builtinNames :: Map Text BuiltinCategory
builtinNames =
  Map.fromList $
    -- Nat operations
    [ ("nat$_pack", Nat)
    , ("nat$_unpack", Nat)
    ]
      ++
      -- IO operations
      [ ("io$_println_string", IO)
      , ("io$_print_string", IO)
      , ("io$_println_int32", IO)
      , ("io$_print_int32", IO)
      , ("io$_println_int64", IO)
      , ("io$_print_int64", IO)
      , ("io$_println_bignum", IO)
      , ("io$_print_bignum", IO)
      , ("io$_println_bool", IO)
      , ("io$_print_bool", IO)
      , ("io$_println_char", IO)
      , ("io$_print_char", IO)
      , ("io$_println_float", IO)
      , ("io$_print_float", IO)
      , ("io$_println_double", IO)
      , ("io$_print_double", IO)
      , ("io$_eval", IO)
      , ("io$_return", IO)
      ]
      ++
      -- String operations
      [ ("string$_char_to_string", String)
      , ("string$_bool_to_string", String)
      , ("string$_int32_to_string", String)
      , ("string$_float_to_string", String)
      , ("string$_double_to_string", String)
      , ("string$_to_list", String)
      , ("string$_from_list", String)
      , ("string$_reverse", String)
      , ("string$_remove_whitespace", String)
      , ("string$_tail", String)
      , ("string$_length", String)
      , ("string$_head_unsafe", String)
      ]
      ++
      -- Number operations
      [("number$_unsafe_parse_bignum", Number)]
      ++
      -- Char operations
      [ ("char$_ord", Char)
      , ("char$_chr", Char)
      ]
      ++
      -- Codata operations
      [ ("process$_process", Codata)
      , ("process$_map_process", Codata)
      , ("process$_contramap_input", Codata)
      , ("process$_duplicate", Codata)
      , ("machine$_machine", Codata)
      ]

-- | All built-in names as a set (for faster lookup)
builtinNameSet :: Set Text
builtinNameSet = Map.keysSet builtinNames

-- | Check if a given name is a built-in function
isBuiltinName :: Text -> Bool
isBuiltinName = (`Set.member` builtinNameSet)
