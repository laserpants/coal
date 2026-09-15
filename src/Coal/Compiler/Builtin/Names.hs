{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Builtin.Names (builtinNames) where

import qualified Data.Set as Set
import Extras (Name, Set)

builtinNames :: Set Name
builtinNames =
  Set.fromList
    [ "(%)"
    , "(*)"
    , "(+)"
    , "(-)"
    , "(/)"
    , "(<>)"
    , "(==)"
    , "(!=)"
    , "NumericBase"
    , "Comparable"
    , "Divisible"
    , "EqualTo"
    , "GreaterThan"
    , "IO"
    , "LessThan"
    , "Modulo"
    , "None"
    , "Numeric"
    , "Option"
    , "Result"
    , "Ok"
    , "Err"
    , "Ordered"
    , "Ordering"
    , "Semigroup"
    , "Some"
    , "Machine"
    , "compare"
    , "from_int32"
    , "from_int64"
    , "from_bignum"
    , "from_negative_int32"
    , "from_negative_int64"
    , "from_negative_bignum"
    , "negate"
    ]
