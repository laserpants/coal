{-# LANGUAGE OverloadedStrings #-}

{- |
Runtime function signatures.

Defines the interface to the C runtime library, which provides primitive
operations, memory allocation, boxing/unboxing, and closure application.
Each function is described by its name, return type, and argument types.

= Runtime library organization

The runtime is divided into several categories:

  * __Boxing/unboxing__: Convert between machine types and heap-allocated
    pointers
  * __Allocation__: Generic heap allocation (@rtAlloc@)
  * __Closures__: Closure construction and application
  * __Records__: Extensible record operations
  * __Strings and bignums__: String construction and arbitrary-precision
    arithmetic
-}
module Coal.Kernel.LLVM.RuntimeDefs (
  RuntimeFun (..),
  rtFun,

  -- * Boxing / unboxing
  rtInt32Box,
  rtInt32Unbox,
  rtInt64Box,
  rtInt64Unbox,
  rtBoolBox,
  rtBoolUnbox,
  rtFloatBox,
  rtFloatUnbox,
  rtDoubleBox,
  rtDoubleUnbox,
  rtCharBox,
  rtCharUnbox,

  -- * String
  rtStringNew,

  -- * Bignum
  rtBignumNew,

  -- * Closures / apply
  rtClosureNew,
  rtApply,

  -- * Allocation
  rtAlloc,

  -- * Records
  rtRecordEmpty,
  rtRecordExtend,
  rtRecordLookup,
) where

import LLVM.IR

import Coal.Common.Name (Name)

{- | Description of a single C runtime function that the code generator can
call.
-}
data RuntimeFun = RuntimeFun
  { rtName :: Name
  , rtRetType :: IRType
  , rtArgTypes :: [IRType]
  }

-- | Smart constructor.
rtFun :: Name -> IRType -> [IRType] -> RuntimeFun
rtFun = RuntimeFun

-- * Boxing / unboxing

rtInt32Box, rtInt32Unbox :: RuntimeFun
rtInt32Box = rtFun "rt_int32_box" TPtr [i32]
rtInt32Unbox = rtFun "rt_int32_unbox" i32 [TPtr]

rtInt64Box, rtInt64Unbox :: RuntimeFun
rtInt64Box = rtFun "rt_int64_box" TPtr [i64]
rtInt64Unbox = rtFun "rt_int64_unbox" i64 [TPtr]

rtBoolBox, rtBoolUnbox :: RuntimeFun
rtBoolBox = rtFun "rt_bool_box" TPtr [i1]
rtBoolUnbox = rtFun "rt_bool_unbox" i1 [TPtr]

rtFloatBox, rtFloatUnbox :: RuntimeFun
rtFloatBox = rtFun "rt_float_box" TPtr [TFloat]
rtFloatUnbox = rtFun "rt_float_unbox" TFloat [TPtr]

rtDoubleBox, rtDoubleUnbox :: RuntimeFun
rtDoubleBox = rtFun "rt_double_box" TPtr [TDouble]
rtDoubleUnbox = rtFun "rt_double_unbox" TDouble [TPtr]

rtCharBox, rtCharUnbox :: RuntimeFun
rtCharBox = rtFun "rt_char_box" TPtr [i32]
rtCharUnbox = rtFun "rt_char_unbox" i32 [TPtr]

-- * String

rtStringNew :: RuntimeFun
rtStringNew = rtFun "rt_string_new" TPtr [TPtr]

-- * Bignum

rtBignumNew :: RuntimeFun
rtBignumNew = rtFun "rt_bignum_new" TPtr [TPtr]

-- * Closures / apply

rtClosureNew :: RuntimeFun
rtClosureNew = rtFun "rt_closure_new" TPtr [TPtr, i32]

rtApply :: RuntimeFun
rtApply = rtFun "rt_apply" TPtr [TPtr, i32, TPtr]

-- * Allocation

rtAlloc :: RuntimeFun
rtAlloc = rtFun "rt_alloc" TPtr [i32]

-- * Records

rtRecordEmpty, rtRecordExtend, rtRecordLookup :: RuntimeFun
rtRecordEmpty = rtFun "rt_record_empty" TPtr []
rtRecordExtend = rtFun "rt_record_extend" TPtr [TPtr, TPtr, TPtr]
rtRecordLookup = rtFun "rt_record_lookup" TPtr [TPtr, TPtr]
