{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Builtin.Functions (builtinFunctions) where

import Coal.Language
import Data.List.NonEmpty (NonEmpty (..))
import Extras (Name)

builtinFunctions :: [(Name, IndexedScheme)]
builtinFunctions =
  [
    ( "operator$__not"
    , forall0 (TIntrinsic IBool ~> TIntrinsic IBool)
    )
  ,
    ( "operator$__reverse_composition"
    , forall3 $ \t0 t1 t2 -> (t1 ~> t2) ~> (t0 ~> t1) ~> t0 ~> t2
    )
  ,
    ( "operator$__forward_composition"
    , forall3 $ \t0 t1 t2 -> (t0 ~> t1) ~> (t1 ~> t2) ~> t0 ~> t2
    )
  ,
    ( "operator$__reverse_application"
    , forall2 $ \t0 t1 -> t0 ~> (t0 ~> t1) ~> t1
    )
  ,
    ( "operator$__forward_application"
    , forall2 $ \t0 t1 -> (t0 ~> t1) ~> t0 ~> t1
    )
  ,
    ( "operator$__list_concatenation"
    , forall1 $ \t0 -> listType t0 ~> listType t0 ~> listType t0
    )
  ,
    ( "io$_println_int32"
    , forall0 (TIntrinsic IInt32 ~> applyTypeArgs KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| []))
    )
  ,
    ( "io$_println_int64"
    , forall0 (TIntrinsic IInt64 ~> applyTypeArgs KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| []))
    )
  ,
    ( "io$_println_bignum"
    , forall0 (TIntrinsic IBignum ~> applyTypeArgs KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| []))
    )
  ,
    ( "io$_println_string"
    , forall0 (TIntrinsic IString ~> applyTypeArgs KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| []))
    )
  ,
    ( "io$_println_bool"
    , forall0 (TIntrinsic IBool ~> applyTypeArgs KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| []))
    )
  ,
    ( "io$_println_char"
    , forall0 (TIntrinsic IChar ~> applyTypeArgs KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| []))
    )
  ,
    ( "io$_println_float"
    , forall0 (TIntrinsic IFloat ~> applyTypeArgs KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| []))
    )
  ,
    ( "io$_println_double"
    , forall0 (TIntrinsic IDouble ~> applyTypeArgs KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| []))
    )
  ,
    ( "io$_print_int32"
    , forall0 (TIntrinsic IInt32 ~> applyTypeArgs KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| []))
    )
  ,
    ( "io$_print_int64"
    , forall0 (TIntrinsic IInt64 ~> applyTypeArgs KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| []))
    )
  ,
    ( "io$_print_bignum"
    , forall0 (TIntrinsic IBignum ~> applyTypeArgs KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| []))
    )
  ,
    ( "io$_print_string"
    , forall0 (TIntrinsic IString ~> applyTypeArgs KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| []))
    )
  ,
    ( "io$_print_bool"
    , forall0 (TIntrinsic IBool ~> applyTypeArgs KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| []))
    )
  ,
    ( "io$_print_char"
    , forall0 (TIntrinsic IChar ~> applyTypeArgs KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| []))
    )
  ,
    ( "io$_print_float"
    , forall0 (TIntrinsic IFloat ~> applyTypeArgs KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| []))
    )
  ,
    ( "io$_print_double"
    , forall0 (TIntrinsic IDouble ~> applyTypeArgs KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| []))
    )
  ,
    ( "io$_eval"
    , forall1 $ \t0 -> applyTypeArgs KType (TConstructor (KArrow KType KType) "IO") (t0 :| []) ~> t0
    )
  ,
    ( "io$_return"
    , forall1 $ \t0 -> t0 ~> applyTypeArgs KType (TConstructor (KArrow KType KType) "IO") (t0 :| [])
    )
  ,
    ( "operator$__string_concatenation"
    , forall0 (TIntrinsic IString ~> TIntrinsic IString ~> TIntrinsic IString)
    )
  ,
    ( "string$_char_to_string"
    , forall0 (TIntrinsic IChar ~> TIntrinsic IString)
    )
  ,
    ( "string$_bool_to_string"
    , forall0 (TIntrinsic IBool ~> TIntrinsic IString)
    )
  ,
    ( "string$_int32_to_string"
    , forall0 (TIntrinsic IInt32 ~> TIntrinsic IString)
    )
  ,
    ( "string$_float_to_string"
    , forall0 (TIntrinsic IFloat ~> TIntrinsic IString)
    )
  ,
    ( "string$_double_to_string"
    , forall0 (TIntrinsic IDouble ~> TIntrinsic IString)
    )
  ,
    ( "string$_to_list"
    , forall0 (TIntrinsic IString ~> listType (TIntrinsic IChar))
    )
  ,
    ( "string$_from_list"
    , forall0 (listType (TIntrinsic IChar) ~> TIntrinsic IString)
    )
  ,
    ( "string$_tail"
    , forall0 (TIntrinsic IString ~> TIntrinsic IString)
    )
  ,
    ( "string$_reverse"
    , forall0 (TIntrinsic IString ~> TIntrinsic IString)
    )
  ,
    ( "string$_remove_whitespace"
    , forall0 (TIntrinsic IString ~> TIntrinsic IString)
    )
  ,
    ( "string$_head_unsafe"
    , forall0 (TIntrinsic IString ~> TIntrinsic IChar)
    )
  ,
    ( "string$_length"
    , forall0 (TIntrinsic IString ~> TIntrinsic IInt32)
    )
  ,
    ( "nat$_unpack"
    , forall0 (TIntrinsic INat ~> TIntrinsic IInt32)
    )
  ,
    ( "nat$_pack"
    , forall0 (TIntrinsic IInt32 ~> TIntrinsic INat)
    )
  ,
    ( "not"
    , forall0 (TIntrinsic IBool ~> TIntrinsic IBool)
    )
  ,
    ( "char$_ord"
    , forall0 (TIntrinsic IChar ~> TIntrinsic IInt32)
    )
  ,
    ( "char$_chr"
    , forall0 (TIntrinsic IInt32 ~> TIntrinsic IChar)
    )
  ,
    ( "number$_unsafe_parse_bignum"
    , forall0 (TIntrinsic IString ~> TIntrinsic IBignum)
    )
  ,
    ( "from_int32"
    , forall1' (\t0 -> ([Trait "Numeric" t0], TIntrinsic IInt32 ~> t0))
    )
  ,
    ( "from_int64"
    , forall1' (\t0 -> ([Trait "Numeric" t0], TIntrinsic IInt64 ~> t0))
    )
  ,
    ( "from_bignum"
    , forall1' (\t0 -> ([Trait "Numeric" t0], TIntrinsic IBignum ~> t0))
    )
  ,
    ( "negate"
    , forall1' (\t0 -> ([Trait "Numeric" t0], t0 ~> t0))
    )
  ,
    ( "compare"
    , forall1' (\t0 -> ([Trait "Ordered" t0], t0 ~> t0 ~> TConstructor KType "Ordering"))
    )
  ,
    ( "(<)"
    , forall1' (\t0 -> ([Trait "Ordered" t0], t0 ~> t0 ~> TIntrinsic IBool))
    )
  ,
    ( "(>)"
    , forall1' (\t0 -> ([Trait "Ordered" t0], t0 ~> t0 ~> TIntrinsic IBool))
    )
  ,
    ( "(<=)"
    , forall1' (\t0 -> ([Trait "Ordered" t0], t0 ~> t0 ~> TIntrinsic IBool))
    )
  ,
    ( "(>=)"
    , forall1' (\t0 -> ([Trait "Ordered" t0], t0 ~> t0 ~> TIntrinsic IBool))
    )
  ,
    ( "(^)"
    , forall1' (\t0 -> ([Trait "Numeric" t0], t0 ~> TIntrinsic INat ~> t0))
    )
  ,
    ( "(!=)"
    , forall1' (\t0 -> ([Trait "Comparable" t0], t0 ~> t0 ~> TIntrinsic IBool))
    )
  ,
    ( "process$_process"
    , forall2 $ \t0 t1 -> t0 ~> (t1 ~> t0 ~> t0) ~> applyTypeArgs KType (TConstructor (KArrow KType (KArrow KType KType)) "Process") (t0 :| [t1])
    )
  ,
    ( "process$_map_process"
    , forall3 $ \t0 t1 t2 -> (t0 ~> t1) ~> applyTypeArgs KType (TConstructor (KArrow KType (KArrow KType KType)) "Process") (t0 :| [t2]) ~> applyTypeArgs KType (TConstructor (KArrow KType (KArrow KType KType)) "Process") (t1 :| [t2])
    )
  ,
    ( "process$_contramap_input"
    , forall3 $ \t0 t1 t2 -> (t2 ~> t1) ~> applyTypeArgs KType (TConstructor (KArrow KType (KArrow KType KType)) "Process") (t0 :| [t1]) ~> applyTypeArgs KType (TConstructor (KArrow KType (KArrow KType KType)) "Process") (t0 :| [t2])
    )
  ,
    ( "process$_duplicate"
    , forall2 $ \t0 t1 -> applyTypeArgs KType (TConstructor (KArrow KType (KArrow KType KType)) "Process") (t0 :| [t1]) ~> applyTypeArgs KType (TConstructor (KArrow KType (KArrow KType KType)) "Process") (applyTypeArgs KType (TConstructor (KArrow KType (KArrow KType KType)) "Process") (t0 :| [t1]) :| [t1])
    )
  ]
