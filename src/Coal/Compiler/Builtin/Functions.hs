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
    ( "operator$__reverse_application"
    , forall2 $ \t0 t1 -> t0 ~> (t0 ~> t1) ~> t1
    )
  ,
    ( "operator$__list_concatenation"
    , forall1 $ \t0 -> listType t0 ~> listType t0 ~> listType t0
    )
  ,
    ( "io$_println_int32"
    , forall0 (TIntrinsic IInt32 ~> TApplication KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| []))
    )
  ,
    ( "io$_println_int64"
    , forall0 (TIntrinsic IInt64 ~> TApplication KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| []))
    )
  ,
    ( "io$_println_bignum"
    , forall0 (TIntrinsic IBignum ~> TApplication KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| []))
    )
  ,
    ( "io$_println_string"
    , forall0 (TIntrinsic IString ~> TApplication KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| []))
    )
  ,
    ( "io$_println_bool"
    , forall0 (TIntrinsic IBool ~> TApplication KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| []))
    )
  ,
    ( "io$_println_char"
    , forall0 (TIntrinsic IChar ~> TApplication KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| []))
    )
  ,
    ( "io$_println_float"
    , forall0 (TIntrinsic IFloat ~> TApplication KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| []))
    )
  ,
    ( "io$_println_double"
    , forall0 (TIntrinsic IDouble ~> TApplication KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| []))
    )
  ,
    ( "io$_print_int32"
    , forall0 (TIntrinsic IInt32 ~> TApplication KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| []))
    )
  ,
    ( "io$_print_int64"
    , forall0 (TIntrinsic IInt64 ~> TApplication KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| []))
    )
  ,
    ( "io$_print_bignum"
    , forall0 (TIntrinsic IBignum ~> TApplication KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| []))
    )
  ,
    ( "io$_print_string"
    , forall0 (TIntrinsic IString ~> TApplication KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| []))
    )
  ,
    ( "io$_print_bool"
    , forall0 (TIntrinsic IBool ~> TApplication KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| []))
    )
  ,
    ( "io$_print_char"
    , forall0 (TIntrinsic IChar ~> TApplication KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| []))
    )
  ,
    ( "io$_print_float"
    , forall0 (TIntrinsic IFloat ~> TApplication KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| []))
    )
  ,
    ( "io$_print_double"
    , forall0 (TIntrinsic IDouble ~> TApplication KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| []))
    )
  ,
    ( "io$_run"
    , forall1 $ \t0 -> TApplication KType (TConstructor (KArrow KType KType) "IO") (t0 :| []) ~> t0
    )
  ,
    ( "operator$__string_concatenation"
    , forall0 (TIntrinsic IString ~> TIntrinsic IString ~> TIntrinsic IString)
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
    ( "from_int32"
    , forall1' (\t0 -> ([Trait "Numeric" t0], TIntrinsic IInt32 ~> t0))
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
  ]
