{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Builtin.Functions (functions) where

import Coal.Language
import Data.List.NonEmpty (NonEmpty (..))
import Extras (Name)

functions :: [(Name, IndexedScheme)]
functions =
  [
    ( "operator$__not"
    , forall0 (TIntrinsic IBool ~> TIntrinsic IBool)
    )
  ,
    ( "not"
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
    ( "always"
    , forall2 $ \t0 t1 -> t0 ~> t1 ~> t0
    )
  ,
    ( "operator$__list_concatenation"
    , forall1 $ \t0 -> listType t0 ~> listType t0 ~> listType t0
    )
  ,
    ( "trace_int32"
    , forall0 (TIntrinsic IInt32 ~> TApplication KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| []))
    )
  ,
    ( "trace_int64"
    , forall0 (TIntrinsic IInt64 ~> TApplication KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| []))
    )
  ,
    ( "trace_bignum"
    , forall0 (TIntrinsic IBignum ~> TApplication KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| []))
    )
  ,
    ( "trace_string"
    , forall0 (TIntrinsic IString ~> TApplication KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| []))
    )
  ,
    ( "trace_bool"
    , forall0 (TIntrinsic IBool ~> TApplication KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| []))
    )
  ,
    ( "trace_char"
    , forall0 (TIntrinsic IChar ~> TApplication KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| []))
    )
  ,
    ( "trace_float"
    , forall0 (TIntrinsic IFloat ~> TApplication KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| []))
    )
  ,
    ( "trace_double"
    , forall0 (TIntrinsic IDouble ~> TApplication KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| []))
    )
  ,
    ( "operator$__string_concatenation"
    , forall0 (TIntrinsic IString ~> TIntrinsic IString ~> TIntrinsic IString)
    )
  ,
    ( "int32_to_string"
    , forall0 (TIntrinsic IInt32 ~> TIntrinsic IString)
    )
  ,
    ( "float_to_string"
    , forall0 (TIntrinsic IFloat ~> TIntrinsic IString)
    )
  ,
    ( "double_to_string"
    , forall0 (TIntrinsic IDouble ~> TIntrinsic IString)
    )
  ,
    ( "unpack_nat"
    , forall0 (TIntrinsic INat ~> TIntrinsic IInt32)
    )
  ,
    ( "pack_nat"
    , forall0 (TIntrinsic IInt32 ~> TIntrinsic INat)
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
    ( "string_to_list"
    , forall0 (TIntrinsic IString ~> listType (TIntrinsic IChar))
    )
  ,
    ( "string_head"
    , forall0 (TIntrinsic IString ~> TIntrinsic IChar)
    )
  ,
    ( "string_tail"
    , forall0 (TIntrinsic IString ~> TIntrinsic IString)
    )
  ,
    ( "string_reverse"
    , forall0 (TIntrinsic IString ~> TIntrinsic IString)
    )
  ,
    ( "string_remove_whitespace"
    , forall0 (TIntrinsic IString ~> TIntrinsic IString)
    )
  ,
    ( "string_length"
    , forall0 (TIntrinsic IString ~> TIntrinsic IInt32)
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
