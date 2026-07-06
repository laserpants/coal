{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Coal.Kernel.Builtin.Objects (builtinObjects, builtinInstance) where

import qualified Coal.Compiler.Builtin.Traits as Trait
import qualified Coal.Kernel.Language.Expr as NK
import Coal.Kernel.Language.Module (Module (..))
import Coal.Kernel.Language.Object (FunctionScope (..), Object (..))
import qualified Coal.Kernel.Language.Type as NKT
import qualified Coal.Kernel.Language.Type.Constructors as NKC
import Coal.Kernel.Parser.Unsafe (unsafeParseExpr)
import Coal.Language (
  Intrinsic (..),
  Parameter (..),
  Serializable,
  Trait (..),
  Type (TApplication, TConstructor, TIntrinsic, TVariable),
  instanceLabel,
 )
import qualified Data.Text as Text
import Extras (Name)
import Text.RawString.QQ (r)

builtinObjects :: Module NKT.Type
builtinObjects = objects

builtinInstance :: (Serializable t) => Trait t -> Name -> Name
builtinInstance trait name = instanceLabel trait ("Builtin$." <> name)

objects :: Module NKT.Type
objects =
  Module
    { moduleName = "Builtin$"
    , moduleImports = []
    , moduleObjects = objectList
    }

objectList :: [Object NKT.Type]
objectList =
  [ DData
      "Ordering"
      [ ("EqualTo", NKT.TCon "Ordering" [])
      , ("GreaterThan", NKT.TCon "Ordering" [])
      , ("LessThan", NKT.TCon "Ordering" [])
      ]
  , -- Machine: single constructor taking one opaque argument (the state record).
    -- Declared here so Builtin$ bodies can use the unqualified name.
    DData
      "Machine"
      [ ("Machine", NKC.arrow NKT.TOpq (NKT.TCon "Machine" [NKT.TOpq, NKT.TOpq]))
      ]
  , DFunction
      Exported
      "Builtin$.operator$__not"
      [ NK.Label NKC.bool "a"
      ]
      ( unsafeParseExpr
          [r|
                  if (a : bool) then false else true
        |]
      )
  , DFunction
      Exported
      "Builtin$.not"
      [ NK.Label NKC.bool "a"
      ]
      ( unsafeParseExpr
          [r|
                  if (a : bool) then false else true
        |]
      )
  , DFunction
      Exported
      "Builtin$.char$_ord"
      [ NK.Label NKC.char "c"
      ]
      ( unsafeParseExpr
          [r|
                  @<char>
                    ( rt_char_unbox : char/int32
                    , c : char
                    )
        |]
      )
  , DFunction
      Exported
      "Builtin$.char$_chr"
      [ NK.Label NKC.int32 "n"
      ]
      ( unsafeParseExpr
          [r|
                  @<char>
                    ( rt_char_box : int32/char
                    , n : int32
                    )
        |]
      )
  , DFunction
      Exported
      "Builtin$.number$_int32_to_float"
      [ NK.Label NKC.int32 "n"
      ]
      ( unsafeParseExpr
          [r|
                  @<float>
                    ( coal_int32_to_float : int32/float
                    , n : int32
                    )
        |]
      )
  , DFunction
      Exported
      "Builtin$.number$_unsafe_parse_bignum"
      [ NK.Label NKC.string "input"
      ]
      ( unsafeParseExpr
          [r|
                  @<bignum>
                    ( coal_bignum_init : string/bignum
                    , input : string
                    )
        |]
      )
  , DFunction
      Exported
      "Builtin$.operator$__reverse_composition"
      [ NK.Label (NKT.TOpq `NKC.arrow` NKT.TOpq) "f"
      , NK.Label (NKT.TOpq `NKC.arrow` NKT.TOpq) "g"
      , NK.Label NKT.TOpq "x"
      ]
      ( unsafeParseExpr
          [r|
                  @<*>
                    ( f : */*
                    , @<*>
                        ( g : */*
                        , x : *
                        )
                    )
        |]
      )
  , DFunction
      Exported
      "Builtin$.operator$__forward_composition"
      [ NK.Label (NKT.TOpq `NKC.arrow` NKT.TOpq) "g"
      , NK.Label (NKT.TOpq `NKC.arrow` NKT.TOpq) "f"
      , NK.Label NKT.TOpq "x"
      ]
      ( unsafeParseExpr
          [r|
                  @<*>
                    ( f : */*
                    , @<*>
                        ( g : */*
                        , x : *
                        )
                    )
        |]
      )
  , DFunction
      Exported
      "Builtin$.operator$__reverse_application"
      [ NK.Label NKT.TOpq "x"
      , NK.Label (NKT.TOpq `NKC.arrow` NKT.TOpq) "f"
      ]
      ( unsafeParseExpr
          [r|
                  @<*>
                    ( f : */*
                    , x : *
                    )
        |]
      )
  , DFunction
      Exported
      "Builtin$.operator$__forward_application"
      [ NK.Label (NKT.TOpq `NKC.arrow` NKT.TOpq) "f"
      , NK.Label NKT.TOpq "x"
      ]
      ( unsafeParseExpr
          [r|
                  @<*>
                    ( f : */*
                    , x : *
                    )
        |]
      )
  , DFunction
      Exported
      "Builtin$.always"
      [ NK.Label NKT.TOpq "a"
      , NK.Label NKT.TOpq "_"
      ]
      ( unsafeParseExpr
          [r|
                  a : *
        |]
      )
  , DFunction
      Exported
      "Builtin$.operator$__list_concatenation"
      [ NK.Label (NKT.TCon "list" [NKT.TOpq]) "xs"
      , NK.Label (NKT.TCon "list" [NKT.TOpq]) "ys"
      ]
      ( unsafeParseExpr
          [r|
                  case<list(*)>(xs : list(*)) {
                    | ( $Cons : */list(*)/list(*)
                      , z : *
                      , zs : list(*)
                      ) =>
                        @<list(*)>
                          ( $Cons : */list(*)/list(*)
                          , z : *
                          , @<list(*)>
                              ( `Builtin$.operator$__list_concatenation` : list(*)/list(*)/list(*)
                              , zs : list(*)
                              , ys : list(*)
                              )
                          )
                    | ( $Nil : list(*)
                      ) =>
                        ys : list(*)
                  }
        |]
      )
  , DFunction
      Exported
      "Builtin$.io$_print_int32"
      [ NK.Label NKC.int32 "n"
      ]
      ( unsafeParseExpr
          [r|
                  @<*>
                    ( coal_print_int32 : int32/*
                    , n : int32
                    )
        |]
      )
  , DFunction
      Exported
      "Builtin$.io$_print_int64"
      [ NK.Label NKC.int64 "n"
      ]
      ( unsafeParseExpr
          [r|
                  @<*>
                    ( coal_print_int64 : int64/*
                    , n : int64
                    )
        |]
      )
  , DFunction
      Exported
      "Builtin$.io$_print_bignum"
      [ NK.Label NKC.bignum "n"
      ]
      ( unsafeParseExpr
          [r|
                  @<*>
                    ( coal_print_bignum : bignum/*
                    , n : bignum
                    )
        |]
      )
  , DFunction
      Exported
      "Builtin$.io$_print_string"
      [ NK.Label NKC.string "s"
      ]
      ( unsafeParseExpr
          [r|
                  @<*>
                    ( coal_print_string : string/*
                    , s : string
                    )
        |]
      )
  , DFunction
      Exported
      "Builtin$.io$_print_bool"
      [ NK.Label NKC.bool "b"
      ]
      ( unsafeParseExpr
          [r|
                  @<*>
                    ( coal_print_bool : bool/*
                    , b : bool
                    )
        |]
      )
  , DFunction
      Exported
      "Builtin$.io$_print_char"
      [ NK.Label NKC.char "c"
      ]
      ( unsafeParseExpr
          [r|
                  @<*>
                    ( coal_print_char : char/*
                    , c : char
                    )
        |]
      )
  , DFunction
      Exported
      "Builtin$.io$_print_float"
      [ NK.Label NKC.float "f"
      ]
      ( unsafeParseExpr
          [r|
                  @<*>
                    ( coal_print_float : float/*
                    , f : float
                    )
        |]
      )
  , DFunction
      Exported
      "Builtin$.io$_print_double"
      [ NK.Label NKC.double "d"
      ]
      ( unsafeParseExpr
          [r|
                  @<*>
                    ( coal_print_double : double/*
                    , d : double
                    )
        |]
      )
  , DFunction
      Exported
      "Builtin$.io$_println_int32"
      [ NK.Label NKC.int32 "n"
      ]
      ( unsafeParseExpr
          [r|
                  @<*>
                    ( coal_println_int32 : int32/*
                    , n : int32
                    )
        |]
      )
  , DFunction
      Exported
      "Builtin$.io$_println_int64"
      [ NK.Label NKC.int64 "n"
      ]
      ( unsafeParseExpr
          [r|
                  @<*>
                    ( coal_println_int64 : int64/*
                    , n : int64
                    )
        |]
      )
  , DFunction
      Exported
      "Builtin$.io$_println_bignum"
      [ NK.Label NKC.bignum "n"
      ]
      ( unsafeParseExpr
          [r|
                  @<*>
                    ( coal_println_bignum : bignum/*
                    , n : bignum
                    )
        |]
      )
  , DFunction
      Exported
      "Builtin$.io$_println_string"
      [ NK.Label NKC.string "s"
      ]
      ( unsafeParseExpr
          [r|
                  @<*>
                    ( coal_println_string : string/*
                    , s : string
                    )
        |]
      )
  , DFunction
      Exported
      "Builtin$.io$_println_bool"
      [ NK.Label NKC.bool "b"
      ]
      ( unsafeParseExpr
          [r|
                  @<*>
                    ( coal_println_bool : bool/*
                    , b : bool
                    )
        |]
      )
  , DFunction
      Exported
      "Builtin$.io$_println_char"
      [ NK.Label NKC.char "c"
      ]
      ( unsafeParseExpr
          [r|
                  @<*>
                    ( coal_println_char : char/*
                    , c : char
                    )
        |]
      )
  , DFunction
      Exported
      "Builtin$.io$_println_float"
      [ NK.Label NKC.float "f"
      ]
      ( unsafeParseExpr
          [r|
                  @<*>
                    ( rt_println_float : float/*
                    , f : float
                    )
        |]
      )
  , DFunction
      Exported
      "Builtin$.io$_println_double"
      [ NK.Label NKC.double "d"
      ]
      ( unsafeParseExpr
          [r|
                  @<*>
                    ( rt_println_double : double/*
                    , d : double
                    )
        |]
      )
  , DFunction
      Exported
      "Builtin$.operator$__string_concatenation"
      [ NK.Label NKC.string "s"
      , NK.Label NKC.string "t"
      ]
      ( unsafeParseExpr
          [r|
                  @<string>
                    ( coal_string_concat : string/string/string
                    , s : string
                    , t : string
                    )
        |]
      )
  , DFunction
      Exported
      "Builtin$.string$_int32_to_string"
      [ NK.Label NKC.int32 "n"
      ]
      ( unsafeParseExpr
          [r|
                  @<string>
                    ( coal_int32_to_string : int32/string
                    , n : int32
                    )
        |]
      )
  , DFunction
      Exported
      "Builtin$.string$_float_to_string"
      [ NK.Label NKC.float "f"
      ]
      ( unsafeParseExpr
          [r|
                  @<string>
                    ( rt_float_to_string : float/string
                    , f : float
                    )
        |]
      )
  , DFunction
      Exported
      "Builtin$.string$_double_to_string"
      [ NK.Label NKC.double "d"
      ]
      ( unsafeParseExpr
          [r|
                  @<string>
                    ( coal_double_to_string : double/string
                    , d : double
                    )
        |]
      )
  , DFunction
      Exported
      "Builtin$.string$_char_to_string"
      [ NK.Label NKC.char "c"
      ]
      ( unsafeParseExpr
          [r|
                  @<string>
                    ( coal_char_to_string : char/string
                    , c : char
                    )
        |]
      )
  , DFunction
      Exported
      "Builtin$.string$_bool_to_string"
      [ NK.Label NKC.bool "b"
      ]
      ( unsafeParseExpr
          [r|
                  @<string>
                    ( coal_bool_to_string : bool/string
                    , b : bool
                    )
        |]
      )
  , DFunction
      Exported
      "Builtin$.nat$_unpack"
      [ NK.Label (NKT.TCon "nat" []) "nat"
      ]
      ( unsafeParseExpr
          [r|
                  case<int32>(nat: $Nat) {
                    | ( $Succ : int32/$Nat
                      , succ : int32
                      ) =>
                        [+ int32](succ : int32, 1)
                    | ( $Zero : $Nat
                      ) =>
                        0
                  }
        |]
      )
  , DFunction
      Exported
      "Builtin$.nat$_pack"
      [ NK.Label NKC.int32 "n"
      ]
      ( unsafeParseExpr
          [r|
                  if ([<= int32](n : int32, 0))
                    then
                      $Zero : $Nat
                    else
                      @<$Nat>
                        ( $Succ : int32/$Nat
                        , [- int32](n : int32, 1)
                        )
        |]
      )
  , DFunction
      Exported
      "Builtin$.from_int32"
      [ NK.Label (NKT.TCon "Numeric" [NKT.TOpq]) "$a"
      ]
      ( unsafeParseExpr
          [r|
                  case<int32/*>($a : Numeric(*)) {
                    | ( $Record : { from_int32 : int32/* | * }/Numeric(*)
                      , $r : { from_int32 : int32/* | * }
                      ) =>
                        get^from_int32<int32/*>($r : { from_int32 : int32/* | * })
                  }
        |]
      )
  , DFunction
      Exported
      "Builtin$.from_int64"
      [ NK.Label (NKT.TCon "Numeric" [NKT.TOpq]) "$a"
      ]
      ( unsafeParseExpr
          [r|
                  case<int64/*>($a : Numeric(*)) {
                    | ( $Record : { from_int64 : int64/* | * }/Numeric(*)
                      , $r : { from_int64 : int64/* | * }
                      ) =>
                        get^from_int64<int64/*>($r : { from_int64 : int64/* | * })
                  }
        |]
      )
  , DFunction
      Exported
      "Builtin$.from_bignum"
      [ NK.Label (NKT.TCon "Numeric" [NKT.TOpq]) "$a"
      ]
      ( unsafeParseExpr
          [r|
                  case<bignum/*>($a : Numeric(*)) {
                    | ( $Record : { from_bignum : bignum/* | * }/Numeric(*)
                      , $r : { from_bignum : bignum/* | * }
                      ) =>
                        get^from_bignum<bignum/*>($r : { from_bignum : bignum/* | * })
                  }
        |]
      )
  , DFunction
      Exported
      "Builtin$.negate"
      [ NK.Label (NKT.TCon "Numeric" [NKT.TOpq]) "$a"
      ]
      ( unsafeParseExpr
          [r|
                  case<*/*>($a : Numeric(*)) {
                    | ( $Record : { negate : */* | * }/Numeric(*)
                      , $r : { negate : */* | * }
                      ) =>
                        get^negate<*/*>($r : { negate : */* | * })
                  }
        |]
      )
  , DFunction
      Exported
      "Builtin$.(+)"
      [ NK.Label (NKT.TCon "Numeric" [NKT.TOpq]) "$a"
      ]
      ( unsafeParseExpr
          [r|
                  case<*/*/*>($a : Numeric(*)) {
                    | ( $Record : { `(+)` : */*/* | * }/Numeric(*)
                      , $r : { `(+)` : */*/* | * }
                      ) =>
                        get^`(+)`<*/*/*>($r : { `(+)` : */*/* | * })
                  }
        |]
      )
  , DFunction
      Exported
      "Builtin$.(-)"
      [ NK.Label (NKT.TCon "Numeric" [NKT.TOpq]) "$a"
      ]
      ( unsafeParseExpr
          [r|
                  case<*/*/*>($a : Numeric(*)) {
                    | ( $Record : { `(-)` : */*/* | * }/Numeric(*)
                      , $r : { `(-)` : */*/* | * }
                      ) =>
                        get^`(-)`<*/*/*>($r : { `(-)` : */*/* | * })
                  }
        |]
      )
  , DFunction
      Exported
      "Builtin$.(*)"
      [ NK.Label (NKT.TCon "Numeric" [NKT.TOpq]) "$a"
      ]
      ( unsafeParseExpr
          [r|
                  case<*/*/*>($a : Numeric(*)) {
                    | ( $Record : { `(*)` : */*/* | * }/Numeric(*)
                      , $r : { `(*)` : */*/* | * }
                      ) =>
                        get^`(*)`<*/*/*>($r : { `(*)` : */*/* | * })
                  }
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IInt32)) "from_int32")
      [ NK.Label NKC.int32 "n"
      ]
      ( unsafeParseExpr
          [r|
                  n : int32
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IInt32)) "from_int64")
      [ NK.Label NKC.int64 "n"
      ]
      ( unsafeParseExpr
          [r|
                  n : int64
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IInt32)) "from_bignum")
      [ NK.Label NKC.bignum "n"
      ]
      ( unsafeParseExpr
          [r|
                  @<int32>
                    ( coal_bignum_to_int32 : bignum/int32
                    , n : bignum
                    )
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IInt32)) "(+)")
      [ NK.Label NKC.int32 "lhs"
      , NK.Label NKC.int32 "rhs"
      ]
      ( unsafeParseExpr
          [r|
                  [+ int32](lhs : int32, rhs : int32)
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IInt32)) "(-)")
      [ NK.Label NKC.int32 "lhs"
      , NK.Label NKC.int32 "rhs"
      ]
      ( unsafeParseExpr
          [r|
                  [- int32](lhs : int32, rhs : int32)
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IInt32)) "(*)")
      [ NK.Label NKC.int32 "lhs"
      , NK.Label NKC.int32 "rhs"
      ]
      ( unsafeParseExpr
          [r|
                  [* int32](lhs : int32, rhs : int32)
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IInt32)) "negate")
      [ NK.Label NKC.int32 "n"
      ]
      ( unsafeParseExpr
          [r|
                  [- int32](0, n : int32)
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IInt64)) "from_int32")
      [ NK.Label NKC.int32 "n"
      ]
      ( unsafeParseExpr
          [r|
                  n : int32
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IInt64)) "from_int64")
      [ NK.Label NKC.int64 "n"
      ]
      ( unsafeParseExpr
          [r|
                  n : int64
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IInt64)) "from_bignum")
      [ NK.Label NKC.bignum "n"
      ]
      ( unsafeParseExpr
          [r|
                  @<int64>
                    ( coal_bignum_to_int64 : bignum/int64
                    , n : bignum
                    )
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IInt64)) "(+)")
      [ NK.Label NKC.int64 "lhs"
      , NK.Label NKC.int64 "rhs"
      ]
      ( unsafeParseExpr
          [r|
                  [+ int64](lhs : int64, rhs : int64)
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IInt64)) "(-)")
      [ NK.Label NKC.int64 "lhs"
      , NK.Label NKC.int64 "rhs"
      ]
      ( unsafeParseExpr
          [r|
                  [- int64](lhs : int64, rhs : int64)
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IInt64)) "(*)")
      [ NK.Label NKC.int64 "lhs"
      , NK.Label NKC.int64 "rhs"
      ]
      ( unsafeParseExpr
          [r|
                  [* int64](lhs : int64, rhs : int64)
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IInt64)) "negate")
      [ NK.Label NKC.int64 "n"
      ]
      ( unsafeParseExpr
          [r|
                  [- int64](0, n : int64)
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IFloat)) "from_int32")
      [ NK.Label NKC.int32 "n"
      ]
      ( unsafeParseExpr
          [r|
                  @<float>
                    ( coal_int32_to_float : int32/float
                    , n : int32 
                    )
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IFloat)) "from_int64")
      [ NK.Label NKC.int64 "n"
      ]
      ( unsafeParseExpr
          [r|
                  @<float>
                    ( coal_int64_to_float : int64/float
                    , n : int64 
                    )
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IFloat)) "from_bignum")
      [ NK.Label NKC.bignum "n"
      ]
      ( unsafeParseExpr
          [r|
                  @<float>
                    ( coal_bignum_to_float : bignum/float
                    , n : bignum 
                    )
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IFloat)) "(+)")
      [ NK.Label NKC.float "lhs"
      , NK.Label NKC.float "rhs"
      ]
      ( unsafeParseExpr
          [r|
                  [+ float](lhs : float, rhs : float)
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IFloat)) "(-)")
      [ NK.Label NKC.float "lhs"
      , NK.Label NKC.float "rhs"
      ]
      ( unsafeParseExpr
          [r|
                  [- float](lhs : float, rhs : float)
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IFloat)) "(*)")
      [ NK.Label NKC.float "lhs"
      , NK.Label NKC.float "rhs"
      ]
      ( unsafeParseExpr
          [r|
                  [* float](lhs : float, rhs : float)
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IFloat)) "negate")
      [ NK.Label NKC.float "f"
      ]
      ( unsafeParseExpr
          [r|
                  [neg float](f : float)
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IDouble)) "from_int32")
      [ NK.Label NKC.int32 "n"
      ]
      ( unsafeParseExpr
          [r|
                  @<double>
                    ( coal_int32_to_double : int32/double
                    , n : int32
                    )
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IDouble)) "from_int64")
      [ NK.Label NKC.int64 "n"
      ]
      ( unsafeParseExpr
          [r|
                  @<double>
                    ( coal_int64_to_double : int64/double
                    , n : int64
                    )
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IDouble)) "from_bignum")
      [ NK.Label NKC.bignum "n"
      ]
      ( unsafeParseExpr
          [r|
                  @<double>
                    ( coal_bignum_to_double : bignum/double
                    , n : bignum
                    )
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IDouble)) "(+)")
      [ NK.Label NKC.double "lhs"
      , NK.Label NKC.double "rhs"
      ]
      ( unsafeParseExpr
          [r|
                  [+ double](lhs : double, rhs : double)
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IDouble)) "(-)")
      [ NK.Label NKC.double "lhs"
      , NK.Label NKC.double "rhs"
      ]
      ( unsafeParseExpr
          [r|
                  [- double](lhs : double, rhs : double)
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IDouble)) "(*)")
      [ NK.Label NKC.double "lhs"
      , NK.Label NKC.double "rhs"
      ]
      ( unsafeParseExpr
          [r|
                  [* double](lhs : double, rhs : double)
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IDouble)) "negate")
      [ NK.Label NKC.double "d"
      ]
      ( unsafeParseExpr
          [r|
                  [neg double](d : double)
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic INat)) "from_int32")
      [ NK.Label NKC.int32 "m"
      ]
      ( unsafeParseExpr
          [r|
                  @<$Nat>
                    ( `Builtin$.nat$_pack` : int32/$Nat
                    , m : int32
                    )
        |]
      )
  , DFunction
      -- TODO: fix
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic INat)) "from_int64")
      [ NK.Label NKC.int64 "m"
      ]
      ( unsafeParseExpr
          [r|
                  @<$Nat>
                    ( `Builtin$.nat$_pack` : int32/$Nat
                    , m : int64
                    )
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic INat)) "from_bignum")
      [ NK.Label NKC.bignum "n"
      ]
      ( unsafeParseExpr
          [r|
                  @<$Nat>
                    ( `Builtin$.nat$_pack` : int32/$Nat
                    , @<int32>
                        ( coal_bignum_to_int32 : bignum/int32
                        , n : bignum
                        )
                    )
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic INat)) "(+)")
      [ NK.Label (NKT.TCon "nat" []) "lhs"
      , NK.Label (NKT.TCon "nat" []) "rhs"
      ]
      ( unsafeParseExpr
          [r|
                  @<$Nat>
                    ( `Builtin$.nat$_pack` : int32/$Nat
                    , [+ int32]
                        ( @<int32>
                            ( `Builtin$.nat$_unpack` : $Nat/int32
                            , lhs : $Nat
                            )
                        , @<int32>
                            ( `Builtin$.nat$_unpack` : $Nat/int32
                            , rhs : $Nat
                            )
                        )
                    )
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic INat)) "(-)")
      [ NK.Label (NKT.TCon "nat" []) "lhs"
      , NK.Label (NKT.TCon "nat" []) "rhs"
      ]
      ( unsafeParseExpr
          [r|
                  @<$Nat>
                    ( `Builtin$.nat$_pack` : int32/$Nat
                    , let
                        n : int32 =
                          [- int32]
                            ( @<int32>
                                ( `Builtin$.nat$_unpack` : $Nat/int32
                                , lhs : $Nat
                                )
                            , @<int32>
                                ( `Builtin$.nat$_unpack` : $Nat/int32
                                , rhs : $Nat
                                )
                            )
                        in
                          if ([< int32] (n : int32, 0)) 
                            then 0
                            else n : int32
                    )
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic INat)) "(*)")
      [ NK.Label (NKT.TCon "nat" []) "lhs"
      , NK.Label (NKT.TCon "nat" []) "rhs"
      ]
      ( unsafeParseExpr
          [r|
                  @<$Nat>
                    ( `Builtin$.nat$_pack` : int32/$Nat
                    , [* int32]
                        ( @<int32>
                            ( `Builtin$.nat$_unpack` : $Nat/int32
                            , lhs : $Nat
                            )
                        , @<int32>
                            ( `Builtin$.nat$_unpack` : $Nat/int32
                            , rhs : $Nat
                            )
                        )
                    )
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic INat)) "negate")
      [ NK.Label (NKT.TCon "nat" []) "_"
      ]
      ( unsafeParseExpr
          [r|
                  $Zero : $Nat
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IBignum)) "from_int32")
      [ NK.Label NKC.int32 "n"
      ]
      ( unsafeParseExpr
          [r|
                  @<bignum>
                    ( coal_int32_to_bignum : int32/bignum
                    , n : int32
                    )
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IBignum)) "from_int64")
      [ NK.Label NKC.int64 "n"
      ]
      ( unsafeParseExpr
          [r|
                  @<bignum>
                    ( coal_int64_to_bignum : int64/bignum
                    , n : int64
                    )
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IBignum)) "from_bignum")
      [ NK.Label NKC.bignum "n"
      ]
      ( unsafeParseExpr
          [r|
                  n : bignum
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IBignum)) "(+)")
      [ NK.Label NKC.bignum "p"
      , NK.Label NKC.bignum "q"
      ]
      ( unsafeParseExpr
          [r|
                  @<bignum>
                    ( coal_bignum_add : bignum/bignum/bignum
                    , p : bignum
                    , q : bignum
                    )
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IBignum)) "(-)")
      [ NK.Label NKC.bignum "p"
      , NK.Label NKC.bignum "q"
      ]
      ( unsafeParseExpr
          [r|
                  @<bignum>
                    ( coal_bignum_sub : bignum/bignum/bignum
                    , p : bignum
                    , q : bignum
                    )
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IBignum)) "(*)")
      [ NK.Label NKC.bignum "p"
      , NK.Label NKC.bignum "q"
      ]
      ( unsafeParseExpr
          [r|
                  @<bignum>
                    ( coal_bignum_mul : bignum/bignum/bignum
                    , p : bignum
                    , q : bignum
                    )
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IBignum)) "negate")
      [ NK.Label NKC.bignum "p"
      ]
      ( unsafeParseExpr
          [r|
                  @<bignum>
                    ( coal_bignum_neg : bignum/bignum
                    , p : bignum
                    )
        |]
      )
  , DFunction
      Exported
      "Builtin$.compare"
      [ NK.Label (NKT.TCon "Ordered" [NKT.TOpq]) "$a"
      ]
      ( unsafeParseExpr
          [r|
                  case<*/*/Ordering>($a : Ordered(*)) {
                    | ( $Record : { compare : */*/Ordering | * }/Ordered(*)
                      , $r : { compare : */*/Ordering | * }
                      ) =>
                        get^compare<*/*/Ordering>($r : { compare : */*/Ordering | * })
                  }
        |]
      )
  , DFunction
      Exported
      "Builtin$.(^)"
      [ NK.Label (NKT.TCon "Numeric" [NKT.TOpq]) "$a"
      , NK.Label NKT.TOpq "m"
      , NK.Label (NKT.TCon "nat" []) "n"
      ]
      ( unsafeParseExpr
          [r|
                  case<*>($a : Numeric(*)) {
                    | ( $Record : { `(*)` : */*/* | from_int32 : int32/* | * }/Numeric(*)
                      , $r : { `(*)` : */*/* | from_int32 : int32/* | * }
                      ) =>
                        let
                          $f : */*/* =
                            get^`(*)`<*/*/*>($r : { `(*)` : */*/* | * })
                          in
                            let
                              $g : int32/* =
                                get^from_int32<int32/*>($r : { from_int32 : int32/* | * })
                            in
                              let 
                                one : * =
                                  @<*>
                                    ( $g : int32/*
                                    , 1 
                                    )
                                in
                                  let
                                    z : int32 =
                                      @<int32>
                                        ( `Builtin$.nat$_unpack` : $Nat/int32
                                        , n : $Nat 
                                        )
                                    in
                                      let
                                        h : int32/*/* =
                                          fn(q : int32, r : *) =>
                                            if ([== int32](q : int32, 0))
                                              then
                                                r : *
                                              else
                                                @<*>
                                                  ( h : int32/*/*
                                                  , [- int32](q : int32, 1)
                                                  , @<*>
                                                      ( $f : */*/*
                                                      , m : *
                                                      , r : *
                                                      )
                                                  )
                                        in
                                          @<*>
                                            ( h : int32/*/*
                                            , z : int32
                                            , one : *
                                            )
                }
        |]
      )
  , DFunction
      Exported
      "Builtin$.(<)"
      [ NK.Label (NKT.TCon "Ordered" [NKT.TOpq]) "$a"
      , NK.Label NKT.TOpq "x"
      , NK.Label NKT.TOpq "y"
      ]
      ( unsafeParseExpr
          [r|
                  case<bool>($a : Ordered(*)) {
                    | ( $Record : { compare : */*/Ordering | * }/Ordered(*)
                      , $r : { compare : */*/Ordering | * }
                      ) =>
                        let
                          $f : */*/Ordering =
                            get^compare<*/*/Ordering>($r : { compare : */*/Ordering | * })
                          in
                            case<bool>(@<Ordering>($f : */*/Ordering, x : *, y : *)) {
                              | ( EqualTo : Ordering ) => false
                              | ( GreaterThan : Ordering ) => false
                              | ( LessThan : Ordering ) => true
                            }
                  }
        |]
      )
  , DFunction
      Exported
      "Builtin$.(<=)"
      [ NK.Label (NKT.TCon "Ordered" [NKT.TOpq]) "$a"
      , NK.Label NKT.TOpq "x"
      , NK.Label NKT.TOpq "y"
      ]
      ( unsafeParseExpr
          [r|
                  case<bool>($a : Ordered(*)) {
                    | ( $Record : { compare : */*/Ordering | * }/Ordered(*)
                      , $r : { compare : */*/Ordering | * }
                      ) =>
                        let
                          $f : */*/Ordering =
                            get^compare<*/*/Ordering>($r : { compare : */*/Ordering | * })
                          in
                            case<bool>(@<Ordering>($f : */*/Ordering, x : *, y : *)) {
                              | ( EqualTo : Ordering ) => true
                              | ( GreaterThan : Ordering ) => false
                              | ( LessThan : Ordering ) => true
                            }
                  }
        |]
      )
  , DFunction
      Exported
      "Builtin$.(>)"
      [ NK.Label (NKT.TCon "Ordered" [NKT.TOpq]) "$a"
      , NK.Label NKT.TOpq "x"
      , NK.Label NKT.TOpq "y"
      ]
      ( unsafeParseExpr
          [r|
                  case<bool>($a : Ordered(*)) {
                    | ( $Record : { compare : */*/Ordering | * }/Ordered(*)
                      , $r : { compare : */*/Ordering | * }
                      ) =>
                        let
                          $f : */*/Ordering =
                            get^compare<*/*/Ordering>($r : { compare : */*/Ordering | * })
                          in
                            case<bool>(@<Ordering>($f : */*/Ordering, x : *, y : *)) {
                              | ( EqualTo : Ordering ) => false
                              | ( GreaterThan : Ordering ) => true
                              | ( LessThan : Ordering ) => false
                            }
                  }
        |]
      )
  , DFunction
      Exported
      "Builtin$.(>=)"
      [ NK.Label (NKT.TCon "Ordered" [NKT.TOpq]) "$a"
      , NK.Label NKT.TOpq "x"
      , NK.Label NKT.TOpq "y"
      ]
      ( unsafeParseExpr
          [r|
                  case<bool>($a : Ordered(*)) {
                    | ( $Record : { compare : */*/Ordering | * }/Ordered(*)
                      , $r : { compare : */*/Ordering | * }
                      ) =>
                        let
                          $f : */*/Ordering =
                            get^compare<*/*/Ordering>($r : { compare : */*/Ordering | * })
                          in
                            case<bool>(@<Ordering>($f : */*/Ordering, x : *, y : *)) {
                              | ( EqualTo : Ordering ) => true
                              | ( GreaterThan : Ordering ) => true
                              | ( LessThan : Ordering ) => false
                            }
                  }
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.ordered (TIntrinsic IInt32)) "compare")
      [ NK.Label NKC.int32 "x"
      , NK.Label NKC.int32 "y"
      ]
      ( unsafeParseExpr
          [r|
                  if ([< int32](x : int32, y : int32))
                    then
                      LessThan : Ordering
                    else
                      if ([> int32](x : int32, y : int32))
                        then
                          GreaterThan : Ordering
                        else
                          EqualTo : Ordering
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.ordered (TIntrinsic IInt64)) "compare")
      [ NK.Label NKC.int64 "x"
      , NK.Label NKC.int64 "y"
      ]
      ( unsafeParseExpr
          [r|
                  if ([< int64](x : int64, y : int64))
                    then
                      LessThan : Ordering
                    else
                      if ([> int64](x : int64, y : int64))
                        then
                          GreaterThan : Ordering
                        else
                          EqualTo : Ordering
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.ordered (TIntrinsic IFloat)) "compare")
      [ NK.Label NKC.float "x"
      , NK.Label NKC.float "y"
      ]
      ( unsafeParseExpr
          [r|
                  if ([< float](x : float, y : float))
                    then
                      LessThan : Ordering
                    else
                      if ([> float](x : float, y : float))
                        then
                          GreaterThan : Ordering
                        else
                          EqualTo : Ordering
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.ordered (TIntrinsic IDouble)) "compare")
      [ NK.Label NKC.double "x"
      , NK.Label NKC.double "y"
      ]
      ( unsafeParseExpr
          [r|
                  if ([< double](x : double, y : double))
                    then
                      LessThan : Ordering
                    else
                      if ([> double](x : double, y : double))
                        then
                          GreaterThan : Ordering
                        else
                          EqualTo : Ordering
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.ordered (TIntrinsic INat)) "compare")
      [ NK.Label (NKT.TCon "nat" []) "x"
      , NK.Label (NKT.TCon "nat" []) "y"
      ]
      ( unsafeParseExpr
          ( [r|
                  let 
                    a : int32 = 
                      @<int32>
                        ( `Builtin$.nat$_unpack` : $Nat/int32
                        , x : $Nat )
                      in
                        let
                          b : int32 =
                            @<int32>
                              ( `Builtin$.nat$_unpack` : $Nat/int32
                              , y : $Nat )
                          in
                            @<Ordering>
                              ( `|]
              <> Text.unpack (builtinInstance (Trait.ordered (TIntrinsic IInt32)) "compare")
              <> [r|` : int32/int32/Ordering
                              , a : int32
                              , b : int32 
                              )
        |]
          )
      )
  , DFunction
      Exported
      (builtinInstance (Trait.ordered (TIntrinsic IBool)) "compare")
      [ NK.Label NKC.bool "x"
      , NK.Label NKC.bool "y"
      ]
      ( unsafeParseExpr
          [r|
                  if 
                    ([&&]
                      ( @<bool>(`Builtin$.not` : bool/bool, x : bool )
                      , y : bool )
                    )
                    then LessThan : Ordering
                    else
                     if 
                       ([&&]
                         ( x : bool
                         , @<bool>(`Builtin$.not` : bool/bool, y : bool )
                         )
                       )
                       then GreaterThan : Ordering
                       else EqualTo : Ordering
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.ordered (TIntrinsic IChar)) "compare")
      [ NK.Label NKC.char "x"
      , NK.Label NKC.char "y"
      ]
      ( unsafeParseExpr
          ( [r|
                  @<Ordering>
                    ( `|]
              <> Text.unpack (builtinInstance (Trait.ordered (TIntrinsic IInt32)) "compare")
              <> [r|` : int32/int32/Ordering
                    , @<int32>(`Builtin$.char$_ord` : char/int32, x : char) 
                    , @<int32>(`Builtin$.char$_ord` : char/int32, y : char)
                    )
        |]
          )
      )
  , DFunction
      Exported
      (builtinInstance (Trait.ordered (TIntrinsic IString)) "compare")
      [ NK.Label NKC.string "s1"
      , NK.Label NKC.string "s2"
      ]
      ( unsafeParseExpr
          ( [r|
                  let
                    fst : list(char) = 
                      @<list(char)>
                        ( `Builtin$.string$_to_list` : string/list(char)
                        , s1 : string )
                  in let
                    snd : list(char) = 
                      @<list(char)>
                        ( `Builtin$.string$_to_list` : string/list(char)
                        , s2 : string )
                  in let
                    f : list(char)/list(char)/Ordering =
                      fn(xs : list(char), ys : list(char)) =>
                        case<Ordering>(xs : list(char)) {
                          | ( $Nil : list(char)
                            ) =>
                              case<Ordering>(ys : list(char)) {
                                | ( $Nil : list(char)
                                  ) =>
                                    EqualTo : Ordering
                                | ( $Cons : char/list(char)/list(char)
                                  , _ : char
                                  , _ : list(char)
                                  ) =>
                                    LessThan : Ordering
                              }
                          | ( $Cons : char/list(char)/list(char)
                            , x : char
                            , xs1 : list(char)
                            ) =>
                              case<Ordering>(ys : list(char)) {
                                | ( $Nil : list(char)
                                  ) =>
                                    GreaterThan : Ordering
                                | ( $Cons : char/list(char)/list(char)
                                  , y : char
                                  , ys1 : list(char)
                                  ) =>
                                    case<Ordering>(
                                      @<Ordering>
                                        ( `|]
              <> Text.unpack (builtinInstance (Trait.ordered (TIntrinsic IChar)) "compare")
              <> [r|` : char/char/Ordering
                                        , x : char
                                        , y : char
                                        )
                                    ) {
                                      | (LessThan : Ordering) => 
                                          LessThan : Ordering
                                      | (GreaterThan : Ordering) => 
                                          GreaterThan : Ordering 
                                      | (EqualTo : Ordering) => 
                                          @<Ordering>
                                            ( f : list(char)/list(char)/Ordering
                                            , xs1 : list(char)
                                            , ys1 : list(char) 
                                            )
                                    }
                              }
                        }
                  in 
                    @<Ordering>
                      ( f : list(char)/list(char)/Ordering
                      , fst : list(char)
                      , snd : list(char)
                      )
        |]
          )
      )
  , DFunction
      Exported
      (builtinInstance (Trait.ordered (TIntrinsic IBignum)) "compare")
      [ NK.Label NKC.bignum "x"
      , NK.Label NKC.bignum "y"
      ]
      ( unsafeParseExpr
          [r|
                  let
                    is_lt : bool =
                      @<bool>
                        ( coal_bignum_lt : bignum/bignum/bool
                        , x : bignum
                        , y : bignum
                        )
                    in
                      if (is_lt : bool)
                        then LessThan : Ordering
                        else 
                          let
                            is_gt : bool =
                              @<bool>
                                ( coal_bignum_gt : bignum/bignum/bool
                                , x : bignum
                                , y : bignum
                                )
                            in
                              if (is_gt : bool)
                                then GreaterThan : Ordering
                                else EqualTo : Ordering
        |]
      )
  , DFunction
      Exported
      "Builtin$.string$_length"
      [ NK.Label NKC.string "str"
      ]
      ( unsafeParseExpr
          [r|
                  @<int32>
                    ( coal_string_length : string/int32
                    , str : string
                    )
        |]
      )
  , DFunction
      Exported
      "Builtin$.string$_head_unsafe"
      [ NK.Label NKC.string "str"
      ]
      ( unsafeParseExpr
          [r|
                  @<char>
                    ( coal_string_head : string/char
                    , str : string
                    )
        |]
      )
  , DFunction
      Exported
      "Builtin$.string$_tail"
      [ NK.Label NKC.string "str"
      ]
      ( unsafeParseExpr
          [r|
                  @<string>
                    ( coal_string_tail : string/string
                    , str : string
                    )
        |]
      )
  , DFunction
      Exported
      "Builtin$.string$_reverse"
      [ NK.Label NKC.string "str"
      ]
      ( unsafeParseExpr
          [r|
                  @<string>
                    ( coal_string_reverse : string/string
                    , str : string
                    )
        |]
      )
  , DFunction
      Exported
      "Builtin$.string$_remove_whitespace"
      [ NK.Label NKC.string "str"
      ]
      ( unsafeParseExpr
          [r|
                  @<string>
                    ( coal_string_remove_whitespace : string/string
                    , str : string
                    )
        |]
      )
  , DFunction
      Exported
      "Builtin$.string$_from_list"
      [ NK.Label (NKT.TCon "list" [NKC.char]) "chars"
      ]
      ( unsafeParseExpr
          [r|
                  let
                    f : list(char)/string/string =
                      fn(chars : list(char), result : string) =>
                        case<string>(chars : list(char)) {
                          | ( $Nil : list(char)
                            ) =>  
                              result : string
                          | ( $Cons : char/list(char)/list(char)
                            , c : char
                            , cs : list(char)
                            ) => 
                              @<string>
                                ( f : list(char)/string/string
                                , cs : list(char)
                                , @<string>
                                    ( `Builtin$.operator$__string_concatenation` : string/string/string
                                    , result : string 
                                    , @<string>
                                        ( `Builtin$.string$_char_to_string` : char/string
                                        , c : char
                                        )
                                    )
                                )
                        }
                  in
                    @<string>
                      ( f : list(char)/string/string
                      , chars : list(char)
                      , "" 
                      )
        |]
      )
  , DFunction
      Exported
      "Builtin$.string$_to_list"
      [ NK.Label NKC.string "str"
      ]
      ( unsafeParseExpr
          [r|
                  let
                    f : string/list(char)/list(char) =
                      fn(input : string, result : list(char)) => 
                        if ( [== int32]
                               ( @<int32>
                                   ( `Builtin$.string$_length` : string/int32 
                                   , input : string
                                   )
                               , 0 
                               ) )
                          then
                            result : list(char)
                          else 
                            @<list(char)>
                              ( f : string/list(char)/list(char)
                              , @<string>
                                  ( `Builtin$.string$_tail` : string/string
                                  , input : string
                                  )
                              , @<list(char)>
                                  ( $Cons : char/list(char)/list(char)
                                  , @<char>
                                      ( `Builtin$.string$_head_unsafe` : string/char
                                      , input : string
                                      )
                                  , result : list(char)
                                  )
                              )
                    in
                      @<list(char)>
                        ( f : string/list(char)/list(char)
                        , @<string>
                            ( `Builtin$.string$_reverse` : string/string 
                            , str : string
                            )
                        , $Nil : list(char)
                        )
        |]
      )
  , DFunction
      Exported
      "Builtin$.(==)"
      [ NK.Label (NKT.TCon "Comparable" [NKT.TOpq]) "$a"
      ]
      ( unsafeParseExpr
          [r|
                  case<*/*/bool>($a : Comparable(*)) {
                    | ( $Record : { `(==)` : */*/bool | * }/Comparable(*)
                      , $r : { `(==)` : */*/bool | * }
                      ) =>
                        get^`(==)`<*/*/bool>($r : { `(==)` : */*/bool | * })
                  }
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.comparable (TIntrinsic IInt32)) "(==)")
      [ NK.Label NKC.int32 "x"
      , NK.Label NKC.int32 "y"
      ]
      ( unsafeParseExpr
          [r|
                  if ([== int32](x : int32, y : int32)) then true else false 
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.comparable (TIntrinsic IInt64)) "(==)")
      [ NK.Label NKC.int64 "x"
      , NK.Label NKC.int64 "y"
      ]
      ( unsafeParseExpr
          [r|
                  if ([== int64](x : int64, y : int64)) then true else false 
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.comparable (TIntrinsic IFloat)) "(==)")
      [ NK.Label NKC.float "x"
      , NK.Label NKC.float "y"
      ]
      ( unsafeParseExpr
          [r|
                  if ([== float](x : float, y : float)) then true else false 
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.comparable (TIntrinsic IDouble)) "(==)")
      [ NK.Label NKC.double "x"
      , NK.Label NKC.double "y"
      ]
      ( unsafeParseExpr
          [r|
                  if ([== double](x : double, y : double)) then true else false 
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.comparable (TIntrinsic IBool)) "(==)")
      [ NK.Label NKC.bool "x"
      , NK.Label NKC.bool "y"
      ]
      ( unsafeParseExpr
          [r|
                  if ([== bool](x : bool, y : bool)) then true else false 
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.comparable (TIntrinsic IChar)) "(==)")
      [ NK.Label NKC.char "x"
      , NK.Label NKC.char "y"
      ]
      ( unsafeParseExpr
          [r|
                  if ([== char](x : char, y : char)) then true else false 
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.comparable (TIntrinsic INat)) "(==)")
      [ NK.Label (NKT.TCon "nat" []) "x"
      , NK.Label (NKT.TCon "nat" []) "y"
      ]
      ( unsafeParseExpr
          [r|
                  let 
                    a : int32 = 
                      @<int32>
                        ( `Builtin$.nat$_unpack` : $Nat/int32
                        , x : $Nat 
                        )
                      in
                        let
                          b : int32 =
                            @<int32>
                              ( `Builtin$.nat$_unpack` : $Nat/int32
                              , y : $Nat 
                              )
                          in
                            if ([== int32](a : int32, b : int32)) 
                              then true 
                              else false 
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.comparable (TIntrinsic IString)) "(==)")
      [ NK.Label NKC.string "str1"
      , NK.Label NKC.string "str2"
      ]
      ( unsafeParseExpr
          [r|
                  @<bool>
                    ( coal_string_compare : string/string/bool
                    , str1 : string
                    , str2 : string
                    )
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.comparable (TIntrinsic IBignum)) "(==)")
      [ NK.Label NKC.bignum "m"
      , NK.Label NKC.bignum "n"
      ]
      ( unsafeParseExpr
          [r|
                  @<bool>
                    ( coal_bignum_eq : bignum/bignum/bool
                    , m : bignum
                    , n : bignum
                    )
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.divisible (TIntrinsic IFloat)) "(/)")
      [ NK.Label NKC.float "q"
      , NK.Label NKC.float "r"
      ]
      ( unsafeParseExpr
          [r|
                  [/ float](q : float, r : float)
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.divisible (TIntrinsic IDouble)) "(/)")
      [ NK.Label NKC.double "q"
      , NK.Label NKC.double "r"
      ]
      ( unsafeParseExpr
          [r|
                  [/ double](q : double, r : double)
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.modulo (TIntrinsic IInt32)) "(%)")
      [ NK.Label NKC.int32 "q"
      , NK.Label NKC.int32 "r"
      ]
      ( unsafeParseExpr
          [r|
                  @<int32>
                    ( coal_int32_mod : int32/int32/int32
                    , q : int32
                    , r : int32
                    )
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.modulo (TIntrinsic IInt64)) "(%)")
      [ NK.Label NKC.int64 "q"
      , NK.Label NKC.int64 "r"
      ]
      ( unsafeParseExpr
          [r|
                  @<int64>
                    ( coal_int64_mod : int64/int64/int64
                    , q : int64
                    , r : int64
                    )
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.modulo (TIntrinsic IBignum)) "(%)")
      [ NK.Label NKC.bignum "q"
      , NK.Label NKC.bignum "r"
      ]
      ( unsafeParseExpr
          [r|
                  @<bignum>
                    ( coal_bignum_mod : bignum/bignum/bignum
                    , q : bignum
                    , r : bignum
                    )
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.semigroup (TIntrinsic IString)) "(<>)")
      [ NK.Label NKC.string "s"
      , NK.Label NKC.string "t"
      ]
      ( unsafeParseExpr
          [r|
                  @<string>
                    ( `Builtin$.operator$__string_concatenation` : string/string/string
                    , s : string
                    , t : string
                    )
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.semigroup (TApplication () (TConstructor () "List") (TVariable (Parameter () "a")))) "(<>)")
      [ NK.Label (NKT.TCon "list" [NKT.TOpq]) "xs"
      , NK.Label (NKT.TCon "list" [NKT.TOpq]) "ys"
      ]
      ( unsafeParseExpr
          [r|
                  @<list(*)>
                    ( `Builtin$.operator$__list_concatenation` : list(*)/list(*)/list(*)
                    , xs : list(*)
                    , ys : list(*)
                    )
        |]
      )
  , DFunction
      Exported
      "Builtin$.io$_eval"
      [ NK.Label NKT.TOpq "v"
      ]
      ( unsafeParseExpr
          [r|
                  v : *
        |]
      )
  , DFunction
      Exported
      "Builtin$.io$_return"
      [ NK.Label NKT.TOpq "v"
      ]
      ( unsafeParseExpr
          [r|
                  v : IO(*)
        |]
      )
  , DFunction
      Exported
      "Builtin$.(!=)"
      [ NK.Label (NKT.TCon "Comparable" [NKT.TOpq]) "$c"
      , NK.Label NKT.TOpq "a"
      , NK.Label NKT.TOpq "b"
      ]
      ( unsafeParseExpr
          [r|
                  @<bool>
                    ( `Builtin$.operator$__not` : bool/bool
                    , @<bool>
                        ( `Builtin$.(==)` : Comparable(*)/*/*/bool
                        , $c : Comparable(*)
                        , a : *
                        , b : *
                        )
                    )
        |]
      )
  , DFunction
      Exported
      ( builtinInstance (Trait.comparable (TApplication () (TApplication () (TConstructor () "#Tuple2") (TVariable (Parameter () "a"))) (TVariable (Parameter () "b")))) "(==)"
      )
      [ NK.Label (NKT.TCon "Comparable" [NKT.TOpq]) "$a"
      , NK.Label (NKT.TCon "Comparable" [NKT.TOpq]) "$b"
      , NK.Label (NKT.TCon "tuple" [NKT.TOpq, NKT.TOpq]) "t1"
      , NK.Label (NKT.TCon "tuple" [NKT.TOpq, NKT.TOpq]) "t2"
      ]
      ( unsafeParseExpr
          [r|
                  case<bool>(t1 : tuple2(*,*)) {
                    | ( $Tuple2 : */*/tuple2(*,*)
                      , a1 : *
                      , b1 : *
                      ) =>
                        case<bool>(t2 : tuple2(*,*)) {
                          | ( $Tuple2 : */*/tuple2(*,*)
                            , a2 : *
                            , b2 : *
                            ) =>
                              ([&&]
                                ( @<bool>
                                    ( `Builtin$.(==)` : Comparable(*)/*/*/bool
                                    , $a : Comparable(*)
                                    , a1 : *
                                    , a2 : * 
                                    )
                                , @<bool>
                                    ( `Builtin$.(==)` : Comparable(*)/*/*/bool
                                    , $b : Comparable(*)
                                    , b1 : *
                                    , b2 : * 
                                    )
                                )
                              )
                        }
                  }
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.ordered (TApplication () (TApplication () (TConstructor () "#Tuple2") (TVariable (Parameter () "a"))) (TVariable (Parameter () "b")))) "compare")
      [ NK.Label (NKT.TCon "Ordered" [NKT.TOpq]) "$a"
      , NK.Label (NKT.TCon "Ordered" [NKT.TOpq]) "$b"
      , NK.Label (NKT.TCon "tuple" [NKT.TOpq, NKT.TOpq]) "t1"
      , NK.Label (NKT.TCon "tuple" [NKT.TOpq, NKT.TOpq]) "t2"
      ]
      ( unsafeParseExpr
          [r|
                  case<Ordering>(t1 : tuple2(*,*)) {
                    | ( $Tuple2 : */*/tuple2(*,*)
                      , a1 : *
                      , b1 : *
                      ) =>
                        case<Ordering>(t2 : tuple2(*,*)) {
                          | ( $Tuple2 : */*/tuple2(*,*)
                            , a2 : *
                            , b2 : *
                            ) =>
                              case<Ordering>(
                                @<Ordering>
                                  ( Builtin$.compare : Ordered(*)/*/*/Ordering
                                  , $a : Ordered(*)
                                  , a1 : *
                                  , a2 : *
                                  )
                              ) {
                                | ( LessThan : Ordering ) => LessThan : Ordering
                                | ( GreaterThan : Ordering ) => GreaterThan : Ordering
                                | ( EqualTo : Ordering ) =>
                                    @<Ordering>
                                      ( Builtin$.compare : Ordered(*)/*/*/Ordering
                                      , $b : Ordered(*)
                                      , b1 : *
                                      , b2 : *
                                      )
                              }
                        }
                  }
        |]
      )
  , DFunction
      Exported
      "Builtin$.machine$_machine"
      [ NK.Label NKT.TOpq "seed"
      , NK.Label (NKT.TOpq `NKC.arrow` NKT.TOpq `NKC.arrow` NKT.TOpq) "transition"
      , NK.Label (NKT.TOpq `NKC.arrow` NKT.TOpq) "view"
      ]
      ( unsafeParseExpr
          [r|
                  let
                    step : */*/Machine(*,*) =
                      fn(input : *, current_state : *) =>
                        @<Machine(*,*)>
                          ( Machine : record({ state : * | step : */*/Machine(*,*) | view : */* | {} })/Machine(*,*)
                          , @<record({ state : * | step : */*/Machine(*,*) | view : */* | {} })>
                              ( $Record : { state : * | step : */*/Machine(*,*) | view : */* | {} }/record({ state : * | step : */*/Machine(*,*) | view : */* | {} })
                              , { state =
                                    @<*>
                                      ( transition : */*/*
                                      , input : *
                                      , current_state : *
                                      )
                                | step = step : */*/Machine(*,*)
                                | view = view : */*
                                | {}
                                }
                              )
                          )
                  in
                  @<Machine(*,*)>
                    ( Machine : record({ state : * | step : */*/Machine(*,*) | view : */* | {} })/Machine(*,*)
                    , @<record({ state : * | step : */*/Machine(*,*) | view : */* | {} })>
                        ( $Record : { state : * | step : */*/Machine(*,*) | view : */* | {} }/record({ state : * | step : */*/Machine(*,*) | view : */* | {} })
                        , { state = seed : *
                          | step = step : */*/Machine(*,*)
                          | view = view : */*
                          | {}
                          }
                        )
                    )
        |]
      )
  , DFunction
      Exported
      "Builtin$.machine$_map_machine"
      [ NK.Label (NKT.TOpq `NKC.arrow` NKT.TOpq) "f"
      , NK.Label (NKT.TCon "Machine" [NKT.TOpq, NKT.TOpq]) "m"
      ]
      ( unsafeParseExpr
          [r|
                  case<Machine(*,*)>(m : Machine(*,*)) {
                    | ( Machine : record({ state : * | step : */*/Machine(*,*) | view : */* | {} })/Machine(*,*)
                      , $r : record({ state : * | step : */*/Machine(*,*) | view : */* | {} })
                      ) =>
                        case<Machine(*,*)>($r : record({ state : * | step : */*/Machine(*,*) | view : */* | {} })) {
                          | ( $Record : { state : * | step : */*/Machine(*,*) | view : */* | {} }/record({ state : * | step : */*/Machine(*,*) | view : */* | {} })
                            , $row : { state : * | step : */*/Machine(*,*) | view : */* | {} }
                            ) =>
                              let 
                                $state : * =
                                  get^state<*>($row : { state : * | * })
                                in
                                  let
                                    $step : */*/Machine(*,*) =
                                      get^step<*/*/Machine(*,*)>($row : { step : */*/Machine(*,*) | * })
                                    in
                                      let
                                        $view : */* =
                                          get^view<*/*>($row : { view : */* | * })
                                        in
                                          @<Machine(*,*)>
                                            ( Machine : record({ state : * | step : */*/Machine(*,*) | view : */* | {} })/Machine(*,*)
                                            , @<record({ state : * | step : */*/Machine(*,*) | view : */* | {} })>
                                                ( $Record : { state : * | step : */*/Machine(*,*) | view : */* | {} }/record({ state : * | step : */*/Machine(*,*) | view : */* | {} })
                                                , { state = $state : *
                                                  | step = 
                                                      fn(inp : *, s : *) =>
                                                        @<Machine(*,*)>
                                                          ( `Builtin$.machine$_map_machine` : (*/*)/Machine(*,*)/Machine(*,*)
                                                          , f : */*
                                                          , @<Machine(*,*)>
                                                              ( $step : */*/Machine(*,*)
                                                              , inp : *
                                                              , s : *
                                                              )
                                                          )
                                                  | view = 
                                                      fn(s : *) =>
                                                        @<*>
                                                          ( f : */*
                                                          , @<*>
                                                              ( $view : */*
                                                              , s : *
                                                              )
                                                          )
                                                  | {}
                                                  }
                                                )
                                            )
                        }
                  }
        |]
      )
  , DFunction
      Exported
      "Builtin$.machine$_contramap_input"
      [ NK.Label (NKT.TOpq `NKC.arrow` NKT.TOpq) "f"
      , NK.Label (NKT.TCon "Machine" [NKT.TOpq, NKT.TOpq]) "m"
      ]
      ( unsafeParseExpr
          [r|
                  case<Machine(*,*)>(m : Machine(*,*)) {
                    | ( Machine : record({ state : * | step : */*/Machine(*,*) | view : */* | {} })/Machine(*,*)
                      , $r : record({ state : * | step : */*/Machine(*,*) | view : */* | {} })
                      ) =>
                        case<Machine(*,*)>($r : record({ state : * | step : */*/Machine(*,*) | view : */* | {} })) {
                          | ( $Record : { state : * | step : */*/Machine(*,*) | view : */* | {} }/record({ state : * | step : */*/Machine(*,*) | view : */* | {} })
                            , $row : { state : * | step : */*/Machine(*,*) | view : */* | {} }
                            ) =>
                              let 
                                $state : * =
                                  get^state<*>($row : { state : * | * })
                                in
                                  let
                                    $step : */*/Machine(*,*) =
                                      get^step<*/*/Machine(*,*)>($row : { step : */*/Machine(*,*) | * })
                                    in
                                      let
                                        $view : */* =
                                          get^view<*/*>($row : { view : */* | * })
                                        in
                                          @<Machine(*,*)>
                                            ( Machine : record({ state : * | step : */*/Machine(*,*) | view : */* | {} })/Machine(*,*)
                                            , @<record({ state : * | step : */*/Machine(*,*) | view : */* | {} })>
                                                ( $Record : { state : * | step : */*/Machine(*,*) | view : */* | {} }/record({ state : * | step : */*/Machine(*,*) | view : */* | {} })
                                                , { state = $state : *
                                                  | step = 
                                                     fn(inp : *, s : *) =>
                                                       @<Machine(*,*)>
                                                         ( $step : */*/Machine(*,*)
                                                         , @<*>
                                                             ( f : */*
                                                             , inp : *
                                                             )
                                                         , s : *
                                                         )
                                                  | view = $view : */*
                                                  | {}
                                                  }
                                                )
                                            )
                        }
                  }
        |]
      )
  , DFunction
      Exported
      "Builtin$.machine$_cofix"
      [ NK.Label (NKT.TCon "Machine" [NKT.TOpq, NKT.TOpq] `NKC.arrow` NKT.TCon "Machine" [NKT.TOpq, NKT.TOpq]) "f"
      ]
      ( unsafeParseExpr
          [r|
                  0 : int32
        |]
      )
  ]
