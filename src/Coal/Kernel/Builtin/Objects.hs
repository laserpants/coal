{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

{- HLINT ignore -}

module Coal.Kernel.Builtin.Objects (builtinObjects, builtinInstance) where

import qualified Coal.Compiler.Builtin.Traits as Trait
import qualified Coal.Kernel.Language.Expr as Kernel
import Coal.Kernel.Language.Module (Module (..))
import Coal.Kernel.Language.Object (FunctionScope (..), Object (..))
import qualified Coal.Kernel.Language.Type as Kernel
import qualified Coal.Kernel.Language.Type.Constructors as Kernel
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

builtinObjects :: Module Kernel.Type
builtinObjects = objects

builtinInstance :: (Serializable t) => Trait t -> Name -> Name
builtinInstance trait name = instanceLabel trait ("Builtin$." <> name)

objects :: Module Kernel.Type
objects =
  Module
    { moduleName = "Builtin$"
    , moduleImports = []
    , moduleObjects = objectList
    }

objectList :: [Object Kernel.Type]
objectList =
  [ DData
      "Ordering"
      [ ("EqualTo", Kernel.TCon "Ordering" [])
      , ("GreaterThan", Kernel.TCon "Ordering" [])
      , ("LessThan", Kernel.TCon "Ordering" [])
      ]
  , DData
      "Option"
      [ ("None", Kernel.TCon "Option" [Kernel.TOpq])
      , ("Some", Kernel.arrow Kernel.TOpq (Kernel.TCon "Option" [Kernel.TOpq]))
      ]
  , -- Machine: single constructor taking one opaque argument (the state record).
    -- Declared here so Builtin$ bodies can use the unqualified name.
    DData
      "Machine"
      [ ("Machine", Kernel.arrow Kernel.TOpq (Kernel.TCon "Machine" [Kernel.TOpq, Kernel.TOpq]))
      ]
  , DFunction
      Exported
      "Builtin$.operator$__not"
      [ Kernel.Label Kernel.bool "a"
      ]
      ( unsafeParseExpr
          [r|
                  if (a : bool) then false else true
        |]
      )
  , DFunction
      Exported
      "Builtin$.not"
      [ Kernel.Label Kernel.bool "a"
      ]
      ( unsafeParseExpr
          [r|
                  if (a : bool) then false else true
        |]
      )
  , DFunction
      Exported
      "Builtin$.char$_ord"
      [ Kernel.Label Kernel.char "c"
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
      [ Kernel.Label Kernel.int32 "n"
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
      "Builtin$.number$_unsafe_parse_bignum"
      [ Kernel.Label Kernel.string "input"
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
      [ Kernel.Label (Kernel.TOpq `Kernel.arrow` Kernel.TOpq) "f"
      , Kernel.Label (Kernel.TOpq `Kernel.arrow` Kernel.TOpq) "g"
      , Kernel.Label Kernel.TOpq "x"
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
      [ Kernel.Label (Kernel.TOpq `Kernel.arrow` Kernel.TOpq) "g"
      , Kernel.Label (Kernel.TOpq `Kernel.arrow` Kernel.TOpq) "f"
      , Kernel.Label Kernel.TOpq "x"
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
      [ Kernel.Label Kernel.TOpq "x"
      , Kernel.Label (Kernel.TOpq `Kernel.arrow` Kernel.TOpq) "f"
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
      [ Kernel.Label (Kernel.TOpq `Kernel.arrow` Kernel.TOpq) "f"
      , Kernel.Label Kernel.TOpq "x"
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
      [ Kernel.Label Kernel.TOpq "a"
      , Kernel.Label Kernel.TOpq "_"
      ]
      ( unsafeParseExpr
          [r|
                  a : *
        |]
      )
  , DFunction
      Exported
      "Builtin$.operator$__list_concatenation"
      [ Kernel.Label (Kernel.TCon "list" [Kernel.TOpq]) "xs"
      , Kernel.Label (Kernel.TCon "list" [Kernel.TOpq]) "ys"
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
      [ Kernel.Label Kernel.int32 "n"
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
      [ Kernel.Label Kernel.int64 "n"
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
      [ Kernel.Label Kernel.bignum "n"
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
      [ Kernel.Label Kernel.string "s"
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
      [ Kernel.Label Kernel.bool "b"
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
      [ Kernel.Label Kernel.char "c"
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
      [ Kernel.Label Kernel.float "f"
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
      [ Kernel.Label Kernel.double "d"
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
      [ Kernel.Label Kernel.int32 "n"
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
      [ Kernel.Label Kernel.int64 "n"
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
      [ Kernel.Label Kernel.bignum "n"
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
      [ Kernel.Label Kernel.string "s"
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
      [ Kernel.Label Kernel.bool "b"
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
      [ Kernel.Label Kernel.char "c"
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
      [ Kernel.Label Kernel.float "f"
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
      [ Kernel.Label Kernel.double "d"
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
      [ Kernel.Label Kernel.string "s"
      , Kernel.Label Kernel.string "t"
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
      [ Kernel.Label Kernel.int32 "n"
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
      "Builtin$.string$_int64_to_string"
      [ Kernel.Label Kernel.int64 "n"
      ]
      ( unsafeParseExpr
          [r|
                  @<string>
                    ( coal_int64_to_string : int64/string
                    , n : int64
                    )
        |]
      )
  , DFunction
      Exported
      "Builtin$.string$_float_to_string"
      [ Kernel.Label Kernel.float "f"
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
      [ Kernel.Label Kernel.double "d"
      ]
      ( unsafeParseExpr
          [r|
                  @<string>
                    ( rt_double_to_string : double/string
                    , d : double
                    )
        |]
      )
  , DFunction
      Exported
      "Builtin$.string$_char_to_string"
      [ Kernel.Label Kernel.char "c"
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
      [ Kernel.Label Kernel.bool "b"
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
      "Builtin$.string$_bignum_to_string"
      [ Kernel.Label Kernel.bignum "n"
      ]
      ( unsafeParseExpr
          [r|
                  @<string>
                    ( coal_bignum_to_string : bignum/string
                    , n : bignum
                    )
        |]
      )
  , DFunction
      Exported
      "Builtin$.nat$_unpack"
      [ Kernel.Label (Kernel.TCon "nat" []) "nat"
      ]
      ( unsafeParseExpr
          [r|
                  case<int64>(nat: $Nat) {
                    | ( $Succ : int64/$Nat
                      , succ : int64
                      ) =>
                        [+ int64](succ : int64, 1)
                    | ( $Zero : $Nat
                      ) =>
                        0
                  }
        |]
      )
  , DFunction
      Exported
      "Builtin$.nat$_pack"
      [ Kernel.Label Kernel.int64 "n"
      ]
      ( unsafeParseExpr
          [r|
                  if ([<= int64](n : int64, 0))
                    then
                      $Zero : $Nat
                    else
                      @<$Nat>
                        ( $Succ : int64/$Nat
                        , [- int64](n : int64, 1)
                        )
        |]
      )
  , DFunction
      Exported
      "Builtin$.from_int32"
      [ Kernel.Label (Kernel.TCon "Numeric" [Kernel.TOpq]) "$a"
      ]
      ( unsafeParseExpr
          [r|
                  case<int32/*>($a : Numeric(*)) {
                    | ( $Record : { from_int32 : int32/* | * }/Numeric(*)
                      , $r : { from_int32 : int32/* | * }
                      ) =>
                        get?_from_int32<int32/*>($r : { from_int32 : int32/* | * })
                  }
        |]
      )
  , DFunction
      Exported
      "Builtin$.from_int64"
      [ Kernel.Label (Kernel.TCon "Numeric" [Kernel.TOpq]) "$a"
      ]
      ( unsafeParseExpr
          [r|
                  case<int64/*>($a : Numeric(*)) {
                    | ( $Record : { from_int64 : int64/* | * }/Numeric(*)
                      , $r : { from_int64 : int64/* | * }
                      ) =>
                        get?_from_int64<int64/*>($r : { from_int64 : int64/* | * })
                  }
        |]
      )
  , DFunction
      Exported
      "Builtin$.from_bignum"
      [ Kernel.Label (Kernel.TCon "Numeric" [Kernel.TOpq]) "$a"
      ]
      ( unsafeParseExpr
          [r|
                  case<bignum/*>($a : Numeric(*)) {
                    | ( $Record : { from_bignum : bignum/* | * }/Numeric(*)
                      , $r : { from_bignum : bignum/* | * }
                      ) =>
                        get?_from_bignum<bignum/*>($r : { from_bignum : bignum/* | * })
                  }
        |]
      )
  , DFunction
      Exported
      "Builtin$.negate"
      [ Kernel.Label (Kernel.TCon "Numeric" [Kernel.TOpq]) "$a"
      ]
      ( unsafeParseExpr
          [r|
                  case<*/*>($a : Numeric(*)) {
                    | ( $Record : { negate : */* | * }/Numeric(*)
                      , $r : { negate : */* | * }
                      ) =>
                        get?_negate<*/*>($r : { negate : */* | * })
                  }
        |]
      )
  , DFunction
      Exported
      "Builtin$.(+)"
      [ Kernel.Label (Kernel.TCon "Numeric" [Kernel.TOpq]) "$a"
      ]
      ( unsafeParseExpr
          [r|
                  case<*/*/*>($a : Numeric(*)) {
                    | ( $Record : { `(+)` : */*/* | * }/Numeric(*)
                      , $r : { `(+)` : */*/* | * }
                      ) =>
                        get?_`(+)`<*/*/*>($r : { `(+)` : */*/* | * })
                  }
        |]
      )
  , DFunction
      Exported
      "Builtin$.(-)"
      [ Kernel.Label (Kernel.TCon "Numeric" [Kernel.TOpq]) "$a"
      ]
      ( unsafeParseExpr
          [r|
                  case<*/*/*>($a : Numeric(*)) {
                    | ( $Record : { `(-)` : */*/* | * }/Numeric(*)
                      , $r : { `(-)` : */*/* | * }
                      ) =>
                        get?_`(-)`<*/*/*>($r : { `(-)` : */*/* | * })
                  }
        |]
      )
  , DFunction
      Exported
      "Builtin$.(*)"
      [ Kernel.Label (Kernel.TCon "Numeric" [Kernel.TOpq]) "$a"
      ]
      ( unsafeParseExpr
          [r|
                  case<*/*/*>($a : Numeric(*)) {
                    | ( $Record : { `(*)` : */*/* | * }/Numeric(*)
                      , $r : { `(*)` : */*/* | * }
                      ) =>
                        get?_`(*)`<*/*/*>($r : { `(*)` : */*/* | * })
                  }
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IInt32)) "from_int32")
      [ Kernel.Label Kernel.int32 "n"
      ]
      ( unsafeParseExpr
          [r|
                  n : int32
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IInt32)) "from_int64")
      [ Kernel.Label Kernel.int64 "n"
      ]
      ( unsafeParseExpr
          [r|
                  n : int64
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IInt32)) "from_bignum")
      [ Kernel.Label Kernel.bignum "n"
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
      [ Kernel.Label Kernel.int32 "lhs"
      , Kernel.Label Kernel.int32 "rhs"
      ]
      ( unsafeParseExpr
          [r|
                  [+ int32](lhs : int32, rhs : int32)
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IInt32)) "(-)")
      [ Kernel.Label Kernel.int32 "lhs"
      , Kernel.Label Kernel.int32 "rhs"
      ]
      ( unsafeParseExpr
          [r|
                  [- int32](lhs : int32, rhs : int32)
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IInt32)) "(*)")
      [ Kernel.Label Kernel.int32 "lhs"
      , Kernel.Label Kernel.int32 "rhs"
      ]
      ( unsafeParseExpr
          [r|
                  [* int32](lhs : int32, rhs : int32)
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IInt32)) "negate")
      [ Kernel.Label Kernel.int32 "n"
      ]
      ( unsafeParseExpr
          [r|
                  [- int32](0, n : int32)
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IInt64)) "from_int32")
      [ Kernel.Label Kernel.int32 "n"
      ]
      ( unsafeParseExpr
          [r|
                  n : int32
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IInt64)) "from_int64")
      [ Kernel.Label Kernel.int64 "n"
      ]
      ( unsafeParseExpr
          [r|
                  n : int64
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IInt64)) "from_bignum")
      [ Kernel.Label Kernel.bignum "n"
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
      [ Kernel.Label Kernel.int64 "lhs"
      , Kernel.Label Kernel.int64 "rhs"
      ]
      ( unsafeParseExpr
          [r|
                  [+ int64](lhs : int64, rhs : int64)
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IInt64)) "(-)")
      [ Kernel.Label Kernel.int64 "lhs"
      , Kernel.Label Kernel.int64 "rhs"
      ]
      ( unsafeParseExpr
          [r|
                  [- int64](lhs : int64, rhs : int64)
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IInt64)) "(*)")
      [ Kernel.Label Kernel.int64 "lhs"
      , Kernel.Label Kernel.int64 "rhs"
      ]
      ( unsafeParseExpr
          [r|
                  [* int64](lhs : int64, rhs : int64)
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IInt64)) "negate")
      [ Kernel.Label Kernel.int64 "n"
      ]
      ( unsafeParseExpr
          [r|
                  [- int64](0, n : int64)
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IFloat)) "from_int32")
      [ Kernel.Label Kernel.int32 "n"
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
      [ Kernel.Label Kernel.int64 "n"
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
      [ Kernel.Label Kernel.bignum "n"
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
      [ Kernel.Label Kernel.float "lhs"
      , Kernel.Label Kernel.float "rhs"
      ]
      ( unsafeParseExpr
          [r|
                  [+ float](lhs : float, rhs : float)
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IFloat)) "(-)")
      [ Kernel.Label Kernel.float "lhs"
      , Kernel.Label Kernel.float "rhs"
      ]
      ( unsafeParseExpr
          [r|
                  [- float](lhs : float, rhs : float)
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IFloat)) "(*)")
      [ Kernel.Label Kernel.float "lhs"
      , Kernel.Label Kernel.float "rhs"
      ]
      ( unsafeParseExpr
          [r|
                  [* float](lhs : float, rhs : float)
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IFloat)) "negate")
      [ Kernel.Label Kernel.float "f"
      ]
      ( unsafeParseExpr
          [r|
                  [neg float](f : float)
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IDouble)) "from_int32")
      [ Kernel.Label Kernel.int32 "n"
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
      [ Kernel.Label Kernel.int64 "n"
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
      [ Kernel.Label Kernel.bignum "n"
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
      [ Kernel.Label Kernel.double "lhs"
      , Kernel.Label Kernel.double "rhs"
      ]
      ( unsafeParseExpr
          [r|
                  [+ double](lhs : double, rhs : double)
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IDouble)) "(-)")
      [ Kernel.Label Kernel.double "lhs"
      , Kernel.Label Kernel.double "rhs"
      ]
      ( unsafeParseExpr
          [r|
                  [- double](lhs : double, rhs : double)
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IDouble)) "(*)")
      [ Kernel.Label Kernel.double "lhs"
      , Kernel.Label Kernel.double "rhs"
      ]
      ( unsafeParseExpr
          [r|
                  [* double](lhs : double, rhs : double)
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IDouble)) "negate")
      [ Kernel.Label Kernel.double "d"
      ]
      ( unsafeParseExpr
          [r|
                  [neg double](d : double)
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic INat)) "from_int32")
      [ Kernel.Label Kernel.int32 "m"
      ]
      ( unsafeParseExpr
          [r|
                  @<$Nat>
                    ( `Builtin$.nat$_pack` : int64/$Nat
                    , m : int32
                    )
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic INat)) "from_int64")
      [ Kernel.Label Kernel.int64 "m"
      ]
      ( unsafeParseExpr
          [r|
                  @<$Nat>
                    ( `Builtin$.nat$_pack` : int64/$Nat
                    , m : int64
                    )
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic INat)) "from_bignum")
      [ Kernel.Label Kernel.bignum "n"
      ]
      ( unsafeParseExpr
          [r|
                  @<$Nat>
                    ( `Builtin$.nat$_pack` : int64/$Nat
                    , @<int64>
                        ( coal_bignum_to_int64 : bignum/int64
                        , n : bignum
                        )
                    )
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic INat)) "(+)")
      [ Kernel.Label (Kernel.TCon "nat" []) "lhs"
      , Kernel.Label (Kernel.TCon "nat" []) "rhs"
      ]
      ( unsafeParseExpr
          [r|
                  @<$Nat>
                    ( `Builtin$.nat$_pack` : int64/$Nat
                    , [+ int64]
                        ( @<int64>
                            ( `Builtin$.nat$_unpack` : $Nat/int64
                            , lhs : $Nat
                            )
                        , @<int64>
                            ( `Builtin$.nat$_unpack` : $Nat/int64
                            , rhs : $Nat
                            )
                        )
                    )
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic INat)) "(-)")
      [ Kernel.Label (Kernel.TCon "nat" []) "lhs"
      , Kernel.Label (Kernel.TCon "nat" []) "rhs"
      ]
      ( unsafeParseExpr
          [r|
                  @<$Nat>
                    ( `Builtin$.nat$_pack` : int64/$Nat
                    , let
                        n : int64 =
                          [- int64]
                            ( @<int64>
                                ( `Builtin$.nat$_unpack` : $Nat/int64
                                , lhs : $Nat
                                )
                            , @<int64>
                                ( `Builtin$.nat$_unpack` : $Nat/int64
                                , rhs : $Nat
                                )
                            )
                        in
                          if ([< int64] (n : int64, 0)) 
                            then 0
                            else n : int64
                    )
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic INat)) "(*)")
      [ Kernel.Label (Kernel.TCon "nat" []) "lhs"
      , Kernel.Label (Kernel.TCon "nat" []) "rhs"
      ]
      ( unsafeParseExpr
          [r|
                  @<$Nat>
                    ( `Builtin$.nat$_pack` : int64/$Nat
                    , [* int64]
                        ( @<int64>
                            ( `Builtin$.nat$_unpack` : $Nat/int64
                            , lhs : $Nat
                            )
                        , @<int64>
                            ( `Builtin$.nat$_unpack` : $Nat/int64
                            , rhs : $Nat
                            )
                        )
                    )
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic INat)) "negate")
      [ Kernel.Label (Kernel.TCon "nat" []) "_"
      ]
      ( unsafeParseExpr
          [r|
                  $Zero : $Nat
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IBignum)) "from_int32")
      [ Kernel.Label Kernel.int32 "n"
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
      [ Kernel.Label Kernel.int64 "n"
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
      [ Kernel.Label Kernel.bignum "n"
      ]
      ( unsafeParseExpr
          [r|
                  n : bignum
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.numeric (TIntrinsic IBignum)) "(+)")
      [ Kernel.Label Kernel.bignum "p"
      , Kernel.Label Kernel.bignum "q"
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
      [ Kernel.Label Kernel.bignum "p"
      , Kernel.Label Kernel.bignum "q"
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
      [ Kernel.Label Kernel.bignum "p"
      , Kernel.Label Kernel.bignum "q"
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
      [ Kernel.Label Kernel.bignum "p"
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
      [ Kernel.Label (Kernel.TCon "Ordered" [Kernel.TOpq]) "$a"
      ]
      ( unsafeParseExpr
          [r|
                  case<*/*/Ordering>($a : Ordered(*)) {
                    | ( $Record : { compare : */*/Ordering | * }/Ordered(*)
                      , $r : { compare : */*/Ordering | * }
                      ) =>
                        get?_compare<*/*/Ordering>($r : { compare : */*/Ordering | * })
                  }
        |]
      )
  , DFunction
      Exported
      "Builtin$.(^)"
      [ Kernel.Label (Kernel.TCon "Numeric" [Kernel.TOpq]) "$a"
      , Kernel.Label Kernel.TOpq "m"
      , Kernel.Label (Kernel.TCon "nat" []) "n"
      ]
      ( unsafeParseExpr
          [r|
                  case<*>($a : Numeric(*)) {
                    | ( $Record : { `(*)` : */*/* | from_int32 : int32/* | * }/Numeric(*)
                      , $r : { `(*)` : */*/* | from_int32 : int32/* | * }
                      ) =>
                        let
                          $f : */*/* =
                            get?_`(*)`<*/*/*>($r : { `(*)` : */*/* | * })
                          in
                            let
                              $g : int32/* =
                                get?_from_int32<int32/*>($r : { from_int32 : int32/* | * })
                            in
                              let 
                                one : * =
                                  @<*>
                                    ( $g : int32/*
                                    , 1 
                                    )
                                in
                                  let
                                    z : int64 =
                                      @<int64>
                                        ( `Builtin$.nat$_unpack` : $Nat/int64
                                        , n : $Nat 
                                        )
                                    in
                                      let
                                        h : int64/*/* =
                                          fn(q : int64, r : *) =>
                                            if ([== int64](q : int64, 0))
                                              then
                                                r : *
                                              else
                                                @<*>
                                                  ( h : int64/*/*
                                                  , [- int64](q : int64, 1)
                                                  , @<*>
                                                      ( $f : */*/*
                                                      , m : *
                                                      , r : *
                                                      )
                                                  )
                                        in
                                          @<*>
                                            ( h : int64/*/*
                                            , z : int64
                                            , one : *
                                            )
                }
        |]
      )
  , DFunction
      Exported
      "Builtin$.(<)"
      [ Kernel.Label (Kernel.TCon "Ordered" [Kernel.TOpq]) "$a"
      , Kernel.Label Kernel.TOpq "x"
      , Kernel.Label Kernel.TOpq "y"
      ]
      ( unsafeParseExpr
          [r|
                  case<bool>($a : Ordered(*)) {
                    | ( $Record : { compare : */*/Ordering | * }/Ordered(*)
                      , $r : { compare : */*/Ordering | * }
                      ) =>
                        let
                          $f : */*/Ordering =
                            get?_compare<*/*/Ordering>($r : { compare : */*/Ordering | * })
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
      [ Kernel.Label (Kernel.TCon "Ordered" [Kernel.TOpq]) "$a"
      , Kernel.Label Kernel.TOpq "x"
      , Kernel.Label Kernel.TOpq "y"
      ]
      ( unsafeParseExpr
          [r|
                  case<bool>($a : Ordered(*)) {
                    | ( $Record : { compare : */*/Ordering | * }/Ordered(*)
                      , $r : { compare : */*/Ordering | * }
                      ) =>
                        let
                          $f : */*/Ordering =
                            get?_compare<*/*/Ordering>($r : { compare : */*/Ordering | * })
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
      [ Kernel.Label (Kernel.TCon "Ordered" [Kernel.TOpq]) "$a"
      , Kernel.Label Kernel.TOpq "x"
      , Kernel.Label Kernel.TOpq "y"
      ]
      ( unsafeParseExpr
          [r|
                  case<bool>($a : Ordered(*)) {
                    | ( $Record : { compare : */*/Ordering | * }/Ordered(*)
                      , $r : { compare : */*/Ordering | * }
                      ) =>
                        let
                          $f : */*/Ordering =
                            get?_compare<*/*/Ordering>($r : { compare : */*/Ordering | * })
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
      [ Kernel.Label (Kernel.TCon "Ordered" [Kernel.TOpq]) "$a"
      , Kernel.Label Kernel.TOpq "x"
      , Kernel.Label Kernel.TOpq "y"
      ]
      ( unsafeParseExpr
          [r|
                  case<bool>($a : Ordered(*)) {
                    | ( $Record : { compare : */*/Ordering | * }/Ordered(*)
                      , $r : { compare : */*/Ordering | * }
                      ) =>
                        let
                          $f : */*/Ordering =
                            get?_compare<*/*/Ordering>($r : { compare : */*/Ordering | * })
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
      (builtinInstance (Trait.ordered (TIntrinsic IUnit)) "compare")
      [ Kernel.Label Kernel.unit "x"
      , Kernel.Label Kernel.unit "y"
      ]
      ( unsafeParseExpr
          [r|
                  EqualTo : Ordering 
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.ordered (TIntrinsic IInt32)) "compare")
      [ Kernel.Label Kernel.int32 "x"
      , Kernel.Label Kernel.int32 "y"
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
      [ Kernel.Label Kernel.int64 "x"
      , Kernel.Label Kernel.int64 "y"
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
      [ Kernel.Label Kernel.float "x"
      , Kernel.Label Kernel.float "y"
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
      [ Kernel.Label Kernel.double "x"
      , Kernel.Label Kernel.double "y"
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
      [ Kernel.Label (Kernel.TCon "nat" []) "x"
      , Kernel.Label (Kernel.TCon "nat" []) "y"
      ]
      ( unsafeParseExpr
          ( [r|
                  let 
                    a : int64 = 
                      @<int64>
                        ( `Builtin$.nat$_unpack` : $Nat/int64
                        , x : $Nat )
                      in
                        let
                          b : int64 =
                            @<int64>
                              ( `Builtin$.nat$_unpack` : $Nat/int64
                              , y : $Nat )
                          in
                            @<Ordering>
                              ( `|]
              <> Text.unpack (builtinInstance (Trait.ordered (TIntrinsic IInt64)) "compare")
              <> [r|` : int64/int64/Ordering
                              , a : int64
                              , b : int64 
                              )
        |]
          )
      )
  , DFunction
      Exported
      (builtinInstance (Trait.ordered (TIntrinsic IBool)) "compare")
      [ Kernel.Label Kernel.bool "x"
      , Kernel.Label Kernel.bool "y"
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
      [ Kernel.Label Kernel.char "x"
      , Kernel.Label Kernel.char "y"
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
      [ Kernel.Label Kernel.string "s1"
      , Kernel.Label Kernel.string "s2"
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
      [ Kernel.Label Kernel.bignum "x"
      , Kernel.Label Kernel.bignum "y"
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
      [ Kernel.Label Kernel.string "str"
      ]
      ( unsafeParseExpr
          [r|
                  @<int64>
                    ( coal_string_length : string/int64
                    , str : string
                    )
        |]
      )
  , DFunction
      Exported
      "Builtin$.string$_head_unsafe"
      [ Kernel.Label Kernel.string "str"
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
      [ Kernel.Label Kernel.string "str"
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
      [ Kernel.Label Kernel.string "str"
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
      [ Kernel.Label Kernel.string "str"
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
      [ Kernel.Label (Kernel.TCon "list" [Kernel.char]) "chars"
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
      [ Kernel.Label Kernel.string "str"
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
      [ Kernel.Label (Kernel.TCon "Comparable" [Kernel.TOpq]) "$a"
      ]
      ( unsafeParseExpr
          [r|
                  case<*/*/bool>($a : Comparable(*)) {
                    | ( $Record : { `(==)` : */*/bool | * }/Comparable(*)
                      , $r : { `(==)` : */*/bool | * }
                      ) =>
                        get?_`(==)`<*/*/bool>($r : { `(==)` : */*/bool | * })
                  }
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.comparable (TIntrinsic IUnit)) "(==)")
      [ Kernel.Label Kernel.unit "x"
      , Kernel.Label Kernel.unit "y"
      ]
      ( unsafeParseExpr
          [r|
                  true
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.comparable (TIntrinsic IInt32)) "(==)")
      [ Kernel.Label Kernel.int32 "x"
      , Kernel.Label Kernel.int32 "y"
      ]
      ( unsafeParseExpr
          [r|
                  if ([== int32](x : int32, y : int32)) then true else false 
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.comparable (TIntrinsic IInt64)) "(==)")
      [ Kernel.Label Kernel.int64 "x"
      , Kernel.Label Kernel.int64 "y"
      ]
      ( unsafeParseExpr
          [r|
                  if ([== int64](x : int64, y : int64)) then true else false 
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.comparable (TIntrinsic IFloat)) "(==)")
      [ Kernel.Label Kernel.float "x"
      , Kernel.Label Kernel.float "y"
      ]
      ( unsafeParseExpr
          [r|
                  if ([== float](x : float, y : float)) then true else false 
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.comparable (TIntrinsic IDouble)) "(==)")
      [ Kernel.Label Kernel.double "x"
      , Kernel.Label Kernel.double "y"
      ]
      ( unsafeParseExpr
          [r|
                  if ([== double](x : double, y : double)) then true else false 
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.comparable (TIntrinsic IBool)) "(==)")
      [ Kernel.Label Kernel.bool "x"
      , Kernel.Label Kernel.bool "y"
      ]
      ( unsafeParseExpr
          [r|
                  if ([== bool](x : bool, y : bool)) then true else false 
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.comparable (TIntrinsic IChar)) "(==)")
      [ Kernel.Label Kernel.char "x"
      , Kernel.Label Kernel.char "y"
      ]
      ( unsafeParseExpr
          [r|
                  if ([== char](x : char, y : char)) then true else false 
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.comparable (TIntrinsic INat)) "(==)")
      [ Kernel.Label (Kernel.TCon "nat" []) "x"
      , Kernel.Label (Kernel.TCon "nat" []) "y"
      ]
      ( unsafeParseExpr
          [r|
                  let 
                    a : int64 = 
                      @<int64>
                        ( `Builtin$.nat$_unpack` : $Nat/int64
                        , x : $Nat 
                        )
                      in
                        let
                          b : int64 =
                            @<int64>
                              ( `Builtin$.nat$_unpack` : $Nat/int64
                              , y : $Nat 
                              )
                          in
                            if ([== int64](a : int64, b : int64)) 
                              then true 
                              else false 
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.comparable (TConstructor () "Ordering")) "(==)")
      [ Kernel.Label (Kernel.TCon "Ordering" []) "ord1"
      , Kernel.Label (Kernel.TCon "Ordering" []) "ord2"
      ]
      ( unsafeParseExpr
          [r|
                  case<bool>(ord1 : Ordering) {
                    | ( EqualTo : Ordering ) => 
                        case<bool>(ord2 : Ordering) {
                          | ( EqualTo     : Ordering ) => true
                          | ( LessThan    : Ordering ) => false
                          | ( GreaterThan : Ordering ) => false
                        }
                    | ( LessThan : Ordering ) => 
                        case<bool>(ord2 : Ordering) {
                          | ( EqualTo     : Ordering ) => false
                          | ( LessThan    : Ordering ) => true
                          | ( GreaterThan : Ordering ) => false
                        }
                    | ( GreaterThan : Ordering ) =>
                        case<bool>(ord2 : Ordering) {
                          | ( EqualTo     : Ordering ) => false
                          | ( LessThan    : Ordering ) => false
                          | ( GreaterThan : Ordering ) => true
                        }
                  }
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.comparable (TIntrinsic IString)) "(==)")
      [ Kernel.Label Kernel.string "str1"
      , Kernel.Label Kernel.string "str2"
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
      [ Kernel.Label Kernel.bignum "m"
      , Kernel.Label Kernel.bignum "n"
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
      [ Kernel.Label Kernel.float "q"
      , Kernel.Label Kernel.float "r"
      ]
      ( unsafeParseExpr
          [r|
                  [/ float](q : float, r : float)
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.divisible (TIntrinsic IDouble)) "(/)")
      [ Kernel.Label Kernel.double "q"
      , Kernel.Label Kernel.double "r"
      ]
      ( unsafeParseExpr
          [r|
                  [/ double](q : double, r : double)
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.modulo (TIntrinsic IInt32)) "(%)")
      [ Kernel.Label Kernel.int32 "q"
      , Kernel.Label Kernel.int32 "r"
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
      [ Kernel.Label Kernel.int64 "q"
      , Kernel.Label Kernel.int64 "r"
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
      [ Kernel.Label Kernel.bignum "q"
      , Kernel.Label Kernel.bignum "r"
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
      (builtinInstance (Trait.semigroup (TIntrinsic IUnit)) "(<>)")
      [ Kernel.Label Kernel.unit "a"
      , Kernel.Label Kernel.unit "b"
      ]
      ( unsafeParseExpr
          [r|
                  () 
        |]
      )
  , DFunction
      Exported
      (builtinInstance (Trait.semigroup (TIntrinsic IString)) "(<>)")
      [ Kernel.Label Kernel.string "s"
      , Kernel.Label Kernel.string "t"
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
      [ Kernel.Label (Kernel.TCon "list" [Kernel.TOpq]) "xs"
      , Kernel.Label (Kernel.TCon "list" [Kernel.TOpq]) "ys"
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
      [ Kernel.Label Kernel.TOpq "v"
      ]
      ( unsafeParseExpr
          [r|
                  v : *
        |]
      )
  , DFunction
      Exported
      "Builtin$.io$_return"
      [ Kernel.Label Kernel.TOpq "v"
      ]
      ( unsafeParseExpr
          [r|
                  v : IO(*)
        |]
      )
  , DFunction
      Exported
      "Builtin$.(!=)"
      [ Kernel.Label (Kernel.TCon "Comparable" [Kernel.TOpq]) "$c"
      , Kernel.Label Kernel.TOpq "a"
      , Kernel.Label Kernel.TOpq "b"
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
      (builtinInstance (Trait.comparable (TApplication () (TApplication () (TConstructor () "#Tuple2") (TVariable (Parameter () "a"))) (TVariable (Parameter () "b")))) "(==)")
      [ Kernel.Label (Kernel.TCon "Comparable" [Kernel.TOpq]) "$a"
      , Kernel.Label (Kernel.TCon "Comparable" [Kernel.TOpq]) "$b"
      , Kernel.Label (Kernel.TCon "tuple" [Kernel.TOpq, Kernel.TOpq]) "t1"
      , Kernel.Label (Kernel.TCon "tuple" [Kernel.TOpq, Kernel.TOpq]) "t2"
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
      [ Kernel.Label (Kernel.TCon "Ordered" [Kernel.TOpq]) "$a"
      , Kernel.Label (Kernel.TCon "Ordered" [Kernel.TOpq]) "$b"
      , Kernel.Label (Kernel.TCon "tuple" [Kernel.TOpq, Kernel.TOpq]) "t1"
      , Kernel.Label (Kernel.TCon "tuple" [Kernel.TOpq, Kernel.TOpq]) "t2"
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
      [ Kernel.Label Kernel.TOpq "seed"
      , Kernel.Label (Kernel.TOpq `Kernel.arrow` Kernel.TOpq `Kernel.arrow` Kernel.TOpq) "transition"
      , Kernel.Label (Kernel.TOpq `Kernel.arrow` Kernel.TOpq) "view"
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
      [ Kernel.Label (Kernel.TOpq `Kernel.arrow` Kernel.TOpq) "f"
      , Kernel.Label (Kernel.TCon "Machine" [Kernel.TOpq, Kernel.TOpq]) "m"
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
                                  get?_state<*>($row : { state : * | * })
                                in
                                  let
                                    $step : */*/Machine(*,*) =
                                      get?_step<*/*/Machine(*,*)>($row : { step : */*/Machine(*,*) | * })
                                    in
                                      let
                                        $view : */* =
                                          get?_view<*/*>($row : { view : */* | * })
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
      [ Kernel.Label (Kernel.TOpq `Kernel.arrow` Kernel.TOpq) "f"
      , Kernel.Label (Kernel.TCon "Machine" [Kernel.TOpq, Kernel.TOpq]) "m"
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
                                  get?_state<*>($row : { state : * | * })
                                in
                                  let
                                    $step : */*/Machine(*,*) =
                                      get?_step<*/*/Machine(*,*)>($row : { step : */*/Machine(*,*) | * })
                                    in
                                      let
                                        $view : */* =
                                          get?_view<*/*>($row : { view : */* | * })
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
      [ Kernel.Label (Kernel.TCon "Machine" [Kernel.TOpq, Kernel.TOpq] `Kernel.arrow` Kernel.TCon "Machine" [Kernel.TOpq, Kernel.TOpq]) "f"
      ]
      ( unsafeParseExpr
          [r|
                  @<Machine(*,*)>
                    ( Machine : record({ state : * | step : */*/Machine(*,*) | view : */* | {} })/Machine(*,*)
                    , @<record({ state : * | step : */*/Machine(*,*) | view : */* | {} })>
                        ( $Record : { state : * | step : */*/Machine(*,*) | view : */* | {} }/record({ state : * | step : */*/Machine(*,*) | view : */* | {} })
                        , { state = f : Machine(*,*)/Machine(*,*)
                          | step =
                              fn(input : *, $unused_state : *) =>
                                let
                                  $unfolded : Machine(*,*) =
                                    @<Machine(*,*)>
                                      ( f : Machine(*,*)/Machine(*,*)
                                      , @<Machine(*,*)>
                                          ( `Builtin$.machine$_cofix` : (Machine(*,*)/Machine(*,*))/Machine(*,*)
                                          , f : Machine(*,*)/Machine(*,*)
                                          )
                                      )
                                  in
                                    case<Machine(*,*)>($unfolded : Machine(*,*)) {
                                      | ( Machine : record({ state : * | step : */*/Machine(*,*) | view : */* | {} })/Machine(*,*)
                                        , $r : record({ state : * | step : */*/Machine(*,*) | view : */* | {} })
                                        ) =>
                                            case<Machine(*,*)>($r : record({ state : * | step : */*/Machine(*,*) | view : */* | {} })) {
                                              | ( $Record : { state : * | step : */*/Machine(*,*) | view : */* | {} }/record({ state : * | step : */*/Machine(*,*) | view : */* | {} })
                                                , $row : { state : * | step : */*/Machine(*,*) | view : */* | {} }
                                                ) =>
                                                    let
                                                      $inner_state : * =
                                                        get?_state<*>($row : { state : * | * })
                                                      in
                                                        let
                                                          $inner_step : */*/Machine(*,*) =
                                                            get?_step<*/*/Machine(*,*)>($row : { step : */*/Machine(*,*) | * })
                                                          in
                                                            @<Machine(*,*)>
                                                              ( $inner_step : */*/Machine(*,*)
                                                              , input : *
                                                              , $inner_state : *
                                                              )
                                            }
                                    }
                          | view =
                              fn($unused_state2 : *) =>
                                let
                                  $unfolded2 : Machine(*,*) =
                                    @<Machine(*,*)>
                                      ( f : Machine(*,*)/Machine(*,*)
                                      , @<Machine(*,*)>
                                          ( `Builtin$.machine$_cofix` : (Machine(*,*)/Machine(*,*))/Machine(*,*)
                                          , f : Machine(*,*)/Machine(*,*)
                                          )
                                      )
                                  in
                                    case<Machine(*,*)>($unfolded2 : Machine(*,*)) {
                                      | ( Machine : record({ state : * | step : */*/Machine(*,*) | view : */* | {} })/Machine(*,*)
                                        , $r2 : record({ state : * | step : */*/Machine(*,*) | view : */* | {} })
                                        ) =>
                                            case<Machine(*,*)>($r2 : record({ state : * | step : */*/Machine(*,*) | view : */* | {} })) {
                                              | ( $Record : { state : * | step : */*/Machine(*,*) | view : */* | {} }/record({ state : * | step : */*/Machine(*,*) | view : */* | {} })
                                                , $row2 : { state : * | step : */*/Machine(*,*) | view : */* | {} }
                                                ) =>
                                                    let
                                                      $inner_state2 : * =
                                                        get?_state<*>($row2 : { state : * | * })
                                                      in
                                                        let
                                                          $inner_view : */* =
                                                            get?_view<*/*>($row2 : { view : */* | * })
                                                          in
                                                            @<*>
                                                              ( $inner_view : */*
                                                              , $inner_state2 : *
                                                              )
                                            }
                                    }
                          | {}
                          }
                        )
                    )
        |]
      )
  , DFunction
      Exported
      "Builtin$.machine$_run_while"
      [ Kernel.Label (Kernel.unit `Kernel.arrow` Kernel.bool) "pred"
      , Kernel.Label (Kernel.TCon "Machine" [Kernel.TOpq, Kernel.TOpq]) "m"
      ]
      ( unsafeParseExpr
          [r|
                  if
                    ( @<bool>
                        ( pred : unit/bool
                        , ()
                        )
                    )
                  then
                    case<Machine(*,*)>(m : Machine(*,*)) {
                      | ( Machine : record({ state : * | step : */*/Machine(*,*) | view : */* | {} })/Machine(*,*)
                        , $r : record({ state : * | step : */*/Machine(*,*) | view : */* | {} })
                        ) =>
                            case<Machine(*,*)>($r : record({ state : * | step : */*/Machine(*,*) | view : */* | {} })) {
                              | ( $Record : { state : * | step : */*/Machine(*,*) | view : */* | {} }/record({ state : * | step : */*/Machine(*,*) | view : */* | {} })
                                , $row : { state : * | step : */*/Machine(*,*) | view : */* | {} }
                                ) =>
                                    let
                                      $cur_state : * =
                                        get?_state<*>($row : { state : * | * })
                                      in
                                        let
                                          $cur_step : */*/Machine(*,*) =
                                            get?_step<*/*/Machine(*,*)>($row : { step : */*/Machine(*,*) | * })
                                          in
                                            @<*>
                                              ( `Builtin$.machine$_run_while` : (unit/bool)/Machine(*,*)/*
                                              , pred : unit/bool
                                              , @<Machine(*,*)>
                                                  ( $cur_step : */*/Machine(*,*)
                                                  , ()
                                                  , $cur_state : *
                                                  )
                                              )
                            }
                    }
                  else
                    case<Machine(*,*)>(m : Machine(*,*)) {
                      | ( Machine : record({ state : * | step : */*/Machine(*,*) | view : */* | {} })/Machine(*,*)
                        , $r2 : record({ state : * | step : */*/Machine(*,*) | view : */* | {} })
                        ) =>
                            case<Machine(*,*)>($r2 : record({ state : * | step : */*/Machine(*,*) | view : */* | {} })) {
                              | ( $Record : { state : * | step : */*/Machine(*,*) | view : */* | {} }/record({ state : * | step : */*/Machine(*,*) | view : */* | {} })
                                , $row2 : { state : * | step : */*/Machine(*,*) | view : */* | {} }
                                ) =>
                                    let
                                      $fin_state : * =
                                        get?_state<*>($row2 : { state : * | * })
                                      in
                                        let
                                          $fin_view : */* =
                                            get?_view<*/*>($row2 : { view : */* | * })
                                          in
                                            @<*>
                                              ( $fin_view : */*
                                              , $fin_state : *
                                              )
                            }
                    }
        |]
      )
  , DFunction
      Exported
      "Builtin$.event$_blocking_poll"
      [ Kernel.Label Kernel.TOpq "state"
      , Kernel.Label (Kernel.TOpq `Kernel.arrow` Kernel.TCon "Option" [Kernel.TOpq]) "try_fn"
      , Kernel.Label (Kernel.TOpq `Kernel.arrow` Kernel.TOpq) "block_fn"
      ]
      ( unsafeParseExpr
          [r|
                  case<*>(@<Option(*)>(try_fn : */Option(*), state : *)) {
                    | ( None : Option(*) ) =>
                        let _ : unit = 
                          @<unit>(block_fn : */unit, state : *)
                        in 
                          @<*>
                            ( `Builtin$.event$_blocking_poll` : */(*/Option(*))/(*/unit)/*
                            , state : *
                            , try_fn : */Option(*)
                            , block_fn : */unit
                            )
                    | ( Some : */Option(*)
                      , result : *
                      ) =>
                        result : *
                  }
        |]
      )
  , DFunction
      Exported
      "Builtin$.event$_loop"
      [ Kernel.Label Kernel.TOpq "state"
      , Kernel.Label (Kernel.TOpq `Kernel.arrow` Kernel.TCon "Option" [Kernel.TOpq]) "step_fn"
      ]
      ( unsafeParseExpr
          [r|
                  case<*>(@<Option(*)>(step_fn : */Option(*), state : *)) {
                    | ( None : Option(*) ) =>
                        state : *
                    | ( Some : */Option(*)
                      , next : *
                      ) =>
                        @<*>
                          ( `Builtin$.event$_loop` : */(*/Option(*))/*
                          , next : *
                          , step_fn : */Option(*)
                          )
                  }
        |]
      )
  , DFunction
      Exported
      "Builtin$.event$_loop_io"
      [ Kernel.Label Kernel.TOpq "state"
      , Kernel.Label (Kernel.TOpq `Kernel.arrow` Kernel.TOpq) "step_fn"
      ]
      ( unsafeParseExpr
          [r|
                  case<IO(*)>(@<Option(*)>(`Builtin$.io$_eval` : IO(Option(*))/Option(*), @<IO(Option(*))>(step_fn : */IO(Option(*)), state : *))) {
                    | ( None : Option(*) ) =>
                        @<IO(*)>(`Builtin$.io$_return` : */IO(*), state : *)
                    | ( Some : */Option(*)
                      , next : *
                      ) =>
                        @<IO(*)>
                          ( `Builtin$.event$_loop_io` : */(*/IO(Option(*)))/IO(*)
                          , next : *
                          , step_fn : */IO(Option(*))
                          )
                  }
        |]
      )
  , DFunction
      Exported
      "Builtin$.runtime$_panic"
      [ Kernel.Label Kernel.string "str"
      ]
      ( unsafeParseExpr
          [r|
                        @<*>
                          ( coal_panic : string/*
                          , str : string
                          )
        |]
      )
  ]
