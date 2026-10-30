{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

{- HLINT ignore -}

module Coal.Kernel.Builtin.Objects (builtinObjects, builtinInstance) where

import qualified Coal.Compiler.Builtin.Traits as Trait
import Coal.Kernel.Language.Module (Module (..))
import Coal.Kernel.Language.Object (Object (..))
import qualified Coal.Kernel.Language.Type as Kernel
import qualified Coal.Kernel.Language.Type.Constructors as Kernel
import Coal.Kernel.Parser.Corn (corn, cornObject)
import Coal.Language (
  Intrinsic (..),
  Parameter (..),
  Type (TApplication, TConstructor, TIntrinsic, TVariable),
  builtinInstance,
 )
import qualified Data.Text as Text
import Text.RawString.QQ (r)

builtinObjects :: Module Kernel.Type
builtinObjects = objects

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
  , -- Free monad: two constructors, listed in lexicographic order so that the
    -- positional tags match those of the surface `Free` type in
    -- @Coal.Monad.Free@ (Free = 0, Pure = 1). Declared here so Builtin$ bodies
    -- can use the unqualified names.
    DData
      "Free"
      [ ("Free", Kernel.arrow Kernel.TOpq (Kernel.TCon "Free" [Kernel.TOpq, Kernel.TOpq]))
      , ("Pure", Kernel.arrow Kernel.TOpq (Kernel.TCon "Free" [Kernel.TOpq, Kernel.TOpq]))
      ]
  , [corn|
      Builtin$.operator$__not(a : bool) =
        if (a : bool) then false else true
    |]
  , [corn|
      Builtin$.not(a : bool) =
        if (a : bool) then false else true
    |]
  , [corn|
      Builtin$.char$_ord(c : char) =
        @<char>
          ( rt_char_unbox : char/int32
          , c : char
          )
    |]
  , [corn|
      Builtin$.char$_chr(n : int32) =
        @<char>
          ( rt_char_box : int32/char
          , n : int32
          )
    |]
  , [corn|
      Builtin$.number$_unsafe_parse_integer(input : string) =
        @<integer>
          ( coal_bignum_init : string/integer
          , input : string
          )
    |]
  , [corn|
      Builtin$.operator$__reverse_composition(f : */*, g : */*, x : *) =
        @<*>
          ( f : */*
          , @<*>
              ( g : */*
              , x : *
              )
          )
    |]
  , [corn|
      Builtin$.operator$__forward_composition(g : */*, f : */*, x : *) =
        @<*>
          ( f : */*
          , @<*>
              ( g : */*
              , x : *
              )
          )
    |]
  , [corn|
      Builtin$.operator$__reverse_application(x : *, f : */*) =
        @<*>
          ( f : */*
          , x : *
          )
    |]
  , [corn|
      Builtin$.operator$__forward_application(f : */*, x : *) =
        @<*>
          ( f : */*
          , x : *
          )
    |]
  , [corn|
      Builtin$.always(a : *, _ : *) =
        a : *
    |]
  , [corn|
      Builtin$.operator$__list_concatenation(xs : list(*), ys : list(*)) =
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
  , [corn|
      Builtin$.io$_print_int32(n : int32) =
        @<*>
          ( coal_print_int32 : int32/*
          , n : int32
          )
    |]
  , [corn|
      Builtin$.io$_print_int64(n : int64) =
        @<*>
          ( coal_print_int64 : int64/*
          , n : int64
          )
    |]
  , [corn|
      Builtin$.io$_print_integer(n : integer) =
        @<*>
          ( coal_print_bignum : integer/*
          , n : integer
          )
    |]
  , [corn|
      Builtin$.io$_print_string(s : string) =
        @<*>
          ( coal_print_string : string/*
          , s : string
          )
    |]
  , [corn|
      Builtin$.io$_print_bool(b : bool) =
        @<*>
          ( coal_print_bool : bool/*
          , b : bool
          )
    |]
  , [corn|
      Builtin$.io$_print_char(c : char) =
        @<*>
          ( coal_print_char : char/*
          , c : char
          )
    |]
  , [corn|
      Builtin$.io$_print_float(f : float) =
        @<*>
          ( coal_print_float : float/*
          , f : float
          )
    |]
  , [corn|
      Builtin$.io$_print_double(d : double) =
        @<*>
          ( coal_print_double : double/*
          , d : double
          )
    |]
  , [corn|
      Builtin$.io$_println_int32(n : int32) =
        @<*>
          ( coal_println_int32 : int32/*
          , n : int32
          )
    |]
  , [corn|
      Builtin$.io$_println_int64(n : int64) =
        @<*>
          ( coal_println_int64 : int64/*
          , n : int64
          )
    |]
  , [corn|
      Builtin$.io$_println_integer(n : integer) =
        @<*>
          ( coal_println_bignum : integer/*
          , n : integer
          )
    |]
  , [corn|
      Builtin$.io$_println_string(s : string) =
        @<*>
          ( coal_println_string : string/*
          , s : string
          )
    |]
  , [corn|
      Builtin$.io$_println_bool(b : bool) =
        @<*>
          ( coal_println_bool : bool/*
          , b : bool
          )
    |]
  , [corn|
      Builtin$.io$_println_char(c : char) =
        @<*>
          ( coal_println_char : char/*
          , c : char
          )
    |]
  , [corn|
      Builtin$.io$_println_float(f : float) =
        @<*>
          ( rt_println_float : float/*
          , f : float
          )
    |]
  , [corn|
      Builtin$.io$_println_double(d : double) =
        @<*>
          ( rt_println_double : double/*
          , d : double
          )
    |]
  , [corn|
      Builtin$.operator$__string_concatenation(s : string, t : string) =
        @<string>
          ( coal_string_concat : string/string/string
          , s : string
          , t : string
          )
    |]
  , [corn|
      Builtin$.string$_int32_to_string(n : int32) =
        @<string>
          ( coal_int32_to_string : int32/string
          , n : int32
          )
    |]
  , [corn|
      Builtin$.string$_int64_to_string(n : int64) =
        @<string>
          ( coal_int64_to_string : int64/string
          , n : int64
          )
    |]
  , [corn|
      Builtin$.string$_float_to_string(f : float) =
        @<string>
          ( rt_float_to_string : float/string
          , f : float
          )
    |]
  , [corn|
      Builtin$.string$_double_to_string(d : double) =
        @<string>
          ( rt_double_to_string : double/string
          , d : double
          )
    |]
  , [corn|
      Builtin$.string$_char_to_string(c : char) =
        @<string>
          ( coal_char_to_string : char/string
          , c : char
          )
    |]
  , [corn|
      Builtin$.string$_bool_to_string(b : bool) =
        @<string>
          ( coal_bool_to_string : bool/string
          , b : bool
          )
    |]
  , [corn|
      Builtin$.string$_integer_to_string(n : integer) =
        @<string>
          ( coal_bignum_to_string : integer/string
          , n : integer
          )
    |]
  , [corn|
      Builtin$.nat$_unpack(nat : nat) =
        case<int64>(nat: $Nat) {
          | ( $Succ : int64/$Nat
            , succ : int64
            ) =>
              [+ int64](succ : int64, %1)
          | ( $Zero : $Nat
            ) =>
              %0
        }
    |]
  , [corn|
      Builtin$.nat$_pack(n : int64) =
        if ([<= int64](n : int64, %0))
          then
            $Zero : $Nat
          else
            @<$Nat>
              ( $Succ : int64/$Nat
              , [- int64](n : int64, %1)
              )
    |]
  , [corn|
      Builtin$.from_int32($a : NumericBase(*)) =
        case<int32/*>($a : NumericBase(*)) {
          | ( $Record : { from_int32 : int32/* | * }/NumericBase(*)
            , $r : { from_int32 : int32/* | * }
            ) =>
              get?_from_int32<int32/*>($r : { from_int32 : int32/* | * })
        }
    |]
  , [corn|
      Builtin$.from_int64($a : NumericBase(*)) =
        case<int64/*>($a : NumericBase(*)) {
          | ( $Record : { from_int64 : int64/* | * }/NumericBase(*)
            , $r : { from_int64 : int64/* | * }
            ) =>
              get?_from_int64<int64/*>($r : { from_int64 : int64/* | * })
        }
    |]
  , [corn|
      Builtin$.from_integer($a : NumericBase(*)) =
        case<integer/*>($a : NumericBase(*)) {
          | ( $Record : { from_integer : integer/* | * }/NumericBase(*)
            , $r : { from_integer : integer/* | * }
            ) =>
              get?_from_integer<integer/*>($r : { from_integer : integer/* | * })
        }
    |]
  , [corn|
      Builtin$.from_negative_int32($a : Numeric(*)) =
        case<int32/*>($a : Numeric(*)) {
          | ( $Record : { from_negative_int32 : int32/* | * }/Numeric(*)
            , $r : { from_negative_int32 : int32/* | * }
            ) =>
              get?_from_negative_int32<int32/*>($r : { from_negative_int32 : int32/* | * })
        }
    |]
  , [corn|
      Builtin$.from_negative_int64($a : Numeric(*)) =
        case<int64/*>($a : Numeric(*)) {
          | ( $Record : { from_negative_int64 : int64/* | * }/Numeric(*)
            , $r : { from_negative_int64 : int64/* | * }
            ) =>
              get?_from_negative_int64<int64/*>($r : { from_negative_int64 : int64/* | * })
        }
    |]
  , [corn|
      Builtin$.from_negative_integer($a : Numeric(*)) =
        case<integer/*>($a : Numeric(*)) {
          | ( $Record : { from_negative_integer : integer/* | * }/Numeric(*)
            , $r : { from_negative_integer : integer/* | * }
            ) =>
              get?_from_negative_integer<integer/*>($r : { from_negative_integer : integer/* | * })
        }
    |]
  , [corn|
      Builtin$.negate($a : Numeric(*)) =
        case<*/*>($a : Numeric(*)) {
          | ( $Record : { negate : */* | * }/Numeric(*)
            , $r : { negate : */* | * }
            ) =>
              get?_negate<*/*>($r : { negate : */* | * })
        }
    |]
  , [corn|
      Builtin$.(+)($a : NumericBase(*)) =
        case<*/*/*>($a : NumericBase(*)) {
          | ( $Record : { `(+)` : */*/* | * }/NumericBase(*)
            , $r : { `(+)` : */*/* | * }
            ) =>
              get?_`(+)`<*/*/*>($r : { `(+)` : */*/* | * })
        }
    |]
  , [corn|
      Builtin$.(-)($a : Numeric(*)) =
        case<*/*/*>($a : Numeric(*)) {
          | ( $Record : { `(-)` : */*/* | * }/Numeric(*)
            , $r : { `(-)` : */*/* | * }
            ) =>
              get?_`(-)`<*/*/*>($r : { `(-)` : */*/* | * })
        }
    |]
  , [corn|
      Builtin$.(*)($a : NumericBase(*)) =
        case<*/*/*>($a : NumericBase(*)) {
          | ( $Record : { `(*)` : */*/* | * }/NumericBase(*)
            , $r : { `(*)` : */*/* | * }
            ) =>
              get?_`(*)`<*/*/*>($r : { `(*)` : */*/* | * })
        }
    |]
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numericBase (TIntrinsic IInt32)) "from_int32")
             <> [r|
          (n : int32) =
            n : int32|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numericBase (TIntrinsic IInt32)) "from_int64")
             <> [r|
          (n : int64) =
            n : int64|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numericBase (TIntrinsic IInt32)) "from_integer")
             <> [r|
          (n : integer) =
            @<int32>
              ( coal_bignum_to_int32 : integer/int32
              , n : integer
              )|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numericBase (TIntrinsic IInt32)) "(+)")
             <> [r|
          (lhs : int32, rhs : int32) =
            [+ int32](lhs : int32, rhs : int32)|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numeric (TIntrinsic IInt32)) "(-)")
             <> [r|
          (lhs : int32, rhs : int32) =
            [- int32](lhs : int32, rhs : int32)|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numericBase (TIntrinsic IInt32)) "(*)")
             <> [r|
          (lhs : int32, rhs : int32) =
            [* int32](lhs : int32, rhs : int32)|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numeric (TIntrinsic IInt32)) "negate")
             <> [r|
          (n : int32) =
            [- int32](0, n : int32)|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numericBase (TIntrinsic IInt64)) "from_int32")
             <> [r|
          (n : int32) =
            n : int32|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numericBase (TIntrinsic IInt64)) "from_int64")
             <> [r|
          (n : int64) =
            n : int64|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numericBase (TIntrinsic IInt64)) "from_integer")
             <> [r|
          (n : integer) =
            @<int64>
              ( coal_bignum_to_int64 : integer/int64
              , n : integer
              )|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numericBase (TIntrinsic IInt64)) "(+)")
             <> [r|
          (lhs : int64, rhs : int64) =
            [+ int64](lhs : int64, rhs : int64)|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numeric (TIntrinsic IInt64)) "(-)")
             <> [r|
          (lhs : int64, rhs : int64) =
            [- int64](lhs : int64, rhs : int64)|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numericBase (TIntrinsic IInt64)) "(*)")
             <> [r|
          (lhs : int64, rhs : int64) =
            [* int64](lhs : int64, rhs : int64)|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numeric (TIntrinsic IInt64)) "negate")
             <> [r|
          (n : int64) =
            [- int64](0, n : int64)|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numericBase (TIntrinsic IFloat)) "from_int32")
             <> [r|
          (n : int32) =
            @<float>
              ( coal_int32_to_float : int32/float
              , n : int32
              )|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numericBase (TIntrinsic IFloat)) "from_int64")
             <> [r|
          (n : int64) =
            @<float>
              ( coal_int64_to_float : int64/float
              , n : int64
              )|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numericBase (TIntrinsic IFloat)) "from_integer")
             <> [r|
          (n : integer) =
            @<float>
              ( coal_bignum_to_float : integer/float
              , n : integer
              )|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numericBase (TIntrinsic IFloat)) "(+)")
             <> [r|
          (lhs : float, rhs : float) =
            [+ float](lhs : float, rhs : float)|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numeric (TIntrinsic IFloat)) "(-)")
             <> [r|
          (lhs : float, rhs : float) =
            [- float](lhs : float, rhs : float)|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numericBase (TIntrinsic IFloat)) "(*)")
             <> [r|
          (lhs : float, rhs : float) =
            [* float](lhs : float, rhs : float)|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numeric (TIntrinsic IFloat)) "negate")
             <> [r|
          (f : float) =
            [neg float](f : float)|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numericBase (TIntrinsic IDouble)) "from_int32")
             <> [r|
          (n : int32) =
            @<double>
              ( coal_int32_to_double : int32/double
              , n : int32
              )|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numericBase (TIntrinsic IDouble)) "from_int64")
             <> [r|
          (n : int64) =
            @<double>
              ( coal_int64_to_double : int64/double
              , n : int64
              )|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numericBase (TIntrinsic IDouble)) "from_integer")
             <> [r|
          (n : integer) =
            @<double>
              ( coal_bignum_to_double : integer/double
              , n : integer
              )|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numericBase (TIntrinsic IDouble)) "(+)")
             <> [r|
          (lhs : double, rhs : double) =
            [+ double](lhs : double, rhs : double)|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numeric (TIntrinsic IDouble)) "(-)")
             <> [r|
          (lhs : double, rhs : double) =
            [- double](lhs : double, rhs : double)|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numericBase (TIntrinsic IDouble)) "(*)")
             <> [r|
          (lhs : double, rhs : double) =
            [* double](lhs : double, rhs : double)|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numeric (TIntrinsic IDouble)) "negate")
             <> [r|
          (d : double) =
            [neg double](d : double)|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numericBase (TIntrinsic INat)) "from_int32")
             <> [r|
          (m : int32) =
            @<$Nat>
              ( `Builtin$.nat$_pack` : int64/$Nat
              , @<int64>
                  ( coal_int32_to_int64 : int32/int64
                  , m : int32
                  )
              )|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numericBase (TIntrinsic INat)) "from_int64")
             <> [r|
          (m : int64) =
            @<$Nat>
              ( `Builtin$.nat$_pack` : int64/$Nat
              , m : int64
              )|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numericBase (TIntrinsic INat)) "from_integer")
             <> [r|
          (n : integer) =
            @<$Nat>
              ( `Builtin$.nat$_pack` : int64/$Nat
              , @<int64>
                  ( coal_bignum_to_int64 : integer/int64
                  , n : integer
                  )
              )|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numericBase (TIntrinsic INat)) "(+)")
             <> [r|
          (lhs : nat, rhs : nat) =
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
              )|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numericBase (TIntrinsic INat)) "(*)")
             <> [r|
          (lhs : nat, rhs : nat) =
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
              )|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numericBase (TIntrinsic IBignum)) "from_int32")
             <> [r|
          (n : int32) =
            @<integer>
              ( coal_int32_to_bignum : int32/integer
              , n : int32
              )|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numericBase (TIntrinsic IBignum)) "from_int64")
             <> [r|
          (n : int64) =
            @<integer>
              ( coal_int64_to_bignum : int64/integer
              , n : int64
              )|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numericBase (TIntrinsic IBignum)) "from_integer")
             <> [r|
          (n : integer) =
            n : integer|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numericBase (TIntrinsic IBignum)) "(+)")
             <> [r|
          (p : integer, q : integer) =
            @<integer>
              ( coal_bignum_add : integer/integer/integer
              , p : integer
              , q : integer
              )|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numeric (TIntrinsic IBignum)) "(-)")
             <> [r|
          (p : integer, q : integer) =
            @<integer>
              ( coal_bignum_sub : integer/integer/integer
              , p : integer
              , q : integer
              )|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numericBase (TIntrinsic IBignum)) "(*)")
             <> [r|
          (p : integer, q : integer) =
            @<integer>
              ( coal_bignum_mul : integer/integer/integer
              , p : integer
              , q : integer
              )|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numeric (TIntrinsic IBignum)) "negate")
             <> [r|
          (p : integer) =
            @<integer>
              ( coal_bignum_neg : integer/integer
              , p : integer
              )|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numeric (TIntrinsic IInt32)) "from_negative_int32")
             <> [r|
          (n : int32) =
            n : int32|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numeric (TIntrinsic IInt32)) "from_negative_int64")
             <> [r|
          (n : int64) =
            n : int64|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numeric (TIntrinsic IInt32)) "from_negative_integer")
             <> [r|
          (n : integer) =
            @<int32>
              ( coal_bignum_to_int32 : integer/int32
              , n : integer
              )|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numeric (TIntrinsic IInt64)) "from_negative_int32")
             <> [r|
          (n : int32) =
            n : int32|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numeric (TIntrinsic IInt64)) "from_negative_int64")
             <> [r|
          (n : int64) =
            n : int64|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numeric (TIntrinsic IInt64)) "from_negative_integer")
             <> [r|
          (n : integer) =
            @<int64>
              ( coal_bignum_to_int64 : integer/int64
              , n : integer
              )|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numeric (TIntrinsic IFloat)) "from_negative_int32")
             <> [r|
          (n : int32) =
            @<float>
              ( coal_int32_to_float : int32/float
              , n : int32
              )|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numeric (TIntrinsic IFloat)) "from_negative_int64")
             <> [r|
          (n : int64) =
            @<float>
              ( coal_int64_to_float : int64/float
              , n : int64
              )|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numeric (TIntrinsic IFloat)) "from_negative_integer")
             <> [r|
          (n : integer) =
            @<float>
              ( coal_bignum_to_float : integer/float
              , n : integer
              )|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numeric (TIntrinsic IDouble)) "from_negative_int32")
             <> [r|
          (n : int32) =
            @<double>
              ( coal_int32_to_double : int32/double
              , n : int32
              )|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numeric (TIntrinsic IDouble)) "from_negative_int64")
             <> [r|
          (n : int64) =
            @<double>
              ( coal_int64_to_double : int64/double
              , n : int64
              )|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numeric (TIntrinsic IDouble)) "from_negative_integer")
             <> [r|
          (n : integer) =
            @<double>
              ( coal_bignum_to_double : integer/double
              , n : integer
              )|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numeric (TIntrinsic IBignum)) "from_negative_int32")
             <> [r|
          (n : int32) =
            @<integer>
              ( coal_int32_to_bignum : int32/integer
              , n : int32
              )|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numeric (TIntrinsic IBignum)) "from_negative_int64")
             <> [r|
          (n : int64) =
            @<integer>
              ( coal_int64_to_bignum : int64/integer
              , n : int64
              )|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.numeric (TIntrinsic IBignum)) "from_negative_integer")
             <> [r|
          (n : integer) =
            n : integer|]
         )
     )
  , [corn|
      Builtin$.compare($a : Ordered(*)) =
        case<*/*/Ordering>($a : Ordered(*)) {
          | ( $Record : { compare : */*/Ordering | * }/Ordered(*)
            , $r : { compare : */*/Ordering | * }
            ) =>
              get?_compare<*/*/Ordering>($r : { compare : */*/Ordering | * })
        }
    |]
  , [corn|
      Builtin$.(^)($a : NumericBase(*), m : *, n : nat) =
          case<*>($a : NumericBase(*)) {
            | ( $Record : { `(*)` : */*/* | from_int32 : int32/* | * }/NumericBase(*)
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
                                    if ([== int64](q : int64, %0))
                                      then
                                        r : *
                                      else
                                        @<*>
                                          ( h : int64/*/*
                                          , [- int64](q : int64, %1)
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
  , [corn|
      Builtin$.(<)($a : Ordered(*), x : *, y : *) =
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
  , [corn|
      Builtin$.(<=)($a : Ordered(*), x : *, y : *) =
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
  , [corn|
      Builtin$.(>)($a : Ordered(*), x : *, y : *) =
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
  , [corn|
      Builtin$.(>=)($a : Ordered(*), x : *, y : *) =
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
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.ordered (TIntrinsic IUnit)) "compare")
             <> [r|
          (x : unit, y : unit) =
            EqualTo : Ordering|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.ordered (TIntrinsic IInt32)) "compare")
             <> [r|
          (x : int32, y : int32) =
            if ([< int32](x : int32, y : int32))
              then
                LessThan : Ordering
              else
                if ([> int32](x : int32, y : int32))
                  then
                    GreaterThan : Ordering
                  else
                    EqualTo : Ordering|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.ordered (TIntrinsic IInt64)) "compare")
             <> [r|
          (x : int64, y : int64) =
            if ([< int64](x : int64, y : int64))
              then
                LessThan : Ordering
              else
                if ([> int64](x : int64, y : int64))
                  then
                    GreaterThan : Ordering
                  else
                    EqualTo : Ordering|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.ordered (TIntrinsic IFloat)) "compare")
             <> [r|
          (x : float, y : float) =
            if ([< float](x : float, y : float))
              then
                LessThan : Ordering
              else
                if ([> float](x : float, y : float))
                  then
                    GreaterThan : Ordering
                  else
                    EqualTo : Ordering|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.ordered (TIntrinsic IDouble)) "compare")
             <> [r|
          (x : double, y : double) =
            if ([< double](x : double, y : double))
              then
                LessThan : Ordering
              else
                if ([> double](x : double, y : double))
                  then
                    GreaterThan : Ordering
                  else
                    EqualTo : Ordering|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.ordered (TIntrinsic INat)) "compare")
             <> [r|
          (x : nat, y : nat) =
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
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.ordered (TIntrinsic IBool)) "compare")
             <> [r|
          (x : bool, y : bool) =
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
                 else EqualTo : Ordering|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.ordered (TIntrinsic IChar)) "compare")
             <> [r|
          (x : char, y : char) =
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
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.ordered (TIntrinsic IString)) "compare")
             <> [r|
          (s1 : string, s2 : string) =
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
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.ordered (TIntrinsic IBignum)) "compare")
             <> [r|
          (x : integer, y : integer) =
            let
              is_lt : bool =
                @<bool>
                  ( coal_bignum_lt : integer/integer/bool
                  , x : integer
                  , y : integer
                  )
              in
                if (is_lt : bool)
                  then LessThan : Ordering
                  else
                    let
                      is_gt : bool =
                        @<bool>
                          ( coal_bignum_gt : integer/integer/bool
                          , x : integer
                          , y : integer
                          )
                      in
                        if (is_gt : bool)
                          then GreaterThan : Ordering
                          else EqualTo : Ordering|]
         )
     )
  , [corn|
      Builtin$.string$_length(str : string) =
        @<int64>
          ( coal_string_length : string/int64
          , str : string
          )
    |]
  , [corn|
      Builtin$.string$_head_unsafe(str : string) =
        @<char>
          ( coal_string_head : string/char
          , str : string
          )
    |]
  , [corn|
      Builtin$.string$_tail(str : string) =
        @<string>
          ( coal_string_tail : string/string
          , str : string
          )
    |]
  , [corn|
      Builtin$.string$_reverse(str : string) =
        @<string>
          ( coal_string_reverse : string/string
          , str : string
          )
    |]
  , [corn|
      Builtin$.string$_remove_whitespace(str : string) =
        @<string>
          ( coal_string_remove_whitespace : string/string
          , str : string
          )
    |]
  , [corn|
      Builtin$.string$_from_list(chars : list(char)) =
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
  , [corn|
      Builtin$.string$_to_list(str : string) =
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
  , [corn|
      Builtin$.(==)($a : Comparable(*)) =
        case<*/*/bool>($a : Comparable(*)) {
          | ( $Record : { `(==)` : */*/bool | * }/Comparable(*)
            , $r : { `(==)` : */*/bool | * }
            ) =>
              get?_`(==)`<*/*/bool>($r : { `(==)` : */*/bool | * })
        }
    |]
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.comparable (TIntrinsic IUnit)) "(==)")
             <> [r|
          (x : unit, y : unit) =
            true|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.comparable (TIntrinsic IInt32)) "(==)")
             <> [r|
          (x : int32, y : int32) =
            if ([== int32](x : int32, y : int32)) then true else false|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.comparable (TIntrinsic IInt64)) "(==)")
             <> [r|
          (x : int64, y : int64) =
            if ([== int64](x : int64, y : int64)) then true else false|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.comparable (TIntrinsic IFloat)) "(==)")
             <> [r|
          (x : float, y : float) =
            if ([== float](x : float, y : float)) then true else false|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.comparable (TIntrinsic IDouble)) "(==)")
             <> [r|
          (x : double, y : double) =
            if ([== double](x : double, y : double)) then true else false|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.comparable (TIntrinsic IBool)) "(==)")
             <> [r|
          (x : bool, y : bool) =
            if ([== bool](x : bool, y : bool)) then true else false|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.comparable (TIntrinsic IChar)) "(==)")
             <> [r|
          (x : char, y : char) =
            if ([== char](x : char, y : char)) then true else false|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.comparable (TIntrinsic INat)) "(==)")
             <> [r|
          (x : nat, y : nat) =
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
                        else false|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.comparable (TConstructor () "Ordering")) "(==)")
             <> [r|
          (ord1 : Ordering, ord2 : Ordering) =
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
            }|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.comparable (TIntrinsic IString)) "(==)")
             <> [r|
          (str1 : string, str2 : string) =
            @<bool>
              ( coal_string_compare : string/string/bool
              , str1 : string
              , str2 : string
              )|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.comparable (TIntrinsic IBignum)) "(==)")
             <> [r|
          (m : integer, n : integer) =
            @<bool>
              ( coal_bignum_eq : integer/integer/bool
              , m : integer
              , n : integer
              )|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.divisible (TIntrinsic IFloat)) "(/)")
             <> [r|
          (q : float, r : float) =
            [/ float](q : float, r : float)|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.divisible (TIntrinsic IDouble)) "(/)")
             <> [r|
          (q : double, r : double) =
            [/ double](q : double, r : double)|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.modulo (TIntrinsic IInt32)) "(%)")
             <> [r|
          (q : int32, r : int32) =
            if ([== int32](r : int32, 0))
              then
                None : Option(int32)
              else
                @<Option(int32)>
                  ( Some : int32/Option(int32)
                  , @<int32>
                      ( coal_int32_mod : int32/int32/int32
                      , q : int32
                      , r : int32
                      )
                  )|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.modulo (TIntrinsic IInt64)) "(%)")
             <> [r|
          (q : int64, r : int64) =
            if ([== int64](r : int64, %0))
              then
                None : Option(int64)
              else
                @<Option(int64)>
                  ( Some : int64/Option(int64)
                  , @<int64>
                      ( coal_int64_mod : int64/int64/int64
                      , q : int64
                      , r : int64
                      )
                  )|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.modulo (TIntrinsic IBignum)) "(%)")
             <> [r|
          (q : integer, r : integer) =
            if (@<bool>(coal_bignum_eq : integer/integer/bool, r : integer, %%0))
              then
                None : Option(integer)
              else
                @<Option(integer)>
                  ( Some : integer/Option(integer)
                  , @<integer>
                      ( coal_bignum_mod : integer/integer/integer
                      , q : integer
                      , r : integer
                      )
                  )|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.semigroup (TIntrinsic IUnit)) "(<>)")
             <> [r|
          (a : unit, b : unit) =
            ()|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.semigroup (TIntrinsic IString)) "(<>)")
             <> [r|
          (s : string, t : string) =
            @<string>
              ( `Builtin$.operator$__string_concatenation` : string/string/string
              , s : string
              , t : string
              )|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.semigroup (TApplication () (TConstructor () "List") (TVariable (Parameter () "a")))) "(<>)")
             <> [r|
          (xs : list(*), ys : list(*)) =
            @<list(*)>
              ( `Builtin$.operator$__list_concatenation` : list(*)/list(*)/list(*)
              , xs : list(*)
              , ys : list(*)
              )|]
         )
     )
  , [corn|
      Builtin$.io$_eval(io : */*) =
        @<*>(io : */*, ())
    |]
  , [corn|
      Builtin$.io$_return(v : *) =
        @<IO(*)>(`Builtin$.io$_defer` : (*/*)/IO(*), fn(_ : unit) => v : *)
    |]
  , [corn|
      Builtin$.io$_defer(f : *) =
        f : IO(*)
    |]
  , [corn|
      Builtin$.(!=)($c : Comparable(*), a : *, b : *) =
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
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.comparable (TApplication () (TApplication () (TConstructor () "#Tuple2") (TVariable (Parameter () "a"))) (TVariable (Parameter () "b")))) "(==)")
             <> [r|
          ($a : Comparable(*), $b : Comparable(*), t1 : tuple(*, *), t2 : tuple(*, *)) =
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
            }|]
         )
     )
  , $( cornObject
         ( Text.unpack (builtinInstance (Trait.ordered (TApplication () (TApplication () (TConstructor () "#Tuple2") (TVariable (Parameter () "a"))) (TVariable (Parameter () "b")))) "compare")
             <> [r|
          ($a : Ordered(*), $b : Ordered(*), t1 : tuple(*, *), t2 : tuple(*, *)) =
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
            }|]
         )
     )
  , [corn|
      Builtin$.machine$_machine(seed : *, transition : */*/*, view : */*) =
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
  , [corn|
      Builtin$.machine$_map_machine(f : */*, m : Machine(*, *)) =
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
  , [corn|
      Builtin$.machine$_contramap_input(f : */*, m : Machine(*, *)) =
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
  , [corn|
      Builtin$.machine$_cofix(f : Machine(*, *)/Machine(*, *)) =
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
  , [corn|
      Builtin$.machine$_run_while(pred : unit/bool, m : Machine(*, *)) =
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
  , [corn|
      Builtin$.event$_blocking_poll(state : *, try_fn : */*, block_fn : */*) =
        @<IO(*)>(
          `Builtin$.io$_defer` : (*/*)/IO(*),
          fn(_ : unit) =>
            case<*>(
              @<Option(*)>(`Builtin$.io$_eval` : IO(Option(*))/Option(*),
                           @<IO(Option(*))>(try_fn : */IO(Option(*)), state : *))
            ) {
              | ( None : Option(*) ) =>
                  let _ : unit =
                    @<unit>(`Builtin$.io$_eval` : IO(unit)/unit,
                            @<IO(unit)>(block_fn : */IO(unit), state : *))
                  in
                    @<*>(`Builtin$.io$_eval` : IO(*)/*,
                         @<IO(*)>
                           ( `Builtin$.event$_blocking_poll` : */(*/IO(Option(*)))/(*/IO(unit))/IO(*)
                           , state : *
                           , try_fn : */IO(Option(*))
                           , block_fn : */IO(unit)
                           )
                    )
              | ( Some : */Option(*)
                , result : *
                ) =>
                  result : *
            }
        )
    |]
  , [corn|
      Builtin$.event$_loop(state : *, step_fn : */Option(*)) =
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
  , [corn|
      Builtin$.event$_loop_io(state : *, step_fn : */*) =
        @<IO(*)>(
          `Builtin$.io$_defer` : (*/*)/IO(*),
          fn(_ : unit) =>
            case<*>(
              @<Option(*)>(`Builtin$.io$_eval` : IO(Option(*))/Option(*),
                           @<IO(Option(*))>(step_fn : */IO(Option(*)), state : *))
            ) {
              | ( None : Option(*) ) =>
                  state : *
              | ( Some : */Option(*)
                , next : *
                ) =>
                  @<*>(`Builtin$.io$_eval` : IO(*)/*,
                       @<IO(*)>
                         ( `Builtin$.event$_loop_io` : */(*/IO(Option(*)))/IO(*)
                         , next : *
                         , step_fn : */IO(Option(*))
                         )
                  )
            }
        )
    |]
  , [corn|
      Builtin$.runtime$_panic(str : string) =
        @<*>
          ( coal_panic : string/*
          , str : string
          )
    |]
  , -- Free monad: map over the functor layer, recursing through the
    -- continuation. The `Functor<f>` dictionary is threaded explicitly as the
    -- first parameter, since `Free<f, a>` embeds `f<Free<f, a>>` and the
    -- payload must be mapped over to reach the recursive occurrences.
    [corn|
      Builtin$.free$_map_free($d : Functor(*), f : */*, s : Free(*,*)) =
        case<Free(*,*)>(s : Free(*,*)) {
          | ( Free : */Free(*,*)
            , ff : *
            ) =>
              let
                $map : */*/* =
                  case<*/*/*>($d : Functor(*)) {
                    | ( $Record : { map : */*/* | * }/Functor(*)
                      , $r : { map : */*/* | * }
                      ) =>
                        get?_map<*/*/*>($r : { map : */*/* | * })
                  }
              in
                @<Free(*,*)>
                  ( Free : */Free(*,*)
                  , @<*>
                      ( $map : */*/*
                      , fn(x : *) =>
                          @<Free(*,*)>
                            ( `Builtin$.free$_map_free` : Functor(*)/(*/*)/Free(*,*)/Free(*,*)
                            , $d : Functor(*)
                            , f : */*
                            , x : *
                            )
                      , ff : *
                      )
                  )
          | ( Pure : */Free(*,*)
            , x : *
            ) =>
              @<Free(*,*)>
                ( Pure : */Free(*,*)
                , @<*>(f : */*, x : *)
                )
        }
    |]
  , -- Free monad: sequence a continuation after the free structure, again
    -- recursing under the functor layer.
    [corn|
      Builtin$.free$_bind_free($d : Functor(*), ma : Free(*,*), k :  */*) =
        case<Free(*,*)>(ma : Free(*,*)) {
          | ( Free : */Free(*,*)
            , ff : *
            ) =>
              let
                $map : */*/* =
                  case<*/*/*>($d : Functor(*)) {
                    | ( $Record : { map : */*/* | * }/Functor(*)
                      , $r : { map : */*/* | * }
                      ) =>
                        get?_map<*/*/*>($r : { map : */*/* | * })
                  }
              in
                @<Free(*,*)>
                  ( Free : */Free(*,*)
                  , @<*>
                      ( $map : */*/*
                      , fn(x : *) =>
                          @<Free(*,*)>
                            ( `Builtin$.free$_bind_free` : Functor(*)/Free(*,*)/(*/*)/Free(*,*)
                            , $d : Functor(*)
                            , x : *
                            , k : */*
                            )
                      , ff : *
                      )
                  )
          | ( Pure : */Free(*,*)
            , x : *
            ) =>
              @<Free(*,*)>(k : */*, x : *)
        }
    |]
  , -- Free monad: applicative application, defined as `bind` with the value
    -- wrapped through `map`.
    [corn|
      Builtin$.free$_ap_free($d : Functor(*), mf : Free(*,*), mx : Free(*,*)) =
        @<Free(*,*)>
          ( `Builtin$.free$_bind_free` : Functor(*)/Free(*,*)/(*/*)/Free(*,*)
          , $d : Functor(*)
          , mf : Free(*,*)
          , fn(g : *) =>
              @<Free(*,*)>
                ( `Builtin$.free$_map_free` : Functor(*)/(*/*)/Free(*,*)/Free(*,*)
                , $d : Functor(*)
                , g : */*
                , mx : Free(*,*)
                )
          )
    |]
  , -- Free monad: interpret the program by handing each instruction layer to
    -- the handler and recursing through the continuation. `pure` comes from
    -- the `Applicative<m>` dictionary and `map` from the `Functor<f>`
    -- dictionary; the two dictionaries come first, in the order the
    -- `free$_interpret` scheme declares them (see
    -- `Coal.Compiler.Builtin.Functions`).
    [corn|
      Builtin$.free$_interpret($d : Applicative(*), $d_f : Functor(*), handler : */*, instructions : Free(*,*)) =
        case<*>(instructions : Free(*,*)) {
          | ( Free : */Free(*,*)
            , ins : *
            ) =>
              let
                $map : */*/* =
                  case<*/*/*>($d_f : Functor(*)) {
                    | ( $Record : { map : */*/* | * }/Functor(*)
                      , $r : { map : */*/* | * }
                      ) =>
                        get?_map<*/*/*>($r : { map : */*/* | * })
                  }
              in
                @<*>
                  ( handler : */*
                  , @<*>
                      ( $map : */*/*
                      , fn(x : *) =>
                          @<*>
                            ( `Builtin$.free$_interpret` : Applicative(*)/Functor(*)/(*/*)/Free(*,*)/*
                            , $d : Applicative(*)
                            , $d_f : Functor(*)
                            , handler : */*
                            , x : *
                            )
                      , ins : *
                      )
                  )
          | ( Pure : */Free(*,*)
            , val : *
            ) =>
              let
                $pure : */* =
                  case<*/*>($d : Applicative(*)) {
                    | ( $Record : { pure : */* | * }/Applicative(*)
                      , $r2 : { pure : */* | * }
                      ) =>
                        get?_pure<*/*>($r2 : { pure : */* | * })
                  }
              in
                @<*>
                  ( $pure : */*
                  , val : *
                  )
        }
    |]
  ]
