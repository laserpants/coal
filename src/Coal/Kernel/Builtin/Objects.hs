{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Coal.Kernel.Builtin.Objects (builtinObjects, builtinInstance) where

import Coal.Common.Label (Label (..))
import qualified Coal.Compiler.Builtin.Traits as Trait
import Coal.Kernel.Language (Module (..), Object (..), char, opaque)
import qualified Coal.Kernel.Language as Kernel
import Coal.Kernel.Parser.Expr (expr)
import Coal.Language
import Data.Text (Text)
import qualified Data.Text as Text
import Extras (Name)
import Text.Megaparsec (errorBundlePretty, runParser)
import Text.RawString.QQ (r)

builtinObjects :: Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type)
builtinObjects = unsafeParseKernelExpr <$> objects

builtinInstance :: (Serializable t) => Trait t -> Name -> Name
builtinInstance trait name = instanceLabel trait ("Builtin$." <> name)

objects :: Kernel.Module Kernel.Type Name Text
objects =
  Kernel.Module
    { moduleName = "Builtin$"
    , moduleImports =
        []
    , moduleObjects =
        [ OData "EqualTo" 0 (Kernel.TCon "Ordering" [])
        , OData "GreaterThan" 1 (Kernel.TCon "Ordering" [])
        , OData "LessThan" 2 (Kernel.TCon "Ordering" [])
        , OFunction
            "Builtin$.operator$__not"
            [ Label Kernel.bool "a"
            ]
            [r| 
                  if (a : bool) then false else true
              |]
        , OFunction
            "Builtin$.not"
            [ Label Kernel.bool "a"
            ]
            [r| 
                  if (a : bool) then false else true
              |]
        , OFunction
            "Builtin$.char$_ord"
            [ Label Kernel.char "c"
            ]
            [r| 
                  c : int32
              |]
        , OFunction
            "Builtin$.char$_chr"
            [ Label Kernel.int32 "n"
            ]
            [r| 
                  n : char
              |]
        , OFunction
            "Builtin$.number$_unsafe_parse_bignum"
            [ Label Kernel.string "input"
            ]
            [r| 
                  #(bignum_init : string/*, input : string) (fn(a : *) => a : *)
              |]
        , OFunction
            "Builtin$.operator$__reverse_composition"
            [ Label (Kernel.opaque `Kernel.arrow` Kernel.opaque) "f"
            , Label (Kernel.opaque `Kernel.arrow` Kernel.opaque) "g"
            , Label Kernel.opaque "x"
            ]
            [r| 
                  @<*>(f : */*, @<*>(g : */*, x : *))
              |]
        , OFunction
            "Builtin$.operator$__forward_composition"
            [ Label (Kernel.opaque `Kernel.arrow` Kernel.opaque) "g"
            , Label (Kernel.opaque `Kernel.arrow` Kernel.opaque) "f"
            , Label Kernel.opaque "x"
            ]
            [r| 
                  @<*>(f : */*, @<*>(g : */*, x : *))
              |]
        , OFunction
            "Builtin$.operator$__reverse_application"
            [ Label Kernel.opaque "x"
            , Label (Kernel.opaque `Kernel.arrow` Kernel.opaque) "f"
            ]
            [r| 
                  @<*>(f : */*, x : *)
              |]
        , OFunction
            "Builtin$.operator$__forward_application"
            [ Label (Kernel.opaque `Kernel.arrow` Kernel.opaque) "f"
            , Label Kernel.opaque "x"
            ]
            [r| 
                  @<*>(f : */*, x : *)
              |]
        , OFunction
            "Builtin$.always"
            [ Label Kernel.opaque "a"
            , Label Kernel.opaque "_"
            ]
            [r|   
                  a : *
              |]
        , OFunction
            "Builtin$.operator$__list_concatenation"
            [ Label (Kernel.TCon "list" [Kernel.opaque]) "xs"
            , Label (Kernel.TCon "list" [Kernel.opaque]) "ys"
            ]
            [r| 
                  match<list(*)>(xs : list(*)) {
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
        , OFunction
            "Builtin$.io$_print_int32"
            [ Label Kernel.int32 "n"
            ]
            [r|
                  #(print_int32 : int32/*, n : int32) (fn(a : *) => a : *)
              |]
        , OFunction
            "Builtin$.io$_print_int64"
            [ Label Kernel.int64 "n"
            ]
            [r|
                  #(print_int64 : int64/*, n : int64) (fn(a : *) => a : *)
              |]
        , OFunction
            "Builtin$.io$_print_bignum"
            [ Label Kernel.bignum "n"
            ]
            [r|
                  #(print_bignum : bignum/*, n : bignum) (fn(a : *) => a : *)
              |]
        , OFunction
            "Builtin$.io$_print_string"
            [ Label Kernel.string "s"
            ]
            [r|
                  #(print_string : string/*, s : string) (fn(a : *) => a : *)
              |]
        , OFunction
            "Builtin$.io$_print_bool"
            [ Label Kernel.bool "b"
            ]
            [r|
                  #(print_bool : bool/*, b : bool) (fn(a : *) => a : *)
              |]
        , OFunction
            "Builtin$.io$_print_char"
            [ Label Kernel.char "c"
            ]
            [r|
                  #(print_char : char/*, c : char) (fn(a : *) => a : *)
              |]
        , OFunction
            "Builtin$.io$_print_float"
            [ Label Kernel.float "f"
            ]
            [r|
                  #(print_float : float/*, f : float) (fn(a : *) => a : *)
              |]
        , OFunction
            "Builtin$.io$_print_double"
            [ Label Kernel.double "d"
            ]
            [r|
                  #(print_double : double/*, d : double) (fn(a : *) => a : *)
              |]
        , OFunction
            "Builtin$.io$_println_int32"
            [ Label Kernel.int32 "n"
            ]
            [r|
                  #(println_int32 : int32/*, n : int32) (fn(a : *) => a : *)
              |]
        , OFunction
            "Builtin$.io$_println_int64"
            [ Label Kernel.int64 "n"
            ]
            [r|
                  #(println_int64 : int64/*, n : int64) (fn(a : *) => a : *)
              |]
        , OFunction
            "Builtin$.io$_println_bignum"
            [ Label Kernel.bignum "n"
            ]
            [r|
                  #(println_bignum : bignum/*, n : bignum) (fn(a : *) => a : *)
              |]
        , OFunction
            "Builtin$.io$_println_string"
            [ Label Kernel.string "s"
            ]
            [r|
                  #(println_string : string/*, s : string) (fn(a : *) => a : *)
              |]
        , OFunction
            "Builtin$.io$_println_bool"
            [ Label Kernel.bool "b"
            ]
            [r|
                  #(println_bool : bool/*, b : bool) (fn(a : *) => a : *)
              |]
        , OFunction
            "Builtin$.io$_println_char"
            [ Label Kernel.char "c"
            ]
            [r|
                  #(println_char : char/*, c : char) (fn(a : *) => a : *)
              |]
        , OFunction
            "Builtin$.io$_println_float"
            [ Label Kernel.float "f"
            ]
            [r|
                  #(println_float : float/*, f : float) (fn(a : *) => a : *)
              |]
        , OFunction
            "Builtin$.io$_println_double"
            [ Label Kernel.double "d"
            ]
            [r|
                  #(println_double : double/*, d : double) (fn(a : *) => a : *)
              |]
        , OFunction
            "Builtin$.operator$__string_concatenation"
            [ Label Kernel.string "s"
            , Label Kernel.string "t"
            ]
            [r|
                  #(string_concat : string/string/string, s : string, t : string) (fn(r : string) => r : string)
              |]
        , OFunction
            "Builtin$.string$_int32_to_string"
            [ Label Kernel.int32 "n"
            ]
            [r| 
                  #(int32_to_string : int32/string, n : int32) (fn(r : string) => r : string)
              |]
        , OFunction
            "Builtin$.string$_float_to_string"
            [ Label Kernel.float "f"
            ]
            [r| 
                  #(float_to_string : float/string, f : float) (fn(r : string) => r : string)
              |]
        , OFunction
            "Builtin$.string$_double_to_string"
            [ Label Kernel.double "d"
            ]
            [r| 
                  #(double_to_string : double/string, d : double) (fn(r : string) => r : string)
              |]
        , OFunction
            "Builtin$.string$_char_to_string"
            [ Label Kernel.char "c"
            ]
            [r| 
                  #(char_to_string : char/string, c : char) (fn(r : string) => r : string)
              |]
        , OFunction
            "Builtin$.string$_bool_to_string"
            [ Label Kernel.bool "b"
            ]
            [r| 
                  #(bool_to_string : bool/string, b : bool) (fn(r : string) => r : string)
              |]
        , OFunction
            "Builtin$.nat$_unpack"
            [ Label (Kernel.TCon "$Nat" []) "nat"
            ]
            [r| 
                  match<int32>(nat: $Nat) {
                    | ( $Succ : int32/$Nat
                      , succ : int32
                      ) =>
                        [+ int32](succ : int32, 1)
                    | ( $Zero : $Nat
                      ) =>
                        0
                  }
              |]
        , OFunction
            "Builtin$.nat$_pack"
            [ Label Kernel.int32 "n"
            ]
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
        , OFunction
            "Builtin$.from_int32"
            [ Label (Kernel.TCon "Numeric" [opaque]) "$a"
            ]
            [r|
                  match<int32/*>($a : Numeric(*)) {
                    | ( $Record : { from_int32 : int32/* | * }/Numeric(*)
                      , $r : { from_int32 : int32/* | * }
                      ) =>
                        select
                          { from_int32 = $f : int32/* | _ : * } =
                            $r : { from_int32 : int32/* | * }
                          in
                            $f : int32/*
                  }
              |]
        , OFunction
            "Builtin$.from_int64"
            [ Label (Kernel.TCon "Numeric" [opaque]) "$a"
            ]
            [r|
                  match<int64/*>($a : Numeric(*)) {
                    | ( $Record : { from_int64 : int64/* | * }/Numeric(*)
                      , $r : { from_int64 : int64/* | * }
                      ) =>
                        select
                          { from_int64 = $f : int64/* | _ : * } =
                            $r : { from_int64 : int64/* | * }
                          in
                            $f : int64/*
                  }
              |]
        , OFunction
            "Builtin$.from_bignum"
            [ Label (Kernel.TCon "Numeric" [opaque]) "$a"
            ]
            [r| 
                  match<bignum/*>($a : Numeric(*)) {
                    | ( $Record : { from_bignum : bignum/* | * }/Numeric(*)
                      , $r : { from_bignum : bignum/* | * }
                      ) =>
                        select
                          { from_bignum = $f : bignum/* | _ : * } =
                            $r : { from_bignum : bignum/* | * }
                          in
                            $f : bignum/*
                  }
              |]
        , OFunction
            "Builtin$.negate"
            [ Label (Kernel.TCon "Numeric" [opaque]) "$a"
            ]
            [r| 
                  match<*/*>($a : Numeric(*)) {
                    | ( $Record : { negate : */* | * }/Numeric(*)
                      , $r : { negate : */* | * }
                      ) =>
                        select
                          { negate = $f : */* | _ : * } =
                            $r : { negate : */* | * }
                          in
                            $f : */*
                  }
              |]
        , OFunction
            "Builtin$.(+)"
            [ Label (Kernel.TCon "Numeric" [opaque]) "$a"
            ]
            [r| 
                  match<*/*/*>($a : Numeric(*)) {
                    | ( $Record : { `(+)` : */*/* | * }/Numeric(*)
                      , $r : { `(+)` : */*/* | * }
                      ) =>
                        select
                          { `(+)` = $f : */*/* | _ : * } =
                            $r : { `(+)` : */*/* | * }
                          in
                            $f : */*/*
                  }
              |]
        , OFunction
            "Builtin$.(-)"
            [ Label (Kernel.TCon "Numeric" [opaque]) "$a"
            ]
            [r| 
                  match<*/*/*>($a : Numeric(*)) {
                    | ( $Record : { `(-)` : */*/* | * }/Numeric(*)
                      , $r : { `(-)` : */*/* | * }
                      ) =>
                        select
                          { `(-)` = $f : */*/* | _ : * } =
                            $r : { `(-)` : */*/* | * }
                          in
                            $f : */*/*
                  }
              |]
        , OFunction
            "Builtin$.(*)"
            [ Label (Kernel.TCon "Numeric" [opaque]) "$a"
            ]
            [r| 
                  match<*/*/*>($a : Numeric(*)) {
                    | ( $Record : { `(*)` : */*/* | * }/Numeric(*)
                      , $r : { `(*)` : */*/* | * }
                      ) =>
                        select
                          { `(*)` = $f : */*/* | _ : * } =
                            $r : { `(*)` : */*/* | * }
                          in
                            $f : */*/*
                  }
              |]
        , -- Numeric(int32)
          OFunction
            (builtinInstance (Trait.numeric (TIntrinsic IInt32)) "from_int32")
            [ Label Kernel.int32 "n"
            ]
            [r|
                  n : int32
              |]
        , OFunction
            (builtinInstance (Trait.numeric (TIntrinsic IInt32)) "from_int64")
            [ Label Kernel.int64 "n"
            ]
            [r|
                  n : int64
              |]
        , OFunction
            (builtinInstance (Trait.numeric (TIntrinsic IInt32)) "from_bignum")
            [ Label Kernel.bignum "n"
            ]
            [r| 
                  #(bignum_to_int32 : bignum/int32, n : bignum)(fn(m : int32) => m : int32)
              |]
        , OFunction
            (builtinInstance (Trait.numeric (TIntrinsic IInt32)) "(+)")
            [ Label Kernel.int32 "lhs"
            , Label Kernel.int32 "rhs"
            ]
            [r| 
                  [+ int32](lhs : int32, rhs : int32)
              |]
        , OFunction
            (builtinInstance (Trait.numeric (TIntrinsic IInt32)) "(-)")
            [ Label Kernel.int32 "lhs"
            , Label Kernel.int32 "rhs"
            ]
            [r| 
                  [- int32](lhs : int32, rhs : int32)
              |]
        , OFunction
            (builtinInstance (Trait.numeric (TIntrinsic IInt32)) "(*)")
            [ Label Kernel.int32 "lhs"
            , Label Kernel.int32 "rhs"
            ]
            [r| 
                  [* int32](lhs : int32, rhs : int32)
              |]
        , OFunction
            (builtinInstance (Trait.numeric (TIntrinsic IInt32)) "negate")
            [ Label Kernel.int32 "n"
            ]
            [r| 
                  [- int32](0, n : int32)
              |]
        , -- Numeric(int64)
          OFunction
            (builtinInstance (Trait.numeric (TIntrinsic IInt64)) "from_int32")
            [ Label Kernel.int32 "n"
            ]
            [r|
                  n : int32
              |]
        , OFunction
            (builtinInstance (Trait.numeric (TIntrinsic IInt64)) "from_int64")
            [ Label Kernel.int64 "n"
            ]
            [r|
                  n : int64
              |]
        , OFunction
            (builtinInstance (Trait.numeric (TIntrinsic IInt64)) "from_bignum")
            [ Label Kernel.bignum "n"
            ]
            [r| 
                  #(bignum_to_int64 : bignum/int64, n : bignum)(fn(m : int64) => m : int64)
              |]
        , OFunction
            (builtinInstance (Trait.numeric (TIntrinsic IInt64)) "(+)")
            [ Label Kernel.int64 "lhs"
            , Label Kernel.int64 "rhs"
            ]
            [r| 
                  [+ int64](lhs : int64, rhs : int64)
              |]
        , OFunction
            (builtinInstance (Trait.numeric (TIntrinsic IInt64)) "(-)")
            [ Label Kernel.int64 "lhs"
            , Label Kernel.int64 "rhs"
            ]
            [r| 
                  [- int64](lhs : int64, rhs : int64)
              |]
        , OFunction
            (builtinInstance (Trait.numeric (TIntrinsic IInt64)) "(*)")
            [ Label Kernel.int64 "lhs"
            , Label Kernel.int64 "rhs"
            ]
            [r| 
                  [* int64](lhs : int64, rhs : int64)
              |]
        , OFunction
            (builtinInstance (Trait.numeric (TIntrinsic IInt64)) "negate")
            [ Label Kernel.int64 "n"
            ]
            [r| 
                  [- int64](0, n : int64)
              |]
        , -- Numeric(float)
          OFunction
            (builtinInstance (Trait.numeric (TIntrinsic IFloat)) "from_int32")
            [ Label Kernel.int32 "n"
            ]
            [r|
                  #(int32_to_float : int32/float, n : int32) (fn(f : float) => f : float)
              |]
        , OFunction
            (builtinInstance (Trait.numeric (TIntrinsic IFloat)) "from_int64")
            [ Label Kernel.int64 "n"
            ]
            [r|
                  #(int64_to_float : int64/float, n : int64) (fn(f : float) => f : float)
              |]
        , OFunction
            (builtinInstance (Trait.numeric (TIntrinsic IFloat)) "from_bignum")
            [ Label Kernel.bignum "n"
            ]
            [r| 
                  #(bignum_to_float : bignum/float, n : bignum) (fn(m : float) => m : float)
              |]
        , OFunction
            (builtinInstance (Trait.numeric (TIntrinsic IFloat)) "(+)")
            [ Label Kernel.float "lhs"
            , Label Kernel.float "rhs"
            ]
            [r| 
                  [+ float](lhs : float, rhs : float)
              |]
        , OFunction
            (builtinInstance (Trait.numeric (TIntrinsic IFloat)) "(-)")
            [ Label Kernel.float "lhs"
            , Label Kernel.float "rhs"
            ]
            [r| 
                  [- float](lhs : float, rhs : float)
              |]
        , OFunction
            (builtinInstance (Trait.numeric (TIntrinsic IFloat)) "(*)")
            [ Label Kernel.float "lhs"
            , Label Kernel.float "rhs"
            ]
            [r| 
                  [* float](lhs : float, rhs : float)
              |]
        , OFunction
            (builtinInstance (Trait.numeric (TIntrinsic IFloat)) "negate")
            [ Label Kernel.float "f"
            ]
            [r| 
                  [neg float](f : float)
              |]
        , -- Numeric(double)
          OFunction
            (builtinInstance (Trait.numeric (TIntrinsic IDouble)) "from_int32")
            [ Label Kernel.int32 "n"
            ]
            [r|
                  #(int32_to_double : int32/double, n : int32) (fn(d : double) => d : double)
              |]
        , OFunction
            (builtinInstance (Trait.numeric (TIntrinsic IDouble)) "from_int64")
            [ Label Kernel.int64 "n"
            ]
            [r|
                  #(int64_to_double : int64/double, n : int64) (fn(d : double) => d : double)
              |]
        , OFunction
            (builtinInstance (Trait.numeric (TIntrinsic IDouble)) "from_bignum")
            [ Label Kernel.bignum "n"
            ]
            [r| 
                  #(bignum_to_double : bignum/double, n : bignum)(fn(m : double) => m : double)
              |]
        , OFunction
            (builtinInstance (Trait.numeric (TIntrinsic IDouble)) "(+)")
            [ Label Kernel.double "lhs"
            , Label Kernel.double "rhs"
            ]
            [r| 
                  [+ double](lhs : double, rhs : double)
              |]
        , OFunction
            (builtinInstance (Trait.numeric (TIntrinsic IDouble)) "(-)")
            [ Label Kernel.double "lhs"
            , Label Kernel.double "rhs"
            ]
            [r| 
                  [- double](lhs : double, rhs : double)
              |]
        , OFunction
            (builtinInstance (Trait.numeric (TIntrinsic IDouble)) "(*)")
            [ Label Kernel.double "lhs"
            , Label Kernel.double "rhs"
            ]
            [r| 
                  [* double](lhs : double, rhs : double)
              |]
        , OFunction
            (builtinInstance (Trait.numeric (TIntrinsic IDouble)) "negate")
            [ Label Kernel.double "d"
            ]
            [r| 
                  [neg double](d : double)
              |]
        , -- Numeric(nat)
          OFunction
            (builtinInstance (Trait.numeric (TIntrinsic INat)) "from_int32")
            [ Label Kernel.int32 "m"
            ]
            [r| 
                  @<$Nat>
                    ( `Builtin$.nat$_pack` : int32/$Nat
                    , m : int32
                    )
              |]
        , OFunction
            -- NOTE: Numbers larger than INT32_MAX are truncated
            (builtinInstance (Trait.numeric (TIntrinsic INat)) "from_int64")
            [ Label Kernel.int64 "m"
            ]
            [r| 
                  @<$Nat>
                    ( `Builtin$.nat$_pack` : int32/$Nat
                    , m : int64
                    )
              |]
        , OFunction
            (builtinInstance (Trait.numeric (TIntrinsic INat)) "from_bignum")
            [ Label Kernel.bignum "n"
            ]
            [r| 
                  #(bignum_to_int32 : bignum/int32, n : bignum) (fn(m : int32) => 
                    @<$Nat>
                      ( `Builtin$.nat$_pack` : int32/$Nat
                      , m : int32
                      ))
              |]
        , OFunction
            (builtinInstance (Trait.numeric (TIntrinsic INat)) "(+)")
            [ Label (Kernel.TCon "$Nat" []) "lhs"
            , Label (Kernel.TCon "$Nat" []) "rhs"
            ]
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
        , OFunction
            (builtinInstance (Trait.numeric (TIntrinsic INat)) "(-)")
            [ Label (Kernel.TCon "$Nat" []) "lhs"
            , Label (Kernel.TCon "$Nat" []) "rhs"
            ]
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
        , OFunction
            (builtinInstance (Trait.numeric (TIntrinsic INat)) "(*)")
            [ Label (Kernel.TCon "$Nat" []) "lhs"
            , Label (Kernel.TCon "$Nat" []) "rhs"
            ]
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
        , OFunction
            (builtinInstance (Trait.numeric (TIntrinsic INat)) "negate")
            [ Label (Kernel.TCon "$Nat" []) "_"
            ]
            [r| 
                  $Zero : $Nat
              |]
        , -- Numeric(bignum)
          OFunction
            (builtinInstance (Trait.numeric (TIntrinsic IBignum)) "from_int32")
            [ Label Kernel.int32 "n"
            ]
            [r|
                  #(int32_to_bignum : int32/bignum, n : int32) (fn(a : *) => a : *)
              |]
        , OFunction
            (builtinInstance (Trait.numeric (TIntrinsic IBignum)) "from_int64")
            [ Label Kernel.int64 "n"
            ]
            [r|
                  #(int64_to_bignum : int64/bignum, n : int64) (fn(a : *) => a : *)
              |]
        , OFunction
            (builtinInstance (Trait.numeric (TIntrinsic IBignum)) "from_bignum")
            [ Label Kernel.bignum "n"
            ]
            [r| 
                  n : bignum
              |]
        , OFunction
            (builtinInstance (Trait.numeric (TIntrinsic IBignum)) "(+)")
            [ Label Kernel.bignum "p"
            , Label Kernel.bignum "q"
            ]
            [r| 
                  #(bignum_add : bignum/bignum/bignum, p : bignum, q : bignum) (fn(r : bignum) => r : bignum)
              |]
        , OFunction
            (builtinInstance (Trait.numeric (TIntrinsic IBignum)) "(-)")
            [ Label Kernel.bignum "p"
            , Label Kernel.bignum "q"
            ]
            [r| 
                  #(bignum_sub : bignum/bignum/bignum, p : bignum, q : bignum) (fn(r : bignum) => r : bignum)
              |]
        , OFunction
            (builtinInstance (Trait.numeric (TIntrinsic IBignum)) "(*)")
            [ Label Kernel.bignum "p"
            , Label Kernel.bignum "q"
            ]
            [r| 
                  #(bignum_mul : bignum/bignum/bignum, p : bignum, q : bignum) (fn(r : bignum) => r : bignum)
              |]
        , OFunction
            (builtinInstance (Trait.numeric (TIntrinsic IBignum)) "negate")
            [ Label Kernel.bignum "p"
            ]
            [r| 
                  #(bignum_neg : bignum/bignum, p : bignum) (fn(r : bignum) => r : bignum)
              |]
        , -- /
          OFunction
            "Builtin$.compare"
            [ Label (Kernel.TCon "Ordered" [opaque]) "$a"
            ]
            [r| 
                  match<*/*/Ordering>($a : Ordered(*)) {
                    | ( $Record : { compare : */*/Ordering | * }/Ordered(*)
                      , $r : { compare : */*/Ordering | * }
                      ) =>
                        select
                          { compare = $f : */*/Ordering | _ : * } =
                            $r : { compare : */*/Ordering | * }
                          in
                            $f : */*/Ordering
                  }
              |]
        , OFunction
            "Builtin$.(^)"
            [ Label (Kernel.TCon "Numeric" [opaque]) "$a"
            , Label opaque "m"
            , Label (Kernel.TCon "$Nat" []) "n"
            ]
            [r| 
                  match<*>($a : Numeric(*)) {
                    | ( $Record : { `(*)` : */*/* | from_int32 : int32/* | * }/Numeric(*)
                      , $r : { `(*)` : */*/* | from_int32 : int32/* | * }
                      ) =>
                        select
                          { `(*)` = $f : */*/* | q : { from_int32 : int32/* | * } } =
                            $r : { `(*)` : */*/* | from_int32 : int32/* | * }
                          in
                            select
                              { from_int32 = $g : int32/* | _ : * } =
                                q : { from_int32 : int32/* | * }
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
        , OFunction
            "Builtin$.(<)"
            [ Label (Kernel.TCon "Ordered" [opaque]) "$a"
            , Label opaque "x"
            , Label opaque "y"
            ]
            [r| 
                  match<bool>($a : Ordered(*)) {
                    | ( $Record : { compare : */*/Ordering | * }/Ordered(*)
                      , $r : { compare : */*/Ordering | * }
                      ) =>
                        select
                          { compare = $f : */*/Ordering | _ : * } =
                            $r : { compare : */*/Ordering | * }
                          in
                            match<bool>(@<Ordering>($f : */*/Ordering, x : *, y : *)) {
                              | ( EqualTo : Ordering ) => false
                              | ( GreaterThan : Ordering ) => false
                              | ( LessThan : Ordering ) => true
                            }
                  }
              |]
        , OFunction
            "Builtin$.(<=)"
            [ Label (Kernel.TCon "Ordered" [opaque]) "$a"
            , Label opaque "x"
            , Label opaque "y"
            ]
            [r| 
                  match<bool>($a : Ordered(*)) {
                    | ( $Record : { compare : */*/Ordering | * }/Ordered(*)
                      , $r : { compare : */*/Ordering | * }
                      ) =>
                        select
                          { compare = $f : */*/Ordering | _ : * } =
                            $r : { compare : */*/Ordering | * }
                          in
                            match<bool>(@<Ordering>($f : */*/Ordering, x : *, y : *)) {
                              | ( EqualTo : Ordering ) => true
                              | ( GreaterThan : Ordering ) => false
                              | ( LessThan : Ordering ) => true
                            }
                  }
              |]
        , OFunction
            "Builtin$.(>)"
            [ Label (Kernel.TCon "Ordered" [opaque]) "$a"
            , Label opaque "x"
            , Label opaque "y"
            ]
            [r| 
                  match<bool>($a : Ordered(*)) {
                    | ( $Record : { compare : */*/Ordering | * }/Ordered(*)
                      , $r : { compare : */*/Ordering | * }
                      ) =>
                        select
                          { compare = $f : */*/Ordering | _ : * } =
                            $r : { compare : */*/Ordering | * }
                          in
                            match<bool>(@<Ordering>($f : */*/Ordering, x : *, y : *)) {
                              | ( EqualTo : Ordering ) => false
                              | ( GreaterThan : Ordering ) => true
                              | ( LessThan : Ordering ) => false
                            }
                  }
              |]
        , OFunction
            "Builtin$.(>=)"
            [ Label (Kernel.TCon "Ordered" [opaque]) "$a"
            , Label opaque "x"
            , Label opaque "y"
            ]
            [r| 
                  match<bool>($a : Ordered(*)) {
                    | ( $Record : { compare : */*/Ordering | * }/Ordered(*)
                      , $r : { compare : */*/Ordering | * }
                      ) =>
                        select
                          { compare = $f : */*/Ordering | _ : * } =
                            $r : { compare : */*/Ordering | * }
                          in
                            match<bool>(@<Ordering>($f : */*/Ordering, x : *, y : *)) {
                              | ( EqualTo : Ordering ) => true
                              | ( GreaterThan : Ordering ) => true
                              | ( LessThan : Ordering ) => false
                            }
                  }
              |]
        , OFunction
            (builtinInstance (Trait.ordered (TIntrinsic IInt32)) "compare")
            [ Label Kernel.int32 "x"
            , Label Kernel.int32 "y"
            ]
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
        , OFunction
            (builtinInstance (Trait.ordered (TIntrinsic IInt64)) "compare")
            [ Label Kernel.int64 "x"
            , Label Kernel.int64 "y"
            ]
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
        , OFunction
            (builtinInstance (Trait.ordered (TIntrinsic IFloat)) "compare")
            [ Label Kernel.float "x"
            , Label Kernel.float "y"
            ]
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
        , OFunction
            (builtinInstance (Trait.ordered (TIntrinsic IDouble)) "compare")
            [ Label Kernel.double "x"
            , Label Kernel.double "y"
            ]
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
        , OFunction
            (builtinInstance (Trait.ordered (TIntrinsic INat)) "compare")
            [ Label (Kernel.TCon "$Nat" []) "x"
            , Label (Kernel.TCon "$Nat" []) "y"
            ]
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
                <> builtinInstance (Trait.ordered (TIntrinsic IInt32)) "compare"
                <> [r|` : int32/int32/Ordering
                              , a : int32
                              , b : int32 )
              |]
            )
        , OFunction
            (builtinInstance (Trait.ordered (TIntrinsic IBool)) "compare")
            [ Label Kernel.bool "x"
            , Label Kernel.bool "y"
            ]
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
        , OFunction
            (builtinInstance (Trait.ordered (TIntrinsic IChar)) "compare")
            [ Label Kernel.char "x"
            , Label Kernel.char "y"
            ]
            ( [r| 
                  @<Ordering>
                    ( `|]
                <> builtinInstance (Trait.ordered (TIntrinsic IInt32)) "compare"
                <> [r|` : int32/int32/Ordering
                    , @<int32>(`Builtin$.char$_ord` : char/int32, x : char) 
                    , @<int32>(`Builtin$.char$_ord` : char/int32, y : char)
                    )
              |]
            )
        , OFunction
            (builtinInstance (Trait.ordered (TIntrinsic IString)) "compare")
            [ Label Kernel.string "s1"
            , Label Kernel.string "s2"
            ]
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
                        match<Ordering>(xs : list(char)) {
                          | ( $Nil : list(char)
                            ) =>
                              match<Ordering>(ys : list(char)) {
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
                              match<Ordering>(ys : list(char)) {
                                | ( $Nil : list(char)
                                  ) =>
                                    GreaterThan : Ordering
                                | ( $Cons : char/list(char)/list(char)
                                  , y : char
                                  , ys1 : list(char)
                                  ) =>
                                    match<Ordering>(
                                      @<Ordering>
                                        ( `|]
                <> builtinInstance (Trait.ordered (TIntrinsic IChar)) "compare"
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
        , OFunction
            (builtinInstance (Trait.ordered (TIntrinsic IBignum)) "compare")
            [ Label Kernel.bignum "x"
            , Label Kernel.bignum "y"
            ]
            [r| 
                  #(bignum_lt : bignum/bignum/bool, x : bignum, y : bignum) (fn(is_lt : bool) => 
                    if (is_lt : bool)
                      then LessThan : Ordering
                      else 
                        #(bignum_gt : bignum/bignum/bool, x : bignum, y : bignum) (fn(is_gt : bool) =>
                          if (is_gt : bool)
                            then GreaterThan : Ordering
                            else EqualTo : Ordering))
              |]
        , OFunction
            "Builtin$.string$_length"
            [ Label Kernel.string "str"
            ]
            [r| 
                  #(string_length : string/int32, str : string) (fn(a : int32) => a : int32)
              |]
        , OFunction
            "Builtin$.string$_head_unsafe"
            [ Label Kernel.string "str"
            ]
            [r| 
                  #(string_head : string/char, str : string) (fn(a : char) => a : char)
              |]
        , OFunction
            "Builtin$.string$_tail"
            [ Label Kernel.string "str"
            ]
            [r| 
                  #(string_tail : string/string, str : string) (fn(a : string) => a : string)
              |]
        , OFunction
            "Builtin$.string$_reverse"
            [ Label Kernel.string "str"
            ]
            [r| 
                  #(string_reverse : string/string, str : string) (fn(a : string) => a : string)
              |]
        , OFunction
            "Builtin$.string$_remove_whitespace"
            [ Label Kernel.string "str"
            ]
            [r| 
                  #(string_remove_whitespace : string/string, str : string) (fn(a : string) => a : string)
              |]
        , OFunction
            "Builtin$.string$_from_list"
            [ Label (Kernel.TCon "List" [char]) "chars"
            ]
            [r| 
                  let
                    f : list(char)/string/string =
                      fn(chars : list(char), result : string) =>
                        match<string>(chars : list(char)) {
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
        , OFunction
            "Builtin$.string$_to_list"
            [ Label Kernel.string "str"
            ]
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
        , OFunction
            "Builtin$.(==)"
            [ Label (Kernel.TCon "Comparable" [opaque]) "$a"
            ]
            [r| 
                  match<*/*/bool>($a : Comparable(*)) {
                    | ( $Record : { `(==)` : */*/bool | * }/Comparable(*)
                      , $r : { `(==)` : */*/bool | * }
                      ) =>
                        select
                          { `(==)` = $f : */*/bool | _ : * } =
                            $r : { `(==)` : */*/bool | * }
                          in
                            $f : */*/bool
                  }
              |]
        , OFunction
            (builtinInstance (Trait.comparable (TIntrinsic IInt32)) "(==)")
            [ Label Kernel.int32 "x"
            , Label Kernel.int32 "y"
            ]
            [r| 
                  if ([== int32](x : int32, y : int32)) then true else false 
              |]
        , OFunction
            (builtinInstance (Trait.comparable (TIntrinsic IInt64)) "(==)")
            [ Label Kernel.int64 "x"
            , Label Kernel.int64 "y"
            ]
            [r| 
                  if ([== int64](x : int64, y : int64)) then true else false 
              |]
        , OFunction
            (builtinInstance (Trait.comparable (TIntrinsic IFloat)) "(==)")
            [ Label Kernel.float "x"
            , Label Kernel.float "y"
            ]
            [r| 
                  if ([== float](x : float, y : float)) then true else false 
              |]
        , OFunction
            (builtinInstance (Trait.comparable (TIntrinsic IDouble)) "(==)")
            [ Label Kernel.double "x"
            , Label Kernel.double "y"
            ]
            [r| 
                  if ([== double](x : double, y : double)) then true else false 
              |]
        , OFunction
            (builtinInstance (Trait.comparable (TIntrinsic IBool)) "(==)")
            [ Label Kernel.bool "x"
            , Label Kernel.bool "y"
            ]
            [r| 
                  if ([== bool](x : bool, y : bool)) then true else false 
              |]
        , OFunction
            (builtinInstance (Trait.comparable (TIntrinsic IChar)) "(==)")
            [ Label Kernel.char "x"
            , Label Kernel.char "y"
            ]
            [r| 
                  if ([== char](x : char, y : char)) then true else false 
              |]
        , OFunction
            (builtinInstance (Trait.comparable (TIntrinsic INat)) "(==)")
            [ Label (Kernel.TCon "$Nat" []) "x"
            , Label (Kernel.TCon "$Nat" []) "y"
            ]
            [r| 
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
                            if ([== int32](a : int32, b : int32)) then true else false 
              |]
        , OFunction
            (builtinInstance (Trait.comparable (TIntrinsic IString)) "(==)")
            [ Label Kernel.string "str1"
            , Label Kernel.string "str2"
            ]
            [r| 
                  #(string_compare : string/string/bool, str1 : string, str2 : string) (fn(r : bool) => r : bool)
              |]
        , OFunction
            (builtinInstance (Trait.comparable (TIntrinsic IBignum)) "(==)")
            [ Label Kernel.bignum "m"
            , Label Kernel.bignum "n"
            ]
            [r| 
                  #(bignum_eq : bignum/bignum/bool, m : bignum, n : bignum) (fn(r : bool) => r : bool)
              |]
        , OFunction
            (builtinInstance (Trait.divisible (TIntrinsic IFloat)) "(/)")
            [ Label Kernel.float "q"
            , Label Kernel.float "r"
            ]
            [r| 
                  [/ float](q : float, r : float)
              |]
        , OFunction
            (builtinInstance (Trait.divisible (TIntrinsic IDouble)) "(/)")
            [ Label Kernel.double "q"
            , Label Kernel.double "r"
            ]
            [r| 
                  [/ double](q : double, r : double)
              |]
        , OFunction
            (builtinInstance (Trait.modulo (TIntrinsic IInt32)) "(%)")
            [ Label Kernel.int32 "q"
            , Label Kernel.int32 "r"
            ]
            [r| 
                  #(int32_mod : int32/int32/int32, q : int32, r : int32) (fn(s : int32) => s : int32)
              |]
        , OFunction
            (builtinInstance (Trait.modulo (TIntrinsic IInt64)) "(%)")
            [ Label Kernel.int64 "q"
            , Label Kernel.int64 "r"
            ]
            [r| 
                  #(int64_mod : int64/int64/int64, q : int64, r : int64) (fn(s : int64) => s : int64)
              |]
        , OFunction
            (builtinInstance (Trait.modulo (TIntrinsic IBignum)) "(%)")
            [ Label Kernel.bignum "q"
            , Label Kernel.bignum "r"
            ]
            [r| 
                  #(bignum_mod : bignum/bignum/bignum, q : bignum, r : bignum) (fn(s : bignum) => s : bignum)
              |]
        , OFunction
            (builtinInstance (Trait.semigroup (TIntrinsic IString)) "(<>)")
            [ Label Kernel.string "s"
            , Label Kernel.string "t"
            ]
            [r| 
                  @<string>
                    ( `Builtin$.operator$__string_concatenation` : string/string/string
                    , s : string
                    , t : string
                    )
              |]
        , OFunction
            (builtinInstance (Trait.semigroup (TApplication () (TConstructor () "List") (TVariable (Parameter () "a")))) "(<>)")
            [ Label (Kernel.TCon "List" [Kernel.TOpq]) "xs"
            , Label (Kernel.TCon "List" [Kernel.TOpq]) "ys"
            ]
            [r| 
                  @<list(*)>
                    ( `Builtin$.operator$__list_concatenation` : list(*)/list(*)/list(*)
                    , xs : list(*)
                    , ys : list(*)
                    )
              |]
        , OFunction
            "Builtin$.io$_eval"
            [ Label Kernel.TOpq "v"
            ]
            [r| 
                  v : *
              |]
        , OFunction
            "Builtin$.io$_return"
            [ Label Kernel.TOpq "v"
            ]
            [r| 
                  v : IO(*)
              |]
        , OFunction
            "Builtin$.(!=)"
            [ Label (Kernel.TCon "Comparable" [opaque]) "$c"
            , Label Kernel.TOpq "a"
            , Label Kernel.TOpq "b"
            ]
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
        , OFunction
            ( builtinInstance (Trait.comparable (TApplication () (TApplication () (TConstructor () "#Tuple2") (TVariable (Parameter () "a"))) (TVariable (Parameter () "b")))) "(==)"
            )
            [ Label (Kernel.TCon "Comparable" [opaque]) "$a"
            , Label (Kernel.TCon "Comparable" [opaque]) "$b"
            , Label (Kernel.TCon "tuple2" [Kernel.TOpq, Kernel.TOpq]) "t1"
            , Label (Kernel.TCon "tuple2" [Kernel.TOpq, Kernel.TOpq]) "t2"
            ]
            [r| 
                  match<bool>(t1 : tuple2(*,*)) {
                    | ( $Tuple2 : */*/tuple2(*,*)
                      , a1 : *
                      , b1 : *
                      ) =>
                        match<bool>(t2 : tuple2(*,*)) {
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
        , OFunction
            (builtinInstance (Trait.ordered (TApplication () (TApplication () (TConstructor () "#Tuple2") (TVariable (Parameter () "a"))) (TVariable (Parameter () "b")))) "compare")
            [ Label (Kernel.TCon "Ordered" [opaque]) "$a"
            , Label (Kernel.TCon "Ordered" [opaque]) "$b"
            , Label (Kernel.TCon "tuple2" [Kernel.TOpq, Kernel.TOpq]) "t1"
            , Label (Kernel.TCon "tuple2" [Kernel.TOpq, Kernel.TOpq]) "t2"
            ]
            [r| 
                  match<Ordering>(t1 : tuple2(*,*)) {
                    | ( $Tuple2 : */*/tuple2(*,*)
                      , a1 : *
                      , b1 : *
                      ) =>
                        match<Ordering>(t2 : tuple2(*,*)) {
                          | ( $Tuple2 : */*/tuple2(*,*)
                            , a2 : *
                            , b2 : *
                            ) =>
                              match<Ordering>(
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
        , OFunction
            "Builtin$.machine$_machine"
            [ Label Kernel.opaque "seed"
            , Label (Kernel.opaque `Kernel.arrow` Kernel.opaque `Kernel.arrow` Kernel.opaque) "transition"
            , Label (Kernel.opaque `Kernel.arrow` Kernel.opaque) "view"
            ]
            [r| 
                  let
                    step : */*/Machine(*,*,*) =
                      fn(input : *, current_state : *) =>
                        @<Machine(*,*,*)>
                          ( Machine : record({ state : * | step : */*/Machine(*,*,*) | view : */* | {} })/Machine(*,*,*)
                          , @<record({ state : * | step : */*/Machine(*,*,*) | view : */* | {} })>
                              ( $Record : { state : * | step : */*/Machine(*,*,*) | view : */* | {} }/record({ state : * | step : */*/Machine(*,*,*) | view : */* | {} })
                              , { state =
                                    @<*>
                                      ( transition : */*/*
                                      , input : *
                                      , current_state : *
                                      )
                                | step = step : */*/Machine(*,*,*)
                                | view = view : */*
                                | {}
                                }
                              )
                          )
                  in
                  @<Machine(*,*,*)>
                    ( Machine : record({ state : * | step : */*/Machine(*,*,*) | view : */* | {} })/Machine(*,*,*)
                    , @<record({ state : * | step : */*/Machine(*,*,*) | view : */* | {} })>
                        ( $Record : { state : * | step : */*/Machine(*,*,*) | view : */* | {} }/record({ state : * | step : */*/Machine(*,*,*) | view : */* | {} })
                        , { state = seed : *
                          | step = step : */*/Machine(*,*,*)
                          | view = view : */*
                          | {}
                          }
                        )
                    )
              |]
        , OFunction
            "Builtin$.machine$_map_machine"
            [ Label (Kernel.opaque `Kernel.arrow` Kernel.opaque) "f"
            , Label (Kernel.TCon "Machine" [opaque, opaque, opaque]) "m"
            ]
            [r| 
                  match<Machine(*,*,*)>(m : Machine(*,*,*)) {
                    | ( Machine : record({ state : * | step : */*/Machine(*,*,*) | view : */* | {} })/Machine(*,*,*)
                      , $r : record({ state : * | step : */*/Machine(*,*,*) | view : */* | {} })
                      ) =>
                        match<Machine(*,*,*)>($r : record({ state : * | step : */*/Machine(*,*,*) | view : */* | {} })) {
                          | ( $Record : { state : * | step : */*/Machine(*,*,*) | view : */* | {} }/record({ state : * | step : */*/Machine(*,*,*) | view : */* | {} })
                            , $row : { state : * | step : */*/Machine(*,*,*) | view : */* | {} }
                            ) =>
                              select
                                { state = $state : * | q : { step : */*/Machine(*,*,*) | view : */* | {} } } =
                                  $row : { state : * | step : */*/Machine(*,*,*) | view : */* | {} }
                                in
                                  select
                                    { step = $step : */*/Machine(*,*,*) | r : { view : */* | {} } } = 
                                      q : { step : */*/Machine(*,*,*) | view : */* | {} }
                                    in
                                      select
                                        { view = $view : */* | _ : {} } = r : { view : */* | {} }
                                        in
                                          @<Machine(*,*,*)>
                                            ( Machine : record({ state : * | step : */*/Machine(*,*,*) | view : */* | {} })/Machine(*,*,*)
                                            , @<record({ state : * | step : */*/Machine(*,*,*) | view : */* | {} })>
                                                ( $Record : { state : * | step : */*/Machine(*,*,*) | view : */* | {} }/record({ state : * | step : */*/Machine(*,*,*) | view : */* | {} })
                                                , { state = $state : *
                                                  | step = 
                                                      fn(inp : *, s : *) =>
                                                        @<Machine(*,*,*)>
                                                          ( `Builtin$.machine$_map_machine` : (*/*)/Machine(*,*,*)/Machine(*,*,*)
                                                          , f : */*
                                                          , @<Machine(*,*,*)>
                                                              ( $step : */*/Machine(*,*,*)
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

        , OFunction
            "Builtin$.machine$_contramap_input"
            [ Label (Kernel.opaque `Kernel.arrow` Kernel.opaque) "f"
            , Label (Kernel.TCon "Machine" [opaque, opaque, opaque]) "m"
            ]
            [r| 
                  match<Machine(*,*,*)>(m : Machine(*,*,*)) {
                    | ( Machine : record({ state : * | step : */*/Machine(*,*,*) | view : */* | {} })/Machine(*,*,*)
                      , $r : record({ state : * | step : */*/Machine(*,*,*) | view : */* | {} })
                      ) =>
                        match<Machine(*,*,*)>($r : record({ state : * | step : */*/Machine(*,*,*) | view : */* | {} })) {
                          | ( $Record : { state : * | step : */*/Machine(*,*,*) | view : */* | {} }/record({ state : * | step : */*/Machine(*,*,*) | view : */* | {} })
                            , $row : { state : * | step : */*/Machine(*,*,*) | view : */* | {} }
                            ) =>
                              select
                                { state = $state : * | q : { step : */*/Machine(*,*,*) | view : */* | {} } } =
                                  $row : { state : * | step : */*/Machine(*,*,*) | view : */* | {} }
                                in
                                  select
                                    { step = $step : */*/Machine(*,*,*) | r : { view : */* | {} } } = 
                                      q : { step : */*/Machine(*,*,*) | view : */* | {} }
                                    in
                                      select
                                        { view = $view : */* | _ : {} } = r : { view : */* | {} }
                                        in
                                          @<Machine(*,*,*)>
                                            ( Machine : record({ state : * | step : */*/Machine(*,*,*) | view : */* | {} })/Machine(*,*,*)
                                            , @<record({ state : * | step : */*/Machine(*,*,*) | view : */* | {} })>
                                                ( $Record : { state : * | step : */*/Machine(*,*,*) | view : */* | {} }/record({ state : * | step : */*/Machine(*,*,*) | view : */* | {} })
                                                , { state = $state : *
                                                  | step = 
                                                     fn(inp : *, s : *) =>
                                                       @<Machine(*,*,*)>
                                                         ( $step : */*/Machine(*,*,*)
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
        , OFunction
            "Builtin$.machine$_compose"
            [ Label (Kernel.TCon "Machine" [opaque, opaque, opaque]) "m1"
            , Label (Kernel.TCon "Machine" [opaque, opaque, opaque]) "m2"
            ]
            [r| 


              |]
        , OFunction
            "Builtin$.process$_process"
            [ Label Kernel.opaque "seed"
            , Label (Kernel.opaque `Kernel.arrow` Kernel.opaque `Kernel.arrow` Kernel.opaque) "f"
            ]
            [r| 
                  let
                    step : */*/Process(*,*) =
                      fn(v : *, a2 : *) =>
                        @<Process(*,*)>
                          ( Process : record({ state : * | step : */*/Process(*,*) | {} })/Process(*,*)
                          , @<record({ state : * | step : */*/Process(*,*) | {} })>
                              ( $Record : { state : * | step : */*/Process(*,*) | {} }/record({ state : * | step : */*/Process(*,*) | {} })
                              , { state = 
                                    @<*>
                                      ( f : */*/*
                                      , v : *
                                      , a2 : *
                                      )
                                | step = step : */*/Process(*,*)
                                | {}
                                }
                              )
                          )
                  in
                  @<Process(*,*)>
                    ( Process : record({ state : * | step : */*/Process(*,*) | {} })/Process(*,*)
                    , @<record({ state : * | step : */*/Process(*,*) | {} })>
                        ( $Record : { state : * | step : */*/Process(*,*) | {} }/record({ state : * | step : */*/Process(*,*) | {} })
                        , { state = seed : *
                          | step = step : */*/Process(*,*)
                          | {}
                          }
                        )
                    )
              |]
        , OFunction
            "Builtin$.process$_map_process"
            [ Label (Kernel.opaque `Kernel.arrow` Kernel.opaque) "f"
            , Label (Kernel.TCon "Process" [opaque, opaque]) "p"
            ]
            [r| 
                  match<Process(*,*)>(p : Process(*,*)) {
                    | ( Process : record({ state : * | step : */*/Process(*,*) | {} })/Process(*,*)
                      , $r : record({ state : * | step : */*/Process(*,*) | {} })
                      ) =>
                        match<Process(*,*)>($r : record({ state : * | step : */*/Process(*,*) | {} })) {
                          | ( $Record : { state : * | step : */*/Process(*,*) | {} }/record({ state : * | step : */*/Process(*,*) | {} })
                            , $row : { state : * | step : */*/Process(*,*) | {} }
                            ) =>
                              select
                                { state = $state : * | q : { step : */*/Process(*,*) | {} } } =
                                  $row : { state : * | step : */*/Process(*,*) | {} }
                                in
                                  select
                                    { step = $step : */*/Process(*,*) | _ : {} } = 
                                      q : { step : */*/Process(*,*) | {} }
                                    in
                                    @<Process(*,*)>
                                      ( Process : record({ state : * | step : */*/Process(*,*) | {} })/Process(*,*)
                                      , @<record({ state : * | step : */*/Process(*,*) | {} })>
                                          ( $Record : { state : * | step : */*/Process(*,*) | {} }/record({ state : * | step : */*/Process(*,*) | {} })
                                          , { state = 
                                                @<*>
                                                  ( f : */*
                                                  , $state : *
                                                  )
                                            | step = 
                                                fn(v : *, _ : *) =>
                                                  @<Process(*,*)>
                                                    ( `Builtin$.process$_map_process` : (*/*)/Process(*,*)/Process(*,*)
                                                    , f : */*
                                                    , @<Process(*,*)>
                                                        ( $step : */*/Process(*,*)
                                                        , v : *
                                                        , $state : *
                                                        )
                                                    )
                                            | {}
                                            }
                                          )
                                      )
                        }
                  }
              |]
        , OFunction
            "Builtin$.process$_contramap_input"
            [ Label (Kernel.opaque `Kernel.arrow` Kernel.opaque) "f"
            , Label (Kernel.TCon "Process" [opaque, opaque]) "p"
            ]
            [r| 
                  match<Process(*,*)>(p : Process(*,*)) {
                    | ( Process : record({ state : * | step : */*/Process(*,*) | {} })/Process(*,*)
                      , $r : record({ state : * | step : */*/Process(*,*) | {} })
                      ) =>
                        match<Process(*,*)>($r : record({ state : * | step : */*/Process(*,*) | {} })) {
                          | ( $Record : { state : * | step : */*/Process(*,*) | {} }/record({ state : * | step : */*/Process(*,*) | {} })
                            , $row : { state : * | step : */*/Process(*,*) | {} }
                            ) =>
                              select
                                { state = $state : * | q : { step : */*/Process(*,*) | {} } } =
                                  $row : { state : * | step : */*/Process(*,*) | {} }
                                in
                                  select
                                    { step = $step : */*/Process(*,*) | _ : {} } = 
                                      q : { step : */*/Process(*,*) | {} }
                                    in
                                    @<Process(*,*)>
                                      ( Process : record({ state : * | step : */*/Process(*,*) | {} })/Process(*,*)
                                      , @<record({ state : * | step : */*/Process(*,*) | {} })>
                                          ( $Record : { state : * | step : */*/Process(*,*) | {} }/record({ state : * | step : */*/Process(*,*) | {} })
                                          , { state = $state : *
                                            | step = 
                                                fn(v : *, a : *) =>
                                                  @<Process(*,*)>
                                                    ( `Builtin$.process$_contramap_input` : (*/*)/Process(*,*)/Process(*,*)
                                                    , f : */*
                                                    , @<Process(*,*)>
                                                        ( $step : */*/Process(*,*)
                                                        , @<*>
                                                            ( f : */*
                                                            , v : *
                                                            )
                                                        , a : *
                                                        )
                                                    )
                                            | {}
                                            }
                                          )
                                      )
                        }
                  }
              |]
        , OFunction
            "Builtin$.process$_duplicate"
            [ Label (Kernel.TCon "Process" [opaque, opaque]) "p"
            ]
            [r| 
                  let
                    step : */Process(*,*)/Process(Process(*,*),*) =
                      fn(v : *, p2 : Process(*,*)) =>
                        match<Process(*,*)>(p2 : Process(*,*)) {
                          | ( Process : record({ state : * | step : */*/Process(*,*) | {} })/Process(*,*)
                            , $r : record({ state : * | step : */*/Process(*,*) | {} })
                            ) =>
                              match<Process(*,*)>($r : record({ state : * | step : */*/Process(*,*) | {} })) {
                                | ( $Record : { state : * | step : */*/Process(*,*) | {} }/record({ state : * | step : */*/Process(*,*) | {} })
                                  , $row : { state : * | step : */*/Process(*,*) | {} }
                                  ) =>
                                    select
                                      { step = $step : */*/Process(*,*) | _ : { state : * | {} } } = 
                                        $row : { state : * | step : */*/Process(*,*) | {} }
                                      in
                                      @<Process(Process(*,*),*)>
                                        ( `Builtin$.process$_duplicate` : Process(*,*)/Process(Process(*,*),*)
                                        , @<Process(Process(*,*),*)>
                                            ( $step : */*/Process(*,*)
                                            , v : *
                                            , p2 : Process(*,*)
                                            )
                                        )
                              }
                        }
                  in
                  @<Process(Process(*,*),*)>
                    ( Process : record({ state : * | step : */*/Process(*,*) | {} })/Process(*,*)
                    , @<record({ state : * | step : */*/Process(*,*) | {} })>
                        ( $Record : { state : * | step : */*/Process(*,*) | {} }/record({ state : * | step : */*/Process(*,*) | {} })
                        , { state = p : Process(*,*)
                          | step = step : */Process(*,*)/Process(Process(*,*),*)
                          | {}
                          }
                        )
                    )
              |]
        ]
    }

unsafeParseKernelExpr :: Text -> Kernel.Expr Kernel.Type
unsafeParseKernelExpr t =
  case runParser expr "" (Text.stripStart t) of
    Left e ->
      error (errorBundlePretty e)
    Right a ->
      a
