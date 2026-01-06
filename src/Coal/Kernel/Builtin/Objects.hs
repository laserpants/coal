{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Coal.Kernel.Builtin.Objects (builtinObjects) where

import Coal.Common.Label (Label (..))
import Coal.Kernel.Language
import qualified Coal.Kernel.Language as Kernel
import Coal.Kernel.Parser.Expr (expr)
import Data.Text (Text)
import qualified Data.Text as Text
import Extras (Name)
import Text.Megaparsec (errorBundlePretty, runParser)
import Text.RawString.QQ (r)

builtinObjects :: Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type)
builtinObjects = unsafeParseKernelExpr <$> objects

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
            "Builtin$.operator$__reverse_application"
            [ Label Kernel.opaque "x"
            , Label (Kernel.opaque `Kernel.arrow` Kernel.opaque) "f"
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
            "Builtin$.from_int32__$impl_Numeric(Intrinsic(Int32))"
            [ Label Kernel.int32 "n"
            ]
            [r|
                  n : int32
              |]
        , OFunction
            "Builtin$.from_int64__$impl_Numeric(Intrinsic(Int32))"
            [ Label Kernel.int64 "n"
            ]
            [r|
                  n : int64
              |]
        , OFunction
            "Builtin$.from_bignum__$impl_Numeric(Intrinsic(Int32))"
            [ Label Kernel.bignum "n"
            ]
            [r| 
                  #(bignum_to_int32 : bignum/int32, n : bignum)(fn(m : int32) => m : int32)
              |]
        , OFunction
            "Builtin$.(+)__$impl_Numeric(Intrinsic(Int32))"
            [ Label Kernel.int32 "lhs"
            , Label Kernel.int32 "rhs"
            ]
            [r| 
                  [+ int32](lhs : int32, rhs : int32)
              |]
        , OFunction
            "Builtin$.(-)__$impl_Numeric(Intrinsic(Int32))"
            [ Label Kernel.int32 "lhs"
            , Label Kernel.int32 "rhs"
            ]
            [r| 
                  [- int32](lhs : int32, rhs : int32)
              |]
        , OFunction
            "Builtin$.(*)__$impl_Numeric(Intrinsic(Int32))"
            [ Label Kernel.int32 "lhs"
            , Label Kernel.int32 "rhs"
            ]
            [r| 
                  [* int32](lhs : int32, rhs : int32)
              |]
        , OFunction
            "Builtin$.negate__$impl_Numeric(Intrinsic(Int32))"
            [ Label Kernel.int32 "n"
            ]
            [r| 
                  [- int32](0, n : int32)
              |]
        , -- Numeric(int64)
          OFunction
            "Builtin$.from_int32__$impl_Numeric(Intrinsic(Int64))"
            [ Label Kernel.int32 "n"
            ]
            [r|
                  n : int32
              |]
        , OFunction
            "Builtin$.from_int64__$impl_Numeric(Intrinsic(Int64))"
            [ Label Kernel.int64 "n"
            ]
            [r|
                  n : int64
              |]
        , OFunction
            "Builtin$.from_bignum__$impl_Numeric(Intrinsic(Int64))"
            [ Label Kernel.bignum "n"
            ]
            [r| 
                  #(bignum_to_int64 : bignum/int64, n : bignum)(fn(m : int64) => m : int64)
              |]
        , OFunction
            "Builtin$.(+)__$impl_Numeric(Intrinsic(Int64))"
            [ Label Kernel.int64 "lhs"
            , Label Kernel.int64 "rhs"
            ]
            [r| 
                  [+ int64](lhs : int64, rhs : int64)
              |]
        , OFunction
            "Builtin$.(-)__$impl_Numeric(Intrinsic(Int64))"
            [ Label Kernel.int64 "lhs"
            , Label Kernel.int64 "rhs"
            ]
            [r| 
                  [- int64](lhs : int64, rhs : int64)
              |]
        , OFunction
            "Builtin$.(*)__$impl_Numeric(Intrinsic(Int64))"
            [ Label Kernel.int64 "lhs"
            , Label Kernel.int64 "rhs"
            ]
            [r| 
                  [* int64](lhs : int64, rhs : int64)
              |]
        , OFunction
            "Builtin$.negate__$impl_Numeric(Intrinsic(Int64))"
            [ Label Kernel.int64 "n"
            ]
            [r| 
                  [- int64](0, n : int64)
              |]
        , -- Numeric(float)
          OFunction
            "Builtin$.from_int32__$impl_Numeric(Intrinsic(Float))"
            [ Label Kernel.int32 "n"
            ]
            [r|
                  #(int32_to_float : int32/float, n : int32) (fn(f : float) => f : float)
              |]
        , OFunction
            "Builtin$.from_int64__$impl_Numeric(Intrinsic(Float))"
            [ Label Kernel.int64 "n"
            ]
            [r|
                  #(int64_to_float : int64/float, n : int64) (fn(f : float) => f : float)
              |]
        , OFunction
            "Builtin$.from_bignum__$impl_Numeric(Intrinsic(Float))"
            [ Label Kernel.bignum "n"
            ]
            [r| 
                  #(bignum_to_float : bignum/float, n : bignum) (fn(m : float) => m : float)
              |]
        , OFunction
            "Builtin$.(+)__$impl_Numeric(Intrinsic(Float))"
            [ Label Kernel.float "lhs"
            , Label Kernel.float "rhs"
            ]
            [r| 
                  [+ float](lhs : float, rhs : float)
              |]
        , OFunction
            "Builtin$.(-)__$impl_Numeric(Intrinsic(Float))"
            [ Label Kernel.float "lhs"
            , Label Kernel.float "rhs"
            ]
            [r| 
                  [- float](lhs : float, rhs : float)
              |]
        , OFunction
            "Builtin$.(*)__$impl_Numeric(Intrinsic(Float))"
            [ Label Kernel.float "lhs"
            , Label Kernel.float "rhs"
            ]
            [r| 
                  [* float](lhs : float, rhs : float)
              |]
        , OFunction
            "Builtin$.negate__$impl_Numeric(Intrinsic(Float))"
            [ Label Kernel.float "f"
            ]
            [r| 
                  [neg float](f : float)
              |]
        , -- Numeric(double)
          OFunction
            "Builtin$.from_int32__$impl_Numeric(Intrinsic(Double))"
            [ Label Kernel.int32 "n"
            ]
            [r|
                  #(int32_to_double : int32/double, n : int32) (fn(d : double) => d : double)
              |]
        , OFunction
            "Builtin$.from_int64__$impl_Numeric(Intrinsic(Double))"
            [ Label Kernel.int64 "n"
            ]
            [r|
                  #(int64_to_double : int64/double, n : int64) (fn(d : double) => d : double)
              |]
        , OFunction
            "Builtin$.from_bignum__$impl_Numeric(Intrinsic(Double))"
            [ Label Kernel.bignum "n"
            ]
            [r| 
                  #(bignum_to_double : bignum/double, n : bignum)(fn(m : double) => m : double)
              |]
        , OFunction
            "Builtin$.(+)__$impl_Numeric(Intrinsic(Double))"
            [ Label Kernel.double "lhs"
            , Label Kernel.double "rhs"
            ]
            [r| 
                  [+ double](lhs : double, rhs : double)
              |]
        , OFunction
            "Builtin$.(-)__$impl_Numeric(Intrinsic(Double))"
            [ Label Kernel.double "lhs"
            , Label Kernel.double "rhs"
            ]
            [r| 
                  [- double](lhs : double, rhs : double)
              |]
        , OFunction
            "Builtin$.(*)__$impl_Numeric(Intrinsic(Double))"
            [ Label Kernel.double "lhs"
            , Label Kernel.double "rhs"
            ]
            [r| 
                  [* double](lhs : double, rhs : double)
              |]
        , OFunction
            "Builtin$.negate__$impl_Numeric(Intrinsic(Double))"
            [ Label Kernel.double "d"
            ]
            [r| 
                  [neg double](d : double)
              |]
        , -- Numeric(nat)
          OFunction
            "Builtin$.from_int32__$impl_Numeric(Intrinsic(Nat))"
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
            "Builtin$.from_int64__$impl_Numeric(Intrinsic(Nat))"
            [ Label Kernel.int64 "m"
            ]
            [r| 
                  @<$Nat>
                    ( `Builtin$.nat$_pack` : int32/$Nat
                    , m : int64
                    )
              |]
        , OFunction
            "Builtin$.from_bignum__$impl_Numeric(Intrinsic(Nat))"
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
            "Builtin$.(+)__$impl_Numeric(Intrinsic(Nat))"
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
            "Builtin$.(-)__$impl_Numeric(Intrinsic(Nat))"
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
            "Builtin$.(*)__$impl_Numeric(Intrinsic(Nat))"
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
            "Builtin$.negate__$impl_Numeric(Intrinsic(Nat))"
            [ Label (Kernel.TCon "$Nat" []) "_"
            ]
            [r| 
                  $Zero : $Nat
              |]
        , -- Numeric(bignum)
          OFunction
            "Builtin$.from_int32__$impl_Numeric(Intrinsic(Bignum))"
            [ Label Kernel.int32 "n"
            ]
            [r|
                  #(int32_to_bignum : int32/bignum, n : int32) (fn(a : *) => a : *)
              |]
        , OFunction
            "Builtin$.from_int64__$impl_Numeric(Intrinsic(Bignum))"
            [ Label Kernel.int64 "n"
            ]
            [r|
                  #(int64_to_bignum : int64/bignum, n : int64) (fn(a : *) => a : *)
              |]
        , OFunction
            "Builtin$.from_bignum__$impl_Numeric(Intrinsic(Bignum))"
            [ Label Kernel.bignum "n"
            ]
            [r| 
                  n : bignum
              |]
        , OFunction
            "Builtin$.(+)__$impl_Numeric(Intrinsic(Bignum))"
            [ Label Kernel.bignum "p"
            , Label Kernel.bignum "q"
            ]
            [r| 
                  #(bignum_add : bignum/bignum/bignum, p : bignum, q : bignum) (fn(r : bignum) => r : bignum)
              |]
        , OFunction
            "Builtin$.(-)__$impl_Numeric(Intrinsic(Bignum))"
            [ Label Kernel.bignum "p"
            , Label Kernel.bignum "q"
            ]
            [r| 
                  #(bignum_sub : bignum/bignum/bignum, p : bignum, q : bignum) (fn(r : bignum) => r : bignum)
              |]
        , OFunction
            "Builtin$.(*)__$impl_Numeric(Intrinsic(Bignum))"
            [ Label Kernel.bignum "p"
            , Label Kernel.bignum "q"
            ]
            [r| 
                  #(bignum_mul : bignum/bignum/bignum, p : bignum, q : bignum) (fn(r : bignum) => r : bignum)
              |]
        , OFunction
            "Builtin$.negate__$impl_Numeric(Intrinsic(Bignum))"
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
            "Builtin$.compare__$impl_Ordered(Intrinsic(Int32))"
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
            "Builtin$.compare__$impl_Ordered(Intrinsic(Int64))"
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
            "Builtin$.compare__$impl_Ordered(Intrinsic(Float))"
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
            "Builtin$.compare__$impl_Ordered(Intrinsic(Double))"
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
            "Builtin$.compare__$impl_Ordered(Intrinsic(Nat))"
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
                            @<Ordering>
                              ( `Builtin$.compare__$impl_Ordered(Intrinsic(Int32))` : int32/int32/Ordering
                              , a : int32
                              , b : int32 )
              |]
        , OFunction
            "Builtin$.compare__$impl_Ordered(Intrinsic(Bool))"
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
            "Builtin$.compare__$impl_Ordered(Intrinsic(Char))"
            [ Label Kernel.char "x"
            , Label Kernel.char "y"
            ]
            [r| 
                  @<Ordering>
                    ( `Builtin$.compare__$impl_Ordered(Intrinsic(Int32))` : int32/int32/Ordering
                    , @<int32>(`Builtin$.char$_ord` : char/int32, x : char) 
                    , @<int32>(`Builtin$.char$_ord` : char/int32, y : char)
                    )
              |]
        , OFunction
            "Builtin$.compare__$impl_Ordered(Intrinsic(Bignum))"
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
            "Builtin$.(==)__$impl_Comparable(Intrinsic(Int32))"
            [ Label Kernel.int32 "x"
            , Label Kernel.int32 "y"
            ]
            [r| 
                  if ([== int32](x : int32, y : int32)) then true else false 
              |]
        , OFunction
            "Builtin$.(==)__$impl_Comparable(Intrinsic(Int64))"
            [ Label Kernel.int64 "x"
            , Label Kernel.int64 "y"
            ]
            [r| 
                  if ([== int64](x : int64, y : int64)) then true else false 
              |]
        , OFunction
            "Builtin$.(==)__$impl_Comparable(Intrinsic(Float))"
            [ Label Kernel.float "x"
            , Label Kernel.float "y"
            ]
            [r| 
                  if ([== float](x : float, y : float)) then true else false 
              |]
        , OFunction
            "Builtin$.(==)__$impl_Comparable(Intrinsic(Double))"
            [ Label Kernel.double "x"
            , Label Kernel.double "y"
            ]
            [r| 
                  if ([== double](x : double, y : double)) then true else false 
              |]
        , OFunction
            "Builtin$.(==)__$impl_Comparable(Intrinsic(Bool))"
            [ Label Kernel.bool "x"
            , Label Kernel.bool "y"
            ]
            [r| 
                  if ([== bool](x : bool, y : bool)) then true else false 
              |]
        , OFunction
            "Builtin$.(==)__$impl_Comparable(Intrinsic(Char))"
            [ Label Kernel.char "x"
            , Label Kernel.char "y"
            ]
            [r| 
                  if ([== char](x : char, y : char)) then true else false 
              |]
        , OFunction
            "Builtin$.(==)__$impl_Comparable(Intrinsic(Nat))"
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
            "Builtin$.(==)__$impl_Comparable(Intrinsic(String))"
            [ Label Kernel.string "str1"
            , Label Kernel.string "str2"
            ]
            [r| 
                  #(string_compare : string/string/bool, str1 : string, str2 : string) (fn(r : bool) => r : bool)
              |]
        , OFunction
            "Builtin$.(==)__$impl_Comparable(Intrinsic(Bignum))"
            [ Label Kernel.bignum "m"
            , Label Kernel.bignum "n"
            ]
            [r| 
                  #(bignum_eq : bignum/bignum/bool, m : bignum, n : bignum) (fn(r : bool) => r : bool)
              |]
        , OFunction
            "Builtin$.(/)__$impl_Divisible(Intrinsic(Float))"
            [ Label Kernel.float "q"
            , Label Kernel.float "r"
            ]
            [r| 
                  [/ float](q : float, r : float)
              |]
        , OFunction
            "Builtin$.(/)__$impl_Divisible(Intrinsic(Double))"
            [ Label Kernel.double "q"
            , Label Kernel.double "r"
            ]
            [r| 
                  [/ double](q : double, r : double)
              |]
        , OFunction
            "Builtin$.(%)__$impl_Modulo(Intrinsic(Int32))"
            [ Label Kernel.int32 "q"
            , Label Kernel.int32 "r"
            ]
            [r| 
                  #(int32_mod : int32/int32/int32, q : int32, r : int32) (fn(s : int32) => s : int32)
              |]
        , OFunction
            "Builtin$.(%)__$impl_Modulo(Intrinsic(Int64))"
            [ Label Kernel.int64 "q"
            , Label Kernel.int64 "r"
            ]
            [r| 
                  #(int64_mod : int64/int64/int64, q : int64, r : int64) (fn(s : int64) => s : int64)
              |]
        , OFunction
            "Builtin$.(%)__$impl_Modulo(Intrinsic(Bignum))"
            [ Label Kernel.bignum "q"
            , Label Kernel.bignum "r"
            ]
            [r| 
                  #(bignum_mod : bignum/bignum/bignum, q : bignum, r : bignum) (fn(s : bignum) => s : bignum)
              |]
        , OFunction
            "Builtin$.(<>)__$impl_Semigroup(Intrinsic(String))"
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
            "Builtin$.(<>)__$impl_Semigroup(Application(Constructor(List))(Variable(Parameter(a))))"
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
        ]
    }

unsafeParseKernelExpr :: Text -> Kernel.Expr Kernel.Type
unsafeParseKernelExpr t =
  case runParser expr "" (Text.stripStart t) of
    Left e ->
      error (errorBundlePretty e)
    Right a ->
      a
