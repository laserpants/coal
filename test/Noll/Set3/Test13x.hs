{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Noll.Set3.Test13x where

import Data.Text (Text)
import Lang.Label (Label (..))
import Lang.Lowpass.Language
import Lang.Lowpass.Parser.Expr (expr)
import Lang.Utils (Name, (<$$>))
import Text.Megaparsec (eof, runParser)
import Text.Megaparsec.Error (errorBundlePretty)
import Text.RawString.QQ

import qualified Data.Text as Text
import qualified Lang.Lowpass.Compiler as Lowpass
import qualified Lang.Lowpass.Compiler.Utils as Lowpass
import qualified Lang.Lowpass.Language as Lowpass

unsafeParseExpr :: Text -> Lowpass.Expr Lowpass.Type
unsafeParseExpr t =
  case runParser expr "" (Text.stripStart t) of
    Left e ->
      error (errorBundlePretty e)
    Right r ->
      r

moduleCore1 :: Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type)
moduleCore1 = unsafeParseExpr <$> moduleCore

moduleCore :: Module Lowpass.Type Name Text
moduleCore =
  Module
    { moduleName = "Core$"
    , moduleImports =
        []
    , moduleObjects =
        [ OFunction
            "Core$.operator__not"
            [ Label bool "a"
            ]
            [r| 
                  if (a : bool) then false else true
              |]
        , OFunction
            "Core$.operator__reverse_composition"
            [ Label (opaque `arrow` opaque) "f"
            , Label (opaque `arrow` opaque) "g"
            , Label opaque "x"
            ]
            [r| 
                  @<*>(f : */*, @<*>(g : */*, x : *))
              |]
        , OFunction
            "Core$.operator__reverse_application"
            [ Label opaque "x"
            , Label (opaque `arrow` opaque) "f"
            ]
            [r| 
                  @<*>(f : */*, x : *)
              |]
        , OFunction
            "Core$.always"
            [ Label opaque "a"
            , Label opaque "_"
            ]
            [r|   
                  a : *
              |]
        , OFunction
            "Core$.operator__list_concatenation"
            [ Label (TCon "list" [opaque]) "xs"
            , Label (TCon "list" [opaque]) "ys"
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
                              ( Core$.operator__list_concatenation : list(*)/list(*)/list(*)
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
            "Core$.trace_int32"
            [ Label int32 "n"
            ]
            [r|
                  #(print_int32 : int32/*, n : int32) (fn(a : *) => a : *)
              |]
        , OFunction
            "Core$.trace_string"
            [ Label string "s"
            ]
            [r|
                  #(print_string : string/*, s : string) (fn(a : *) => a : *)
              |]
        , OFunction
            "Core$.operator__string_concatenation"
            [ Label string "s"
            , Label string "t"
            ]
            [r|
                  #(string_concat : string/string/string, s : string, t : string) (fn(r : string) => r : string)
              |]
        , OFunction
            "Core$.int32_to_string"
            [ Label int32 "n"
            ]
            [r| 
                  #(int32_to_string : int32/string, n : int32) (fn(r : string) => r : string)
              |]
        , OFunction
            "Core$.pair_to_string"
            [ Label (TCon "Traceable" [TOpq]) "$dict1"
            , Label (TCon "Traceable" [TOpq]) "$dict2"
            , Label (TCon "$Tuple2" [TOpq, TOpq]) "p"
            ]
            [r| 
                  match<string>
                    ( p : $Tuple2(*,*) ) { 
                      | ( $Tuple2 : */*/$Tuple2(*,*)
                        , a : *
                        , b : *
                        ) =>
                          @<string>
                            ( Core$.operator__string_concatenation : string/string/string
                            , @<string>
                                ( Core$.operator__string_concatenation : string/string/string
                                , "("
                                , @<string>
                                    ( Core$.operator__string_concatenation : string/string/string
                                    , @<string>
                                        ( Core$.operator__string_concatenation : string/string/string
                                        , @<string>
                                            ( Core$.trace : Traceable(*)/*/string
                                            , $dict1 : Traceable(*)
                                            , a : *
                                            )
                                        , ","
                                        )
                                    , @<string>
                                        ( Core$.trace : Traceable(*)/*/string
                                        , $dict2 : Traceable(*)
                                        , b : *
                                        )
                                    )
                                )
                            , ")"
                            )
                    }
              |]
        , OFunction
            "Core$.list_to_string"
            [ Label (TCon "Traceable" [TOpq]) "$dict1"
            , Label (TCon "list" [TOpq]) "ls"
            ]
            [r| 
                  let
                    f : bool/list(*)/string =
                      fn(first : bool, l : list(*)) =>
                        match<string>
                          ( l : list(*)
                          ) {
                            | ( $Cons : */list(*)/list(*)
                              , x : *
                              , xs : list(*)
                              ) =>
                                @<string>
                                  ( Core$.operator__string_concatenation : string/string/string
                                  , if (first : bool) then "" else ","
                                  , @<string>
                                      ( Core$.operator__string_concatenation : string/string/string
                                      , @<string>
                                          ( Core$.trace : Traceable(*)/*/string
                                          , $dict1 : Traceable(*)
                                          , x : *
                                          )
                                      , @<string>
                                          ( f : list(*)/string
                                          , false
                                          , xs : list(*)
                                          )
                                      )
                                  )
                            | ( $Nil : list(*)
                              ) =>
                                ""
                          }
                    in
                      @<string>
                        ( Core$.operator__string_concatenation : string/string/string
                        , @<string>
                            ( Core$.operator__string_concatenation : string/string/string
                            , "["
                            , @<string>
                                ( f : list(*)/string
                                , true
                                , ls : list(*)
                                )
                            )
                        , "]"
                        )
              |]
        , OFunction
            "Core$.trace"
            [ Label (TCon "Traceable" [opaque]) "$a"
            ]
            [r| 
                  match<*>($a : Traceable(*)) {
                    | ( $Record : { trace : * | * }/Traceable(*)
                      , $r : { trace : * | * }
                      ) =>
                        select
                          { trace = $f : * | _ : * } =
                            $r : { trace : * | * }
                          in
                            $f : *
                  }
              |]
        ]
    }

moduleMain1 :: Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type)
moduleMain1 = unsafeParseExpr <$> moduleMain

moduleMain :: Module Lowpass.Type Name Text
moduleMain =
  Module
    { moduleName = "Main"
    , moduleImports =
        [ "Core$.int32_to_string"
        , "Core$.list_to_string"
        , "Core$.pair_to_string"
        , "Core$.trace"
        , "Core$.trace_string"
        , "Core$.operator__not"
        , "Core$.operator__reverse_composition"
        , "Core$.operator__reverse_application"
        , "Core$.always"
        , "Core$.operator__list_concatenation"
        , "Core$.trace_int32"
        , "Core$.trace_string"
        , "Core$.operator__string_concatenation"
        , "Core$.int32_to_string"
        , "Core$.pair_to_string"
        , "Core$.list_to_string"
        , "Core$.trace"
        , "Core$.unpack_nat"
        , "Core$.pack_nat"
        ]
    , moduleObjects =
        [ OFunction
            "Main.trace__$instance.c81d5162b7d14248"
            [ Label string "s"
            ]
            [r| 
                  s : string
              |]
        , OFunction
            "Main.trace__$instance.c847f12006235dc0"
            [ Label int32 "n"
            ]
            [r| 
                  @<string>
                    ( Core$.int32_to_string : int32/string
                    , n : int32
                    )
              |]
        , OFunction
            "Main.trace__$instance.a2de7bde6bbaafb6"
            [ Label (TCon "Traceable" [opaque]) "$d.Traceable__$instance.46804159d1c1d0e2"
            , Label (TCon "Traceable" [opaque]) "$d.Traceable__$instance.46804159d1c1d0e3"
            , Label (TCon "tuple2" [opaque, opaque]) "p"
            ]
            [r| 
                  @<string>
                    ( @<tuple2(*,*)/string>
                        ( Core$.pair_to_string : Traceable(*)/Traceable(*)/tuple2(*,*)/string
                        , $d.Traceable__$instance.46804159d1c1d0e2 : Traceable(*)
                        , $d.Traceable__$instance.46804159d1c1d0e3 : Traceable(*)
                        )
                    , p : tuple2(*,*)
                    )
              |]
        , OFunction
            "Main.trace__$instance.fcea41ba44fb0cf4"
            [ Label (TCon "Traceable" [opaque]) "$d.Traceable__$instance.46804159d1c1d0e2"
            , Label (TCon "list" [opaque]) "lst"
            ]
            [r| 
                  @<string>
                    ( @<list(*)/string>
                        ( Core$.list_to_string : Traceable(*)/list(*)/string
                        , $d.Traceable__$instance.46804159d1c1d0e2 : Traceable(*)
                        )
                    , lst : list(*)
                    )
              |]
        , -- pair1
          OConstant
            "Main.pair1"
            [r| 
                  let
                    p : tuple2(int32, string) =
                      @<tuple2(int32, string)>
                        ( $Tuple2 : int32/string/tuple2(int32, string)
                        , 1
                        , "hello"
                        )
                    in
                       @<string>
                         ( @<tuple2(int32, string)/string>
                             ( Core$.trace : Traceable(tuple2(int32, string))/tuple2(int32, string)/string
                             , @<Traceable(tuple2(int32, string))>
                                 ( $Record : { trace : tuple2(int32, string)/string | {} }/record({ trace : tuple2(int32, string)/string | {} })
                                 , { trace = 
                                       @<tuple2(int32, string)/string>
                                         ( Main.trace__$instance.a2de7bde6bbaafb6 : Traceable(int32)/Traceable(string)/tuple2(int32, string)/string
                                         , @<Traceable(int32)>
                                             ( $Record : { trace : int32/string | {} }/record({ trace : int32/string | {} })
                                             , { trace = Main.trace__$instance.c847f12006235dc0 : int32/string
                                               | {}
                                               }
                                             )
                                         , @<Traceable(string)>
                                             ( $Record : { trace : string/string | {} }/record({ trace : string/string | {} })
                                             , { trace = Main.trace__$instance.c81d5162b7d14248 : string/string
                                               | {}
                                               }
                                             )
                                         )
                                   | {}
                                   }
                                 )
                             )
                         , p : tuple2(int32, string)
                         )
              |]
        , -- list1
          OConstant
            "Main.list1"
            [r| 
                  @<string>
                    ( @<list(tuple2(int32, string))/string>
                        ( Core$.trace : Traceable(list(tuple2(int32, string)))/list(tuple2(int32, string))/string
                        , @<Traceable(list(tuple2(int32, string)))>
                            ( $Record : { trace : tuple2(int32, string)/string | {} }/record({ trace : tuple2(int32, string)/string | {} })
                            , { trace =
                                  @<tuple2(int32, string)/string>
                                    ( Main.trace__$instance.fcea41ba44fb0cf4 : Traceable(tuple2(int32, string))/tuple2(int32, string)/string
                                    , @<Traceable(tuple2(int32, string))>
                                        ( $Record : { trace : tuple2(int32, string)/string | {} }/record({ trace : tuple2(int32, string)/string | {} })
                                        , { trace = 
                                              @<tuple2(int32, string)/string>
                                                ( Main.trace__$instance.a2de7bde6bbaafb6 : Traceable(int32)/Traceable(string)/tuple2(int32, string)/string
                                                , @<Traceable(int32)>
                                                    ( $Record : { trace : int32/string | {} }/record({ trace : int32/string | {} })
                                                    , { trace = Main.trace__$instance.c847f12006235dc0 : int32/string
                                                      | {}
                                                      }
                                                    )
                                                , @<Traceable(string)>
                                                    ( $Record : { trace : string/string | {} }/record({ trace : string/string | {} })
                                                    , { trace = Main.trace__$instance.c81d5162b7d14248 : string/string
                                                      | {}
                                                      }
                                                    )
                                                )
                                          | {}
                                          }
                                        )
                                    )
                              | {}
                              }
                            )
                        )
                    , @<list(tuple2(int32, string))>
                         ( $Cons : tuple2(int32, string)/list(tuple2(int32, string))/list(tuple2(int32, string))
                         , @<tuple2(int32, string)>
                             ( $Tuple2 : int32/string/tuple2(int32, string)
                             , 1 
                             , "a"
                             )
                         , @<list(tuple2(int32, string))>
                             ( $Cons : tuple2(int32, string)/list(tuple2(int32, string))/list(tuple2(int32, string))
                             , @<tuple2(int32, string)>
                                 ( $Tuple2 : int32/string/tuple2(int32, string)
                                 , 2
                                 , "b"
                                 )
                             , $Nil : list(tuple2(int32, string))
                             )
                         )
                    )
              |]
        , OFunction
            "Main.main"
            [Label (TCon "unit" []) "$v.0"]
            [r|
                    @<*>
                      ( Core$.trace_string : string/*
                      , Main.list1 : string
 //                      , Main.pair1 : string
                      )
              |]
        ]
    }

prog3_13x :: [Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type)]
prog3_13x = unsafeParseExpr <$$> fixture1

fixture1 :: [Module Lowpass.Type Name Text]
fixture1 =
  [ moduleMain
  ]

fixture3 :: [Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type)]
fixture3 =
  [ moduleCore1
  , moduleMain1
  ]

woozx :: IO ()
woozx = Lowpass.testModules =<< Lowpass.compileModules fixture3
