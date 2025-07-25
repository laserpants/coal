{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Noll.Set3.Test13 where

import Data.Text (Text)
import Lang.Common.Label (Label (..))
import Lang.Kernel.Language
import Lang.Kernel.Parser.Expr (expr)
import Extra (Name, (<$$>))
import Text.Megaparsec (eof, runParser)
import Text.Megaparsec.Error (errorBundlePretty)
import Text.RawString.QQ

import qualified Data.Text as Text
import qualified Lang.Kernel.Compiler as Kernel
import qualified Lang.Kernel.Compiler.Utils as Kernel
import qualified Lang.Kernel.Language as Kernel

unsafeParseExpr :: Text -> Kernel.Expr Kernel.Type
unsafeParseExpr t =
  case runParser expr "" (Text.stripStart t) of
    Left e ->
      error (errorBundlePretty e)
    Right r ->
      r

moduleCore1 :: Module Kernel.Type Name (Kernel.Expr Kernel.Type)
moduleCore1 = unsafeParseExpr <$> moduleCore

moduleCore :: Module Kernel.Type Name Text
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

moduleMain1 :: Module Kernel.Type Name (Kernel.Expr Kernel.Type)
moduleMain1 = unsafeParseExpr <$> moduleMain

moduleMain :: Module Kernel.Type Name Text
moduleMain =
  Module
    { moduleName = "Main"
    , moduleImports =
        [ "Core$.int32_to_string"
        , "Core$.list_to_string"
        , "Core$.pair_to_string"
        , "Core$.trace"
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
        ]
    , moduleObjects =
        [ OFunction
            "Main.trace__$instance.c81d5162b7d14248"
            [ Label string "s"
            ]
            [r| 
                  s : string
              |]
        , OConstant
            "Main.Traceable__$instance.c81d5162b7d14248"
            [r| 
                  @<Traceable(string)>
                    ( $Record : { trace : string/string | {} }/Traceable(string)
                    , { trace = Main.trace__$instance.c81d5162b7d14248 : string/string
                      | {}
                      }
                    )
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
        , OConstant
            "Main.Traceable__$instance.c847f12006235dc0"
            [r| 
                  @<Traceable(int32)>
                    ( $Record : { trace : int32/string | {} }/Traceable(int32)
                    , { trace = Main.trace__$instance.c847f12006235dc0 : int32/string
                      | {}
                      }
                    )
              |]
        , OFunction
            "Main.trace__$instance.a2de7bde6bbaafb6"
            [ Label (TCon "Traceable" [opaque]) "$dict1"
            , Label (TCon "Traceable" [opaque]) "$dict2"
            , Label (TCon "tuple2" [opaque, opaque]) "p"
            ]
            [r| 
                  @<string>
                    ( Core$.pair_to_string : Traceable(*)/Traceable(*)/tuple2(*,*)/string
                    , $dict1 : Traceable(*)
                    , $dict2 : Traceable(*)
                    , p : tuple2(*,*)
                    )
              |]
        , OConstant
            "Main.Traceable__$instance.a2de7bde6bbaafb6"
            [r| 
                  @<Traceable(tuple2(*,*))>
                    ( $Record : { trace : Traceable(*)/Traceable(*)/tuple2(*,*)/string | {} }/Traceable(tuple2(*,*))
                    , { trace = Main.trace__$instance.a2de7bde6bbaafb6 : Traceable(*)/Traceable(*)/tuple2(*,*)/string
                      | {}
                      }
                    )
              |]
        , OFunction
            "Main.trace__$instance.fcea41ba44fb0cf4"
            [ Label (TCon "Traceable" [opaque]) "$dict1"
            , Label (TCon "list" [opaque]) "l"
            ]
            [r| 
                  @<string>
                    ( Core$.list_to_string : Traceable(*)/tuple2(*,*)/string
                    , $dict1 : Traceable(*)
                    , l : list(*)
                    )
              |]
        , OConstant
            "Main.Traceable__$instance.fcea41ba44fb0cf4"
            [r| 
                  @<Traceable(list(*))>
                    ( $Record : { trace : Traceable(*)/list(*)/string | {} }/Traceable(list(*))
                    , { trace = Main.trace__$instance.fcea41ba44fb0cf4 : Traceable(*)/list(*)/string
                      | {}
                      }
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
                            ( Core$.trace : Traceable(tuple2(int32, string))/Traceable(int32)/Traceable(string)/tuple2(int32, string)/string
                            , Main.Traceable__$instance.a2de7bde6bbaafb6 : Traceable(tuple2(int32, string))
                            , Main.Traceable__$instance.c847f12006235dc0 : Traceable(int32)
                            , Main.Traceable__$instance.c81d5162b7d14248 : Traceable(string)
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
                        ( Core$.trace : Traceable(list(tuple2(int32, string)))/Traceable(tuple2(int32, string))/list(tuple2(int32, string))/string
                        , Main.Traceable__$instance.fcea41ba44fb0cf4 : Traceable(list(tuple2(int32, string)))
                        , @<Traceable(tuple2(int32, string))>
                            ( $Record : ({ trace : tuple2(int32, string)/string | * })/Traceable(tuple2(int32, string))
                            , { trace = 
                                  @<tuple2(int32, string)/string>
                                    ( Core$.trace : Traceable(tuple2(int32, string))/Traceable(int32)/Traceable(string)/tuple2(int32, string)/string
                                    , Main.Traceable__$instance.a2de7bde6bbaafb6 : Traceable(tuple2(int32, string))
                                    , Main.Traceable__$instance.c847f12006235dc0 : Traceable(int32)
                                    , Main.Traceable__$instance.c81d5162b7d14248 : Traceable(string)
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
                      )
              |]
        ]
    }

prog3_13 :: [Module Kernel.Type Name (Kernel.Expr Kernel.Type)]
prog3_13 = unsafeParseExpr <$$> fixture1

fixture1 :: [Module Kernel.Type Name Text]
fixture1 =
  [ moduleMain
  ]

fixture3 :: [Module Kernel.Type Name (Kernel.Expr Kernel.Type)]
fixture3 =
  [ moduleCore1
  , moduleMain1
  ]

wooz :: IO ()
wooz = Kernel.testModules =<< Kernel.compileModules fixture3
