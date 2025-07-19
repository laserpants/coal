{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Noll.Set4.Test14 where

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
            "Core$.not"
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
        [ "Core$.trace_string"
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
            "Main.from_int32"
            [ Label (TCon "Numeric" [opaque]) "$a"
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
            "Main.from_int32__$instance.a952655fec712ed8"
            -- TODO
            [ Label int32 "n"
            ]
            [r| 
                  let
                    f : int32/$Nat/$Nat =
                      fn(n : int32, m : $Nat) =>
                        if ([== int32](n : int32, 0))
                          then m : $Nat
                          else
                            @<$Nat>
                              ( f : int32/$Nat/$Nat 
                              , [- int32](n : int32, 1)
                              , @<$Nat>
                                  ( Succ : $Nat/$Nat
                                  , m : $Nat
                                  )
                              )
                    in
                      @<$Nat>
                        ( f : int32/$Nat/$Nat
                        , n : int32
                        , Zero : $Nat
                        )
              |]
        , OFunction
            "Main.$$force_Head"
            []
            [r| 
                 fn(s : { $_Head : unit/* | * }) =>
                   select 
                     { $_Head = f : unit/* | _ : * } = 
                       s : { $_Head : unit/* | * }
                     in
                       @<*>
                         ( f : unit/*
                         , () 
                         )
              |]
        , OFunction
            "Main.$$force_Tail"
            []
            [r| 
                 fn(s : { $_Tail : unit/* | * }) =>
                   select 
                     { $_Tail = f : unit/* | _ : * } = 
                       s : { $_Tail : unit/* | * }
                     in
                       @<*>
                         ( f : unit/*
                         , () 
                         )
              |]
        , OConstant
            "Main.nats"
            [r| 
                 @<Stream(int32)>
                   ( let
                       $unfold.1 : int32/* =
                         fn(n : int32) =>
                           { $_Head = 
                               fn(_ : unit) => 
                                 n : int32
                           | $_Tail = 
                               fn(_ : unit) =>
                                 @<*>
                                   ( $unfold.1 : int32/*
                                   , [+ int32]
                                       ( n : int32
                                       , 1
                                       )
                                   )
                           | {}
                           }
                       in 
                         $unfold.1 : int32/*
                   , 0
                   )
              |]
        , OFunction
            "Main.nth"
            [Label (TCon "$Nat" []) "n"]
            [r| 
                 let
                   $fold.1 : $Nat/Stream(*)/* =
                     fn($fold.1.expr : $Nat) =>
                       match<Stream(*)/*>($fold.1.expr : $Nat) {
                         | ( Succ : $Nat/$Nat
                           , $match.1.f : $Nat
                           ) =>
                             fn(stream : Stream(*)) =>
                               @<*>
                                 ( $fold.1 : $Nat/Stream(*)/*
                                 , $match.1.f : $Nat
                                 , @<Stream(*)>
                                     ( Main.$$force_Tail : Stream(*)/Stream(*)
                                     , stream : Stream(*)
                                     )
                                 )
                         | ( Zero : $Nat
                           ) =>
                             fn(stream : Stream(*)) =>
                               @<*>
                                 ( Main.$$force_Head : Stream(*)/*
                                 , stream : Stream(*)
                                 )
                       }
                   in
                     @<Stream(*)/*>
                       ( $fold.1 : $Nat/Stream(*)/*
                       , n : $Nat
                       )
              |]
        , OFunction
            "Main.main"
            [Label (TCon "unit" []) "$v.0"]
            [r| 
                 let
                   v : int32 =
                     @<int32>
                       ( Main.nth : $Nat/Stream(int32)/int32
                       , @<$Nat>
                           ( Main.from_int32 : Numeric($Nat)/int32/$Nat
                           , @<record({ from_int32 : int32/$Nat | {} })>
                               ( $Record : { from_int32 : int32/$Nat | {} }/record({ from_int32 : int32/$Nat | {} })
                               , { from_int32 = Main.from_int32__$instance.a952655fec712ed8 : int32/$Nat
                                 | {}
                                 }
                               )
                           , 5
                           )
                       , Main.nats : Stream(int32)
                       )
                   in
                     @<*>
                       ( Core$.trace_int32 : int32/*
                       , v : int32
                       )
              |]
        ]
    }

prog4_14 :: [Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type)]
prog4_14 = unsafeParseExpr <$$> fixture1

fixture1 :: [Module Lowpass.Type Name Text]
fixture1 =
  [ moduleMain
  ]

fixture3 :: [Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type)]
fixture3 =
  [ moduleCore1
  , moduleMain1
  ]

woobx :: IO ()
woobx = Lowpass.testModules =<< Lowpass.compileModules fixture3
