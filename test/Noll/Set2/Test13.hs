{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Noll.Set2.Test13 where

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
        ]
    }

moduleFoo1 :: Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type)
moduleFoo1 = unsafeParseExpr <$> moduleFoo

moduleFoo :: Module Lowpass.Type Name Text
moduleFoo =
  Module
    { moduleName = "Foo"
    , moduleImports =
        [ "Core$.operator__string_concatenation"
        , "Core$.int32_to_string"
        ]
    , moduleObjects =
        [ OFunction
            "Foo.show"
            [ Label (TCon "Show" [opaque]) "$a"
            ]
            [r| 
                  match<*>($a : Show(*)) {
                    | ( $Record : { show : * | * }/Show(*)
                      , $r : { show : * | * }
                      ) =>
                        select
                          { show = $f : * | _ : * } =
                            $r : { show : * | * }
                          in
                            $f : *
                  }
              |]
        , OConstant
            "Foo.Show__$instance.string"
            [r| 
                  @<Show(string)>
                    ( $Record : { show : string/string | {} }/Show(string)
                    , { show = fn(s : string) => s : string
                      | {}
                      }
                    )
              |]
        , OConstant
            "Foo.Show__$instance.int32"
            [r| 
                  @<Show(int32)>
                    ( $Record : { show : int32/string | {} }/Show(int32)
                    , { show = Core$.int32_to_string : int32/string
                      | {}
                      }
                    )
              |]
        , OFunction
            "Foo.show__$instance.pair"
            [ Label (TCon "Show" [TOpq]) "$dict1"
            , Label (TCon "Show" [TOpq]) "$dict2"
            , Label Lowpass.opaque "p"
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
                                            ( Foo.show : Show(*)/*/string
                                            , $dict1 : Show(*)
                                            , a : *
                                            )
                                        , ","
                                        )
                                    , @<string>
                                        ( Foo.show : Show(*)/*/string
                                        , $dict2 : Show(*)
                                        , b : *
                                        )
                                    )
                                )
                            , ")"
                            )
                    }
              |]
        , OConstant
            "Foo.Show__$instance.pair"
            [r| 
                  @<Show($Tuple2(*,*))>
                    ( $Record : { show : Show(*)/Show(*)/$Tuple2(*,*)/string | {} }/Show($Tuple2(*,*))
                    , { show = Foo.show__$instance.pair : Show(*)/Show(*)/$Tuple2(*,*)/string
                      | {}
                      }
                    )
              |]
        , OFunction
            "Foo.show__$instance.list"
            [ Label (TCon "Show" [TOpq]) "$dict1"
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
                                          ( Foo.show : Show(*)/*/string
                                          , $dict1 : Show(*)
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
        , OConstant
            "Foo.Show__$instance.list"
            [r| 
                  @<Show(list(*))>
                    ( $Record : { show : Show(*)/list(*)/string | {} }/Show(list(*))
                    , { show = Foo.show__$instance.list : Show(*)/list(*)/string
                      | {}
                      }
                    )
              |]
        , OConstant
            "Foo.foo"
            [r| 
                  let
                    p : $Tuple2(int32, string) =
                      @<$Tuple2(int32, string)>
                        ( $Tuple2 : int32/string/$Tuple2(int32, string)
                        , 1
                        , "hello"
                        )
                    in
                      @<string>
                        ( Foo.show__$instance.pair : Show(*)/Show(*)/$Tuple2(*,*)/string
                        , p : $Tuple2(int32, string)
                        , Foo.show__$instance.int32 : int32/string
                        , Foo.show__$instance.string : string/string
                        )
              |]
        , --        , OFunction
          --            "Foo.baz"
          --            [
          --            ]
          --            undefined
          OFunction
            "Foo.bar"
            [ Label (TCon "Show" [TOpq]) "$dict1"
            , Label (TCon "Show" [TOpq]) "$dict2"
            , Label (TCon "list" [opaque]) "xs"
            ]
            [r| 
                  @<string>
                    ( @<list(*)/string>
                        ( show : Show(list(*))
                        , Foo.Show__$instance.list : Show(*)/list(*)/string
                        , @<Show(*)> 
                            ( $Record : { show : * | * }/Show(*)
                            , { show = 
                                  @<Show(*)>
                                    ( Foo.show : Show($Tuple2(*,*))/Show(*)/Show(*)/Show(*)
                                    , Foo.Show__$instance.list : Show($Tuple2(*,*))
                                    , $dict1 : Show(*)
                                    , $dict2 : Show(*)
                                    )
                              | {}
                              }
                            )
                        )
                    , xs : list(*)
                    )
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
        [ "Core$.operator__not"
        , "Core$.operator__reverse_composition"
        , "Core$.operator__reverse_application"
        , "Core$.always"
        , "Core$.operator__list_concatenation"
        , "Core$.trace_int32"
        , "Core$.trace_string"
        , "Core$.operator__string_concatenation"
        , "Foo.Show__$instance.string"
        , "Foo.Show__$instance.pair"
        , "Foo.Show__$instance.int32"
        , "Foo.Show__$instance.list"
        , "Foo.foo"
        , "Foo.show"
        ]
    , moduleObjects =
        [ OFunction
            "Main.main"
            [Label (TCon "unit" []) "$v.0"]
            [r|
                      let
                      //  xs : list(int32) =
                      //    @<list(int32)>
                      //      ( $Cons : int32/list(int32)/list(int32)
                      //      , 5
                      //      , @<list(int32)>
                      //          ( $Cons : int32/list(int32)/list(int32)
                      //          , 3
                      //          , $Nil : list(int32)
                      //          )
                      //      )
                      //  in
                      //    let s : string =
                      //      @<string>
                      //        ( Foo.show : Show(list(int32))/Show(int32)/list(int32)/string
                      //        , Foo.Show__$instance.list : Show(list(int32))
                      //        , Foo.Show__$instance.int32 : Show(int32)
                      //        , xs : list(int32)
                      //        )
                      //      in
                      //        @<*>
                      //          ( Core$.trace_string : string/*
                      //          , s : string
                      //          )

                      //let
                        xs : list($Tuple2(int32, string)) =
                          @<list($Tuple2(int32, string))>
                            ( $Cons : $Tuple2(int32, string)/list($Tuple2(int32, string))/list($Tuple2(int32, string))
                            , @<$Tuple2(int32, string)>
                                ( $Tuple2 : int32/string/$Tuple2(int32, string)
                                , 5
                                , "abc"
                                )
                            , @<list($Tuple2(int32, string))>
                                ( $Cons : $Tuple2(int32, string)/list($Tuple2(int32, string))/list($Tuple2(int32, string))
                                , @<$Tuple2(int32, string)>
                                    ( $Tuple2 : int32/string/$Tuple2(int32, string)
                                    , 6
                                    , "def"
                                    )
                                , $Nil : list($Tuple2(int32, string))
                                )
                            )
                        in
                          let s : string =
                            @<string>
                              ( Foo.show : Show(list($Tuple2(int32, string)))/Show($Tuple2(int32, string))/list($Tuple2(int32, string))/string
                              , Foo.Show__$instance.list : Show(list($Tuple2(int32, string)))
                              , @<Show($Tuple2(int32, string))> 
                                  ( $Record : { show : * | * }/Show(*)
                                  , { show = 
                                        @<$Tuple2(int32, string)/string>
                                          ( Foo.show : Show($Tuple2(int32, string))/Show(int32)/Show(string)/$Tuple2(int32, string)/string
                                          , Foo.Show__$instance.pair : Show($Tuple2(int32, string))
                                          , Foo.Show__$instance.int32 : Show(int32)
                                          , Foo.Show__$instance.string : Show(string)
                                          )
                                    | {}
                                    }
                                  )
                              , xs : list($Tuple2(int32, string))
                              )
                            in
                              @<*>
                                ( Core$.trace_string : string/*
                                , s : string
                                )

                  //let
                  //  q : string =
                  //    let
                  //      p : $Tuple2(int32, string) =
                  //        @<$Tuple2(int32, string)>
                  //          ( $Tuple2 : int32/string/$Tuple2(int32, string)
                  //          , 1
                  //          , "hello"
                  //          )
                  //      in
                  //        @<string>
                  //          ( @<$Tuple2(int32, string)/string>
                  //              ( Foo.show : Show($Tuple2(int32, string))/Show(int32)/Show(string)/$Tuple2(int32, string)/string
                  //              , Foo.Show__$instance.pair : Show($Tuple2(int32, string))
                  //              , Foo.Show__$instance.int32 : Show(int32)
                  //              , Foo.Show__$instance.string : Show(string)
                  //              )
                  //          , p : $Tuple2(int32, string)
                  //          )
                  //  in
                  //    @<*>
                  //      ( Core$.trace_string : string/*
                  //      , q : string
                  //      )
            |]
        ]
    }

fixture3 :: [Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type)]
fixture3 =
  [ moduleCore1
  , moduleFoo1
  , moduleMain1
  ]

mooz :: IO ()
mooz = Lowpass.testModules =<< Lowpass.compileModules fixture3
