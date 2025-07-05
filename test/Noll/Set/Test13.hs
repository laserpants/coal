{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Noll.Set.Test13 where

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
                                            ( Core$.show : Show(*)/*/string
                                            , $dict1 : Show(*)
                                            , a : *
                                            )
                                        , ","
                                        )
                                    , @<string>
                                        ( Core$.show : Show(*)/*/string
                                        , $dict2 : Show(*)
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
                                          ( Core$.show : Show(*)/*/string
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

moduleOrdered1 :: Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type)
moduleOrdered1 = unsafeParseExpr <$> moduleOrdered

moduleOrdered :: Module Lowpass.Type Name Text
moduleOrdered =
  Module
    { moduleName = "Ordered"
    , moduleImports =
        [ "Utils.Predicate"
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
        [ OData "Ordered.EqualTo" 0 (TCon "Ordering" [])
        , OData "Ordered.GreaterThan" 1 (TCon "Ordering" [])
        , OData "Ordered.LessThan" 2 (TCon "Ordering" [])
        , OFunction
            "Ordered.compare"
            [ Label (TCon "Ordered" [opaque]) "$a"
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
            "Ordered.compare__$instance.b7c5e7e84eeaf782"
            [ Label Lowpass.int32 "x"
            , Label Lowpass.int32 "y"
            ]
            [r| 
                  if ([< int32](x : int32, y : int32))
                    then
                      Ordered.LessThan : Ordering
                    else
                      if ([> int32](x : int32, y : int32))
                        then
                          Ordered.GreaterThan : Ordering
                        else
                          Ordered.EqualTo : Ordering
              |]
        , --        , OConstant
          --            "Ordered.Ordered__$instance.b7c5e7e84eeaf782"
          --            [r|
          --                  @<Ordered(int32)>
          --                    ( $Record : { compare : int32/int32/Ordering | {} }/Ordered(int32)
          --                    , { compare = Ordered.compare__$instance.b7c5e7e84eeaf782 : int32/int32/Ordering
          --                      | {}
          --                      }
          --                    )
          --              |]
          OFunction
            "Ordered.less_than_or_equal_to"
            [ Label (TCon "Ordered" [opaque]) "$dict.ffef54c635ab7d00"
            , Label opaque "m"
            , Label opaque "n"
            ]
            [r| 
                  match<bool>
                    ( @<Ordering>
                      ( Ordered.compare : Ordered(*)/*/*/Ordering
                      , $dict.ffef54c635ab7d00 : Ordered(*)
                      , m : *
                      , n : *
                      )
                    ) { 
                      | (Ordered.EqualTo : Ordering) => true
                      | (Ordered.GreaterThan : Ordering) => false
                      | (Ordered.LessThan : Ordering) => true
                  }
              |]
        , OFunction
            "Ordered.greater_than"
            [ Label (TCon "Ordered" [opaque]) "$dict.ffef54c635ab7d01"
            , Label opaque "n"
            ]
            [r| 
                  @<*/bool>
                    ( Core$.operator__reverse_composition : (bool/bool)/(*/bool)/*/bool
                    , Core$.operator__not : bool/bool
                    , @<*/bool>
                        ( Ordered.less_than_or_equal_to : Ordered(*)/*/*/bool
                        , $dict.ffef54c635ab7d01 : Ordered(*)
                        , n : *
                        )
                    )
              |]
        ]
    }

moduleBinarySearch1 :: Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type)
moduleBinarySearch1 = unsafeParseExpr <$> moduleBinarySearch

moduleBinarySearch :: Module Lowpass.Type Name Text
moduleBinarySearch =
  Module
    { moduleName = "BinarySearch"
    , moduleImports =
        [ "Ordered.EqualTo"
        , "Ordered.GreaterThan"
        , "Ordered.LessThan"
        , "Ordered.compare"
        , "Ordered.greater_than"
        , "Ordered.less_than_or_equal_to"
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
        [ OData "BinarySearch.Leaf" 0 (TCon "Tree" [Lowpass.opaque])
        , OData "BinarySearch.Node" 1 (Lowpass.opaque `Lowpass.arrow` TCon "Tree" [Lowpass.opaque] `Lowpass.arrow` TCon "Tree" [Lowpass.opaque] `Lowpass.arrow` TCon "Tree" [Lowpass.opaque])
        , OFunction
            "BinarySearch.from_int32"
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
            "BinarySearch.in_range"
            [ Label (TCon "Numeric" [TOpq]) "$dict.be194a5d16952b76"
            , Label (TCon "Ordered" [TOpq]) "$dict.ffef54c635ab7d00"
            , Label (TCon "record" [RExt "max" TOpq (RExt "min" TOpq RNil)]) "$v.0"
            , Label TOpq "n"
            ]
            [r| 
                  match<bool>($v.0 : record({ max : * | min : * | {} })) 
                    { | ( $Record : { max : * | min : * | {} }/record({ max : * | min : * | {} })
                        , $match.8.$row.1 : { max : * | min : * | {} }
                        ) =>
                          select
                            { max = $row.1.field.max : * | $rest : { min : * | {} } } =
                              $match.8.$row.1 : { max : * | min : * | {} }   
                            in
                              let 
                                $row.1.tail : record({ min : * | {} }) =
                                  @<record({ min : * | {} })>
                                    ( $Record : { min : * | {} }/record({ min : * | {} })
                                    , $rest : { min : * | {} }
                                    )
                               in
                                match<bool>($row.1.tail : record({ min : * | {} })) 
                                  { | ( $Record : { min : * | {} }/record({ min : * | {} })
                                      , $match.5.$row.2 : { min : * | {} }
                                      ) =>
                                        select
                                          { min = $row.2.field.min : * | $rest : {} } =
                                            $match.5.$row.2 : { min : * | {} }   
                                          in
                                            let
                                              $row.2.tail : record({}) =
                                                @<record({})>
                                                  ( $Record : {}/record({})
                                                  , $rest : {}
                                                  )
                                              in
                                              match<bool>($row.2.tail : record({})) 
                                                { | ( $Record : {}/record({})
                                                    , $match.2._ : {}
                                                    ) =>
                                                      [&&]
                                                      ( @<bool>
                                                          ( Ordered.greater_than : Ordered(*)/*/*/bool
                                                          , $dict.ffef54c635ab7d00 : Ordered(*)
                                                          , n : *
                                                          , $row.2.field.min : * 
                                                          )
                                                      , [|| ]
                                                        ( @<bool>
                                                            ( Ordered.less_than_or_equal_to : Ordered(*)/*/*/bool
                                                            , $dict.ffef54c635ab7d00 : Ordered(*)
                                                            , n : *
                                                            , $row.1.field.max : * 
                                                            )
                                                        , @<bool>
                                                            ( Ordered.less_than_or_equal_to : Ordered(*)/*/*/bool
                                                            , $dict.ffef54c635ab7d00 : Ordered(*)
                                                            , $row.1.field.max : * 
                                                            , @<*>
                                                                ( BinarySearch.from_int32 : Numeric(*)/int32/*
                                                                , $dict.be194a5d16952b76 : Numeric(*)
                                                                , -1
                                                                )
                                                            )
                                                        )
                                                      )
                                                }
                                }
                    }
          |]
        , OFunction
            "BinarySearch.from_list"
            [ Label (TCon "Numeric" [TOpq]) "$dict.be194a5d16952b77"
            , Label (TCon "Ordered" [TOpq]) "$dict.ffef54c635ab7d01"
            , Label (TCon "list" [TOpq]) "list"
            ]
            [r| 
                  let
                    $fold.1 : list(*)/record({ max : * | min : * | {} })/Tree(*) =
                      fn($fold.1.expr : list(*)) =>
                        match<record({ max : * | min : * | {} })/Tree(*)>($fold.1.expr : list(*)) {
                          | ( $Cons : */list(*)/list(*)
                            , $match.10.p : *
                            , $match.11.g : list(*)
                            ) =>
                              fn(range : record({ max : * | min : * | {} })) =>
                                if 
                                  ( @<bool>
                                      ( Core$.operator__reverse_application : */(*/bool)/bool
                                      , $match.10.p : *
                                      , @<*/bool>
                                          ( BinarySearch.in_range : Numeric(*)/Ordered(*)/record({ max : * | min : * | {} })/*/bool
                                          , $dict.be194a5d16952b77 : Numeric(*)
                                          , $dict.ffef54c635ab7d01 : Ordered(*)
                                          , range : record({ max : * | min : * | {} })
                                          )
                                      )
                                  )
                                  then
                                    @<Tree(*)>
                                      ( BinarySearch.Node : */Tree(*)/Tree(*)/Tree(*)
                                      , $match.10.p : *
                                      , @<Tree(*)>
                                          ( $fold.1 : list(*)/record({ max : * | min : * | {} })/Tree(*)
                                          , $match.11.g : list(*)
                                          , @<record({ max : * | min : * | {} })>
                                              ( $Record : { max : * | min : * | {} }/record({ max : * | min : * | {} })
                                              , { max = $match.10.p : *
                                                | min =
                                                    match<*>(range : record({ max : * | min : * | {} })) {
                                                      | ( $Record : { max : * | min : * | {} }/record({ max : * | min : * | {} })
                                                        , $row : { max : * | min : * | {} }
                                                        ) =>
                                                          select
                                                            { min = min : * | _ : { max : * | {} } } =
                                                              $row : { max : * | min : * | {} }
                                                            in
                                                              min : *
                                                    }
                                                | {}
                                                }
                                              )
                                          )
                                      , @<Tree(*)>
                                        ( $fold.1 : list(*)/record({ max : * | min : * | {} })/Tree(*)
                                        , $match.11.g : list(*)
                                        , @<record({ max : * | min : * | {} })>
                                            ( $Record : { max : * | min : * | {} }/record({ max : * | min : * | {} })
                                            , { max = 
                                                  match<*>(range : record({ max : * | min : * | {} })) {
                                                    | ( $Record : { max : * | min : * | {} }/record({ max : * | min : * | {} })
                                                      , $row : { max : * | min : * | {} }
                                                      ) =>
                                                        select
                                                          { max = max : * | _ : { min : * | {} } } =
                                                            $row : { max : * | min : * | {} }
                                                          in
                                                            max : *
                                                  }
                                              | min = $match.10.p : *
                                              | {}
                                              }
                                            )
                                        )
                                      )
                                  else
                                    @<Tree(*)>
                                      ( $fold.1 : list(*)/record({ max : * | min : * | {} })/Tree(*)
                                      , $match.11.g : list(*)
                                      , range : record({ max : * | min : * | {} })
                                      )
                          | ($Nil : list(*)) =>
                              @<record({ max : * | min : * | {} })/Tree(*)>
                                ( Core$.always : Tree(*)/record({ max : * | min : * | {} })/Tree(*)
                                , BinarySearch.Leaf : Tree(*)
                                )
                        }
                    in
                      @<Tree(*)>
                        ( $fold.1 : list(*)/record({ max : * | min : * | {} })/Tree(*)
                        , list : list(*)
                        , @<record({ max : * | min : * | {} })>
                            ( $Record : { max : * | min : * | {} }/record({ max : * | min : * | {} })
                            , { max =
                                  @<*>
                                    ( BinarySearch.from_int32 : Numeric(*)/int32/*
                                    , $dict.be194a5d16952b77 : Numeric(*)
                                    , -1 )
                              | min =
                                  @<*>
                                    ( BinarySearch.from_int32 : Numeric(*)/int32/*
                                    , $dict.be194a5d16952b77 : Numeric(*)
                                    , 0 )
                              | {}
                              }
                            )
                        )
          |]
        , OFunction
            "BinarySearch.flatten"
            [ Label (TCon "Tree" [opaque]) "tree"
            ]
            [r| 
                  let
                    $fold.2 : Tree(*)/list(*) =
                      fn($fold.2.expr : Tree(*)) =>
                        match<list(*)>($fold.2.expr : Tree(*)) {
                          | ( BinarySearch.Leaf : Tree(*)
                            ) =>
                              $Nil : list(*)
                          | ( BinarySearch.Node : */Tree(*)/Tree(*)/Tree(*)
                            , $match.13.y : *
                            , $match.14.lhs : Tree(*)
                            , $match.15.rhs : Tree(*)
                            ) =>
                              @<list(*)>
                                ( Core$.operator__list_concatenation : list(*)/list(*)/list(*)
                                , @<list(*)>
                                    ( $fold.2 : Tree(*)/list(*)
                                    , $match.14.lhs : Tree(*))
                                , @<list(*)>
                                    ( $Cons : */list(*)/list(*)
                                    , $match.13.y : *
                                    , @<list(*)>
                                        ( $fold.2 : Tree(*)/list(*)
                                        , $match.15.rhs : Tree(*))))
                        }
                    in
                      @<list(*)>
                        ( $fold.2 : Tree(*)/list(*)
                        , tree : Tree(*)
                        )
          |]
        , OFunction
            "BinarySearch.sort"
            [ Label (TCon "Numeric" [TOpq]) "$dict.be194a5d16952b75"
            , Label (TCon "Ordered" [TOpq]) "$dict.ffef54c635ab7d03"
            ]
            [r|
                  @<list(*)/list(*)>
                    ( Core$.operator__reverse_composition : (Tree(*)/list(*))/(list(*)/Tree(*))/list(*)/list(*)
                    , BinarySearch.flatten : Tree(*)/list(*)
                    , @<list(*)/Tree(*)>
                        ( BinarySearch.from_list : Numeric(*)/Ordered(*)/list(*)/Tree(*)
                        , $dict.be194a5d16952b75 : Numeric(*)
                        , $dict.ffef54c635ab7d03 : Ordered(*)
                        )
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
        [ "BinarySearch.Leaf"
        , "BinarySearch.Node"
        , "Ordered.Ordered__$instance.b7c5e7e84eeaf782"
        , "BinarySearch.from_int32"
        , "BinarySearch.in_range"
        , "BinarySearch.sort"
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
            "Main.from_int32__$instance.2967b53e939a3c94"
            [ Label Lowpass.int32 "x"
            ]
            [r| 
                  x : int32
              |]
        , --        , OConstant
          --            "Main.Numeric__$instance.2967b53e939a3c94"
          --            [r|
          --                  @<Numeric(int32)>
          --                    ( $Record : { from_int32 : int32/int32 | {} }/Numeric(int32)
          --                    , { from_int32 = Main.from_int32__$instance.2967b53e939a3c94 : int32/int32
          --                      | {}
          --                      }
          --                    )
          --              |]
          OFunction
            "Main.main"
            [Label (TCon "unit" []) "$v.0"]
            [r|
                  let
                    xs : list(int32) =
                      @<list(int32)>
                        ( $Cons : int32/list(int32)/list(int32)
                        , @<int32>
                            ( BinarySearch.from_int32 : Numeric(int32)/int32/int32
                            , Main.Numeric__$instance.2967b53e939a3c94 : Numeric(int32)
                            , 5
                            )
                          , @<list(int32)>
                              ( $Cons : int32/list(int32)/list(int32)
                              , @<int32>
                                  ( BinarySearch.from_int32 : Numeric(int32)/int32/int32
                                  , Main.Numeric__$instance.2967b53e939a3c94 : Numeric(int32)
                                  , 3
                                  )
                                , @<list(int32)>
                                    ( $Cons : int32/list(int32)/list(int32)
                                    , @<int32>
                                        ( BinarySearch.from_int32 : Numeric(int32)/int32/int32
                                        , Main.Numeric__$instance.2967b53e939a3c94 : Numeric(int32)
                                        , 7
                                        )
                                      , @<list(int32)>
                                          ( $Cons : int32/list(int32)/list(int32)
                                          , @<int32>
                                              ( BinarySearch.from_int32 : Numeric(int32)/int32/int32
                                              , Main.Numeric__$instance.2967b53e939a3c94 : Numeric(int32)
                                              , 2
                                              )
                                            , @<list(int32)>
                                                ( $Cons : int32/list(int32)/list(int32)
                                                , @<int32>
                                                    ( BinarySearch.from_int32 : Numeric(int32)/int32/int32
                                                    , Main.Numeric__$instance.2967b53e939a3c94 : Numeric(int32)
                                                    , 1
                                                    )
                                                  , @<list(int32)>
                                                      ( $Cons : int32/list(int32)/list(int32)
                                                      , @<int32>
                                                          ( BinarySearch.from_int32 : Numeric(int32)/int32/int32
                                                          , Main.Numeric__$instance.2967b53e939a3c94 : Numeric(int32)
                                                          , 6
                                                          )
                                                        , @<list(int32)>
                                                            ( $Cons : int32/list(int32)/list(int32)
                                                            , @<int32>
                                                                ( BinarySearch.from_int32 : Numeric(int32)/int32/int32
                                                                , Main.Numeric__$instance.2967b53e939a3c94 : Numeric(int32)
                                                                , 4
                                                                )
                                                              , 
                                                              $Nil : list(int32) 
                                                            )
                                                      )
                                                )
                                          )
                                    )
                              )
                        )
                    in
                      @<*>
                        ( Core$.trace_int32 : int32/*
                        , match<int32>
                            ( @<list(int32)>
                                ( BinarySearch.sort : Numeric(int32)/Ordered(int32)/list(int32)/list(int32)
                                , Main.Numeric__$instance.2967b53e939a3c94 : Numeric(int32)
                                , Ordered.Ordered__$instance.b7c5e7e84eeaf782 : Ordered(int32)
                                , xs : list(int32)
                                )
                            ) {
                            | ( $Cons : int32/list(int32)/list(int32)
                              , $match.17.y : int32
                              , $match.18._ : list(int32)
                              ) =>
                                $match.17.y : int32
                            | ( $Nil : list(int32)
                              ) =>
                                12345
                          }
                        )
            |]
        ]
    }

prog1_13 :: [Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type)]
prog1_13 = unsafeParseExpr <$$> fixture1

fixture1 :: [Module Lowpass.Type Name Text]
fixture1 =
  [ Module
      { moduleName = "Utils"
      , moduleImports =
          [ "Core$.operator__not"
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
          []
      }
  , moduleOrdered
  , moduleBinarySearch
  , moduleMain
  ]

fixture2 :: [Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type)]
fixture2 =
  [ moduleCore1
  , moduleOrdered1
  , moduleBinarySearch1
  , moduleMain1
  ]

zooz :: IO ()
zooz = Lowpass.testModules =<< Lowpass.compileModules fixture2
