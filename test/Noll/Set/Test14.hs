{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Noll.Set.Test14 where

import Data.Text (Text)
import Lang.Label (Label (..))
import Lang.Lowpass.Language
import Lang.Lowpass.Parser.Expr (expr)
import Lang.Utils (Name, (<$$>))
import Text.Megaparsec (eof, runParser)
import Text.Megaparsec.Error (errorBundlePretty)
import Text.RawString.QQ

import qualified Data.Text as Text
import qualified Lang.Lowpass.Language as Lowpass

unsafeParseExpr :: Text -> Lowpass.Expr Lowpass.Type
unsafeParseExpr t =
  case runParser expr "" (Text.stripStart t) of
    Left e ->
      error (errorBundlePretty e)
    Right r ->
      r

moduleOrdered1 :: Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type)
moduleOrdered1 = unsafeParseExpr <$> moduleOrdered

moduleOrdered :: Module Lowpass.Type Name Text
moduleOrdered =
  Module
    { moduleName = "Ordered"
    , moduleImports =
        []
    , moduleObjects =
        [ OData "Ordered.EqualTo" 0 (TCon "Ordered.Ordering" [])
        , OData "Ordered.GreaterThan" 1 (TCon "Ordered.Ordering" [])
        , OData "Ordered.LessThan" 2 (TCon "Ordered.Ordering" [])
        , OFunction
            "Ordered.compare"
            [ Label (TCon "Ordered.Ordered" [opaque]) "$a"
            ]
            [r| 
                  match<*/*/Ordered.Ordering>($a : Ordered.Ordered(*)) {
                    | ( $Record : { compare : */*/Ordered.Ordering | * }/Ordered.Ordered(*)
                      , $r : { compare : */*/Ordered.Ordering | * }
                      ) =>
                        select
                          { compare = $f : */*/Ordered.Ordering | _ : * } =
                            $r : { compare : */*/Ordered.Ordering | * }
                          in
                            $f : */*/Ordered.Ordering
                  }
              |]
        , OFunction
            "compare__$instance.f377c7c1cf28bc72"
            [ Label Lowpass.int32 "x"
            , Label Lowpass.int32 "y"
            ]
            [r| 
                  if ([< int32](x : int32, y : int32))
                    then
                      Ordered.LessThan : Ordered.Ordering
                    else
                      if ([> int32](x : int32, y : int32))
                        then
                          Ordered.GreaterThan : Ordered.Ordering
                        else
                          Ordered.EqualTo : Ordered.Ordering
              |]
        , OFunction
            "Ordered.less_than_or_equal_to"
            [ Label (TCon "Ordered.Ordered" [opaque]) "$dict.ffef54c635ab7d00"
            , Label opaque "m"
            , Label opaque "n"
            ]
            [r| 
                  match<bool>
                    ( @<Ordered.Ordering>
                      ( Ordered.compare : Ordered.Ordered(*)/*/*/Ordered.Ordering
                      , $dict.ffef54c635ab7d00 : Ordered.Ordered(*)
                      , m : *
                      , n : *
                      )
                    ) { 
                      | (Ordered.EqualTo : Ordered.Ordering) => true
                      | (Ordered.GreaterThan : Ordered.Ordering) => false
                      | (Ordered.LessThan : Ordered.Ordering) => true
                  }
              |]
        , OFunction
            "Ordered.greater_than"
            [ Label (TCon "Ordered.Ordered" [opaque]) "$dict.ffef54c635ab7d01"
            , Label opaque "n"
            ]
            [r| 
                  @<*/bool>
                    ( Core$.operator__reverse_composition : (bool/bool)/(*/bool)/*/bool
                    , Core$.not : bool/bool
                    , @<*/bool>
                        ( Ordered.less_than_or_equal_to : Ordered.Ordered(*)/*/*/bool
                        , $dict.ffef54c635ab7d01 : Ordered.Ordered(*)
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
        []
    , moduleObjects =
        [ OData "BinarySearch.Leaf" 0 (TCon "BinarySearch.Tree" [Lowpass.opaque])
        , OData "BinarySearch.Node" 1 (Lowpass.opaque `Lowpass.arrow` TCon "BinarySearch.Tree" [Lowpass.opaque] `Lowpass.arrow` TCon "Tree" [Lowpass.opaque] `Lowpass.arrow` TCon "BinarySearch.Tree" [Lowpass.opaque])
        , OFunction
            "BinarySearch.from_int32"
            [ Label (TCon "BinarySearch.Numeric" [opaque]) "$a"
            ]
            [r| 
                  match<int32/*>($a : BinarySearch.Numeric(*)) {
                    | ( $Record : { from_int32 : int32/* | * }/BinarySearch.Numeric(*)
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
            [ Label (TCon "BinarySearch.Numeric" [TOpq]) "$dict.be194a5d16952b76"
            , Label (TCon "Ordered.Ordered" [TOpq]) "$dict.ffef54c635ab7d00"
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
            [ Label (TCon "BinarySearch.Numeric" [TOpq]) "$dict.be194a5d16952b77"
            , Label (TCon "Ordered.Ordered" [TOpq]) "$dict.ffef54c635ab7d01"
            , Label (TCon "list" [TOpq]) "list"
            ]
            [r| 
                  let
                    $fold.1 : list(*)/record({ max : * | min : * | {} })/BinarySearch.Tree(*) =
                      fn($fold.1.expr : list(*)) =>
                        match<record({ max : * | min : * | {} })/BinarySearch.Tree(*)>($fold.1.expr : list(*)) {
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
                                    @<BinarySearch.Tree(*)>
                                      ( BinarySearch.Node : */BinarySearch.Tree(*)/BinarySearch.Tree(*)/BinarySearch.Tree(*)
                                      , $match.10.p : *
                                      , @<BinarySearch.Tree(*)>
                                          ( $fold.1 : list(*)/record({ max : * | min : * | {} })/BinarySearch.Tree(*)
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
                                      , @<BinarySearch.Tree(*)>
                                        ( $fold.1 : list(*)/record({ max : * | min : * | {} })/BinarySearch.Tree(*)
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
                                    @<BinarySearch.Tree(*)>
                                      ( $fold.1 : list(*)/record({ max : * | min : * | {} })/BinarySearch.Tree(*)
                                      , $match.11.g : list(*)
                                      , range : record({ max : * | min : * | {} })
                                      )
                          | ($Nil : list(*)) =>
                              @<record({ max : * | min : * | {} })/BinarySearch.Tree(*)>
                                ( Core$.always : BinarySearch.Tree(*)/record({ max : * | min : * | {} })/BinarySearch.Tree(*)
                                , BinarySearch.Leaf : BinarySearch.Tree(*)
                                )
                        }
                    in
                      @<BinarySearch.Tree(*)>
                        ( $fold.1 : list(*)/record({ max : * | min : * | {} })/BinarySearch.Tree(*)
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
            [ Label (TCon "BinarySearch.Tree" [opaque]) "tree"
            ]
            [r| 
                  let
                    $fold.2 : BinarySearch.Tree(*)/list(*) =
                      fn($fold.2.expr : BinarySearch.Tree(*)) =>
                        match<list(*)>($fold.2.expr : BinarySearch.Tree(*)) {
                          | ( BinarySearch.Leaf : BinarySearch.Tree(*)
                            ) =>
                              $Nil : list(*)
                          | ( BinarySearch.Node : */BinarySearch.Tree(*)/BinarySearch.Tree(*)/BinarySearch.Tree(*)
                            , $match.13.y : *
                            , $match.14.lhs : BinarySearch.Tree(*)
                            , $match.15.rhs : BinarySearch.Tree(*)
                            ) =>
                              @<list(*)>
                                ( Core$.operator__list_concatenation : list(*)/list(*)/list(*)
                                , @<list(*)>
                                    ( $fold.2 : BinarySearch.Tree(*)/list(*)
                                    , $match.14.lhs : BinarySearch.Tree(*))
                                , @<list(*)>
                                    ( $Cons : */list(*)/list(*)
                                    , $match.13.y : *
                                    , @<list(*)>
                                        ( $fold.2 : BinarySearch.Tree(*)/list(*)
                                        , $match.15.rhs : BinarySearch.Tree(*))))
                        }
                    in
                      @<list(*)>
                        ( $fold.2 : BinarySearch.Tree(*)/list(*)
                        , tree : BinarySearch.Tree(*)
                        )
          |]
        , OFunction
            "BinarySearch.sort"
            [ Label (TCon "BinarySearch.Numeric" [TOpq]) "$dict.be194a5d16952b75"
            , Label (TCon "Ordered.Ordered" [TOpq]) "$dict.ffef54c635ab7d03"
            ]
            [r|
                  @<list(*)/list(*)>
                    ( Core$.operator__reverse_composition : (BinarySearch.Tree(*)/list(*))/(list(*)/BinarySearch.Tree(*))/list(*)/list(*)
                    , BinarySearch.flatten : BinarySearch.Tree(*)/list(*)
                    , @<list(*)/BinarySearch.Tree(*)>
                        ( BinarySearch.from_list : BinarySearch.Numeric(*)/Ordered.Ordered(*)/list(*)/BinarySearch.Tree(*)
                        , $dict.be194a5d16952b75 : BinarySearch.Numeric(*)
                        , $dict.ffef54c635ab7d03 : Ordered.Ordered(*)
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
        []
    , moduleObjects =
        [ OFunction
            "main"
            [Label (TCon "unit" []) "_"]
            [r|
                  let
                    xs : list(int32) =
                      @<list(int32)>
                        ( $Cons : int32/list(int32)/list(int32)
                        , @<int32>
                            ( BinarySearch.from_int32 : BinarySearch.Numeric(int32)/int32/int32
                            , $dict.2967b53e939a3c94 : BinarySearch.Numeric(int32)
                            , 5
                            )
                          , @<list(int32)>
                              ( $Cons : int32/list(int32)/list(int32)
                              , @<int32>
                                  ( BinarySearch.from_int32 : BinarySearch.Numeric(int32)/int32/int32
                                  , $dict.2967b53e939a3c94 : BinarySearch.Numeric(int32)
                                  , 3
                                  )
                                , @<list(int32)>
                                    ( $Cons : int32/list(int32)/list(int32)
                                    , @<int32>
                                        ( BinarySearch.from_int32 : BinarySearch.Numeric(int32)/int32/int32
                                        , $dict.2967b53e939a3c94 : BinarySearch.Numeric(int32)
                                        , 7
                                        )
                                      , @<list(int32)>
                                          ( $Cons : int32/list(int32)/list(int32)
                                          , @<int32>
                                              ( BinarySearch.from_int32 : BinarySearch.Numeric(int32)/int32/int32
                                              , $dict.2967b53e939a3c94 : BinarySearch.Numeric(int32)
                                              , 2
                                              )
                                            , @<list(int32)>
                                                ( $Cons : int32/list(int32)/list(int32)
                                                , @<int32>
                                                    ( BinarySearch.from_int32 : BinarySearch.Numeric(int32)/int32/int32
                                                    , $dict.2967b53e939a3c94 : BinarySearch.Numeric(int32)
                                                    , 1
                                                    )
                                                  , @<list(int32)>
                                                      ( $Cons : int32/list(int32)/list(int32)
                                                      , @<int32>
                                                          ( BinarySearch.from_int32 : BinarySearch.Numeric(int32)/int32/int32
                                                          , $dict.2967b53e939a3c94 : BinarySearch.Numeric(int32)
                                                          , 6
                                                          )
                                                        , @<list(int32)>
                                                            ( $Cons : int32/list(int32)/list(int32)
                                                            , @<int32>
                                                                ( BinarySearch.from_int32 : BinarySearch.Numeric(int32)/int32/int32
                                                                , $dict.2967b53e939a3c94 : BinarySearch.Numeric(int32)
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
                      let
                        ys : list(int32) =
                          @<list(int32)>
                            ( BinarySearch.sort : BinarySearch.Numeric(int32)/Ordered.Ordered(int32)/list(int32)/list(int32)
                            , Main.d : BinarySearch.Numeric(int32)
                            , Main.d2 : Ordered.Ordered(int32)
                            , xs : list(int32)
                            )
                        in
                          match<int32>(ys : list(int32)) {
                            | ( $Cons : int32/list(int32)/list(int32)
                              , x : int32
                              , _ : list(int32)
                              ) =>
                                #(print_int32 : int32/*, x : int32) (fn(a : *) => 0)
                            | ( $Nil : list(int32)
                              ) =>
                                #(print_int32 : int32/*, 7) (fn(a : *) => 0)
                          }
            |]
        ]
    }

prog1_14 :: [Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type)]
prog1_14 = unsafeParseExpr <$$> fixture1

fixture1 :: [Module Lowpass.Type Name Text]
fixture1 =
  [ Module
      { --      { moduleName = "Core$"
        --      , moduleImports =
        --          []
        --      , moduleObjects =
        --          []
        --      }
        --  , Module
        moduleName = "Utils"
      , moduleImports =
          []
      , moduleObjects =
          []
      }
  , moduleOrdered
  , moduleBinarySearch
  , moduleMain
  ]
