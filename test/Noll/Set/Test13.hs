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
        [ OFunction
            "less_than_or_equal_to"
            [ Label (TCon "Ordered" [opaque]) "$dict.ffef54c635ab7d00"
            , Label opaque "m"
            , Label opaque "n"
            ]
            [r| 
                  match<bool>
                    ( @<Ordering>
                      ( compare : Ordered(*)/*/*/Ordering
                      , $dict.ffef54c635ab7d00 : Ordered(*)
                      , m : *
                      , n : *
                      )
                    ) { 
                      | (EqualTo : Ordering) => true
                      | (GreaterThan : Ordering) => false
                      | (LessThan : Ordering) => true
                  }
              |]
        , OFunction
            "greater_than"
            [ Label (TCon "Ordered" [opaque]) "$dict.ffef54c635ab7d01"
            , Label opaque "n"
            ]
            [r| 
                  @<*/bool>
                    ( Prelude.operator__reverse_composition : (bool/bool)/(*/bool)/*/bool
                    , not : bool/bool
                    , @<*/bool>
                        ( less_than_or_equal_to : Ordered(*)/*/*/bool
                        , $dict.ffef54c635ab7d01 : Ordered(*)
                        , n : *)
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
        [ OFunction
          "in_range"
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
                        { max = $row.1.field.max : * | $row.1.tail : record({ min : * | {} }) } =
                          $match.8.$row.1 : { max : * | min : * | {} }   
                        in
                          match<bool>($row.1.tail : record({ min : * | {} })) 
                            { | ( $Record : { min : * | {} }/record({ min : * | {} })
                                , $match.5.$row.2 : { min : * | {} }
                                ) =>
                                  select
                                    { min = $row.2.field.min : * | $row.2.tail : record({}) } =
                                      $match.5.$row.2 : { min : * | {} }   
                                    in
                                      match<bool>($row.2.tail : record({})) 
                                        { | ( $Record : {}/record({})
                                            , $match.2._ : {}
                                            ) =>
                                              [&&]
                                              ( @<bool>( greater_than : Ordered(*)/*/*/bool
                                                       , $dict.ffef54c635ab7d00 : Ordered(*)
                                                       , n : *
                                                       , $row.2.field.min : * )
                                              , [|| ]
                                                ( @<bool>( less_than_or_equal_to : Ordered(*)/*/*/bool
                                                         , $dict.ffef54c635ab7d00 : Ordered(*)
                                                         , n : *
                                                         , $row.1.field.max : * )
                                                , @<bool>
                                                    ( less_than_or_equal_to : Ordered(*)/*/*/bool
                                                    , $dict.ffef54c635ab7d00 : Ordered(*)
                                                    , $row.1.field.max : * 
                                                    , @<*>
                                                        ( from_int32 : Numeric(*)/int32/*
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
          "from_list"
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
                                     ( Prelude.operator__reverse_application : */(*/bool)/bool
                                     , $match.10.p : *
                                     , @<*/bool>
                                         ( in_range : Numeric(*)/Ordered(*)/record({ max : * | min : * | {} })/*/bool
                                         , $dict.be194a5d16952b77 : Numeric(*)
                                         , $dict.ffef54c635ab7d01 : Ordered(*)
                                         , range : record({ max : * | min : * | {} })
                                         )
                                     )
                                 )
                                 then
                                   @<Tree(*)>
                                     ( Node : */Tree(*)/Tree(*)/Tree(*)
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
                               ( always : Tree(*)/record({ max : * | min : * | {} })/Tree(*)
                               , Leaf : Tree(*)
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
                                   ( from_int32 : Numeric(*)/int32/*
                                   , $dict.be194a5d16952b77 : Numeric(*)
                                   , -1 )
                             | min =
                                 @<*>
                                   ( from_int32 : Numeric(*)/int32/*
                                   , $dict.be194a5d16952b77 : Numeric(*)
                                   , 0 )
                             | {}
                             }
                           )
                       )
          |]
      ]
    }

prog1_13 :: [Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type)]
prog1_13 = unsafeParseExpr <$$> fixture1

fixture1 :: [Module Lowpass.Type Name Text]
fixture1 =
  [ Module
      { moduleName = "Prelude"
      , moduleImports =
          []
      , moduleObjects =
          []
      }
  , Module
      { moduleName = "Utils"
      , moduleImports =
          []
      , moduleObjects =
          []
      }
  , moduleOrdered
  , -- Module
    --  { moduleName = "Ordered"
    --  , moduleImports =
    --      []
    --  , moduleObjects =
    --      [ OFunction
    --          "Ordered.compare"
    --          [ Label (TCon "Ordered.Ordered" [opaque]) "a_1"
    --          , Label opaque "a_2"
    --          , Label opaque "a_3"
    --          ]
    --          [r|
    --          |]
    --      , OFunction
    --          "Ordered.$instance.??.compare"
    --          [Label int32 "x", Label int32 "y"]
    --          [r|
    --          |]
    --      , OFunction
    --          "Ordered.less_than_or_equal_to"
    --          [Label (TCon "Ordered.Ordered" [opaque]) "$dict.ffef54c635ab7d00", Label opaque "m", Label opaque "n"]
    --          [r|
    --          |]
    --      , OFunction
    --          "Ordered.greater_than"
    --          [ Label (TCon "Ordered.Ordered" [opaque]) "$dict.ffef54c635ab7d01"
    --          , Label opaque "n"
    --          ]
    --          [r|
    --          |]
    --      ]
    --  }
    moduleBinarySearch
    --Module
    --  { moduleName = "BinarySearch"
    --  , moduleImports =
    --      []
    --  , moduleObjects =
    --      [ OFunction
    --          "BinarySearch.in_range"
    --          [ Label (TCon "Numeric" [TOpq]) "$dict.be194a5d16952b76"
    --          , Label (TCon "Ordered" [TOpq]) "$dict.ffef54c635ab7d00"
    --          , Label (TCon "record" [RExt "max" TOpq (RExt "min" TOpq RNil)]) "$v.0"
    --          , Label TOpq "n"
    --          ]
    --          [r|
    --          |]
    --      , OFunction
    --          "BinarySearch.from_list"
    --          [ Label (TCon "Numeric" [TOpq]) "$dict.be194a5d16952b74"
    --          , Label (TCon "Ordered" [TOpq]) "$dict.ffef54c635ab7d02"
    --          , Label (TCon "list" [TOpq]) "list"
    --          ]
    --          [r|
    --          |]
    --      , OFunction
    --          "BinarySearch.flatten"
    --          []
    --          [r|
    --          |]
    --      , OFunction
    --          "BinarySearch.sort"
    --          []
    --          [r|
    --          |]
    --      ]
    --  }
  , Module
      { moduleName = "Main"
      , moduleImports =
          []
      , moduleObjects =
          [ OFunction
              "Main.main"
              []
              [r|
              |]
          ]
      }
  ]
