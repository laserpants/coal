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

moduleOrdered :: Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type)
moduleOrdered = unsafeParseExpr <$> moduleOrdered1

moduleOrdered1 :: Module Lowpass.Type Name Text
moduleOrdered1 =
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
  , moduleOrdered1
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
    Module
      { moduleName = "BinarySearch"
      , moduleImports =
          []
      , moduleObjects =
          [ OFunction
              "BinarySearch.in_range"
              [ Label (TCon "Numeric" [TOpq]) "$dict.be194a5d16952b76"
              , Label (TCon "Ordered" [TOpq]) "$dict.ffef54c635ab7d00"
              , Label (TCon "record" [RExt "max" TOpq (RExt "min" TOpq RNil)]) "$v.0"
              , Label TOpq "n"
              ]
              [r|
              |]
          , OFunction
              "BinarySearch.from_list"
              [ Label (TCon "Numeric" [TOpq]) "$dict.be194a5d16952b74"
              , Label (TCon "Ordered" [TOpq]) "$dict.ffef54c635ab7d02"
              , Label (TCon "list" [TOpq]) "list"
              ]
              [r|
              |]
          , OFunction
              "BinarySearch.flatten"
              []
              [r|
              |]
          , OFunction
              "BinarySearch.sort"
              []
              [r|
              |]
          ]
      }
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
