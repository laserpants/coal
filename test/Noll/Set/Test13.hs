{-# LANGUAGE OverloadedStrings #-}

module Noll.Set.Test13 where

import Data.Text (Text)
import Lang.Label (Label (..))
import Lang.Lowpass.Language
import Lang.Lowpass.Parser.Expr (expr)
import Lang.Utils (Name, (<$$>))
import Text.Megaparsec (eof, runParser)
import Text.Megaparsec.Error (errorBundlePretty)

import qualified Data.Text as Text
import qualified Lang.Lowpass.Language as Lowpass

unsafeParseExpr :: Text -> Lowpass.Expr Lowpass.Type
unsafeParseExpr t =
  case runParser expr "" (Text.stripStart t) of
    Left e ->
      error (errorBundlePretty e)
    Right r ->
      r

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
  , Module
      { moduleName = "Ordered"
      , moduleImports =
          []
      , moduleObjects =
          [ OFunction
              "Ordered.compare"
              [ Label (TCon "Ordered.Ordered" [opaque]) "a_1"
              , Label opaque "a_2"
              , Label opaque "a_3"
              ]
              "\
              \ match<Ordered.Ordering>(a_1 : Ordered.Ordered(*)) \
              \   { | ( $Record : { compare : */*/Ordered.Ordering | * }/Ordered.Ordered(*) \
              \       , r_1 : { compare : */*/Ordered.Ordering | * } \
              \       ) => \
              \         select \
              \           { compare = f_1 : */*/Ordered.Ordering | q_1 : * } = \
              \             r_1 : { compare : */*/Ordered.Ordering | * } \
              \           in \
              \             @<Ordered.Ordering>(f_1 : */*/Ordered.Ordering, a_2 : *, a_3 : *) \
              \   } \
              \"
          , OFunction
              "Ordered.$instance.??.compare"
              [Label int32 "x", Label int32 "y"]
              "\
              \ if ([< int32](x : int32, y : int32)) \
              \   then Ordered.LessThan : Ordered.Ordering \
              \   else \
              \     if ([> int32](x : int32, y : int32)) \
              \       then Ordered.GreaterThan : Ordered.Ordering \
              \       else Ordered.EqualTo : Ordered.Ordering \
              \"
          , OFunction
              "Ordered.less_than_or_equal_to"
              [Label (TCon "Ordered.Ordered" [opaque]) "$dict.ffef54c635ab7d00", Label opaque "m", Label opaque "n"]
              "\
              \ match<bool> \
              \   ( @<Ordered.Ordering> \
              \     ( Ordered.compare : Ordered.Ordered(*)/*/*/Ordered.Ordering \
              \     , $dict.ffef54c635ab7d00 : Ordered.Ordered(*) \
              \     , m : * \
              \     , n : * \
              \     )) \
              \   { | (EqualTo : Ordered.Ordering) => true \
              \     | (GreaterThan : Ordered.Ordering) => false \
              \     | (LessThan : Ordered.Ordering) => true \
              \   } \
              \"
          , OFunction
              "Ordered.greater_than"
              [Label (TCon "Ordered.Ordered" [opaque]) "$dict.ffef54c635ab7d00", Label opaque "n"]
              "\
              \ @<*/bool> \
              \   ( Prelude.operator__reverse_composition : (bool/bool)/(*/bool)/*/bool \
              \   , Prelude.not : bool/bool \
              \   , @<*/bool> \
              \       ( Ordered.less_than_or_equal_to : Ordered.Ordered(*)/*/*/bool \
              \       , $dict.ffef54c635ab7d00 : Ordered.Ordered(*) \
              \       , n : *) \
              \   ) \
              \"
          ]
      }
      --  , Module
      --      { moduleName = "BinarySearch"
      --      , moduleImports =
      --          []
      --      , moduleObjects =
      --          [ OFunction
      --              "BinarySearch.in_range"
      --              []
      --              ""
      --          , OFunction
      --              "BinarySearch.from_list"
      --              []
      --              ""
      --          , OFunction
      --              "BinarySearch.flatten"
      --              []
      --              ""
      --          , OFunction
      --              "BinarySearch.sort"
      --              []
      --              ""
      --          ]
      --      }
      --  , Module
      --      { moduleName = "Main"
      --      , moduleImports =
      --          []
      --      , moduleObjects =
      --          [ OFunction
      --              "Main.main"
      --              []
      --              ""
      --          ]
      --      }
  ]

-- Lowpass

-------------------

-- Utils

-------------------

-- Ordered

{-

data Ordered.EqualTo<0, Ordered.Ordering>

data Ordered.GreaterThan<1, Ordered.Ordering>

data Ordered.LessThan<2, Ordered.Ordering>

Ordered.compare(a_1 : record({ compare : */*/Ordered.Ordering | * }), a_2 : *, a_3 : *) =
  match<Ordered.Ordering>(a_1 : record({ compare : */*/Ordered.Ordering | * }))
  { | ( $Record : { compare : */*/Ordered.Ordering | * }/record({ compare : */*/Ordered.Ordering | * })
      , r_1 : { compare : */*/Ordered.Ordering | * }
      ) =>
        select
          { compare = f_1 : */*/Ordered.Ordering | q_1 : * } =
            r_1 : { compare : */*/Ordered.Ordering | * }
          in
            @<Ordered.Ordering>(f_1 : */*/Ordered.Ordering, a_2 : *, a_3 : *)
  }

// instance Ordered.Ordered(int32)

Ordered.Ordered_compare_instance_1(x : int32, y : int32) =
  if ([< int32](x : int32, y : int32))
    then Ordered.LessThan : Ordered.Ordering
    else
      if ([> int32](x : int32, y : int32))
        then Ordered.GreaterThan : Ordered.Ordering
        else Ordered.EqualTo : Ordered.Ordering

-}

-------------------

-- BinarySearch

-------------------

-- Main
