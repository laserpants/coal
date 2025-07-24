{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Noll.Compiler.Lowpass.TranslateExpressionSpec where

import Data.Text (Text)
import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Lang.Lowpass.Parser.Expr (expr)
import Lang.Utils (Name, (<$$>))
import Noll.Compiler.Lowpass.TranslateExpression
import Noll.Language
import Noll.Language.Module (Constant (..), Definition (..), Function (..), Module (..), Path (..))
import Text.Megaparsec (eof, runParser)
import Text.Megaparsec.Error (errorBundlePretty)
import Text.RawString.QQ

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Lang.Lowpass.Language as LP
import qualified Noll.Language.Module as Module

foobaz1 =
  ELambda
    ()
    ( PVariable () (Label (TIntrinsic IInt32) "x")
        <| PVariable () (Label (TIntrinsic IInt32) "y")
        :| []
    )
    ( EIf
        ()
        (TConstructor KType "Ordering")
        ( EApplication
            ()
            (TIntrinsic IBool)
            ( EBinaryOperator
                ()
                (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IBool)
                OLessThan
            )
            ( EVariable () (Label (TIntrinsic IInt32) "x")
                <| EVariable () (Label (TIntrinsic IInt32) "y")
                :| []
            )
        )
        (EConstructor () (Label (TConstructor KType "Ordering") "LessThan"))
        ( EIf
            ()
            (TConstructor KType "Ordering")
            ( EApplication
                ()
                (TIntrinsic IBool)
                ( EBinaryOperator
                    ()
                    (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IBool)
                    OGreaterThan
                )
                ( EVariable () (Label (TIntrinsic IInt32) "x")
                    <| EVariable () (Label (TIntrinsic IInt32) "y")
                    :| []
                )
            )
            (EConstructor () (Label (TConstructor KType "Ordering") "GreaterThan"))
            (EConstructor () (Label (TConstructor KType "Ordering") "EqualTo"))
        )
    )

foobaz1r =
  LP.lam
    (Label LP.int32 "x" <| Label LP.int32 "y" :| [])
    ( LP.if_
        ( LP.op
            ( LP.OLtInt32
                (LP.var (Label LP.int32 "x"))
                (LP.var (Label LP.int32 "y"))
            )
        )
        (LP.var (Label (LP.TCon "Ordering" []) "LessThan"))
        ( LP.if_
            ( LP.op
                ( LP.OGtInt32
                    (LP.var (Label LP.int32 "x"))
                    (LP.var (Label LP.int32 "y"))
                )
            )
            (LP.var (Label (LP.TCon "Ordering" []) "GreaterThan"))
            (LP.var (Label (LP.TCon "Ordering" []) "EqualTo"))
        )
    )

orderedCompareInstance1 =
  DFunction
    "compare"
    ( Function
        ()
        (With [] (TConstructor KType "Ordering"))
        (PVariable () (Label (TIntrinsic IInt32) "x") <| PVariable () (Label (TIntrinsic IInt32) "y") :| [])
        ( EIf
            ()
            (TConstructor KType "Ordering")
            ( EApplication
                ()
                (TIntrinsic IBool)
                ( EBinaryOperator
                    ()
                    (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IBool)
                    OLessThan
                )
                ( EVariable () (Label (TIntrinsic IInt32) "x")
                    <| EVariable () (Label (TIntrinsic IInt32) "y")
                    :| []
                )
            )
            (EConstructor () (Label (TConstructor KType "Ordering") "LessThan"))
            ( EIf
                ()
                (TConstructor KType "Ordering")
                ( EApplication
                    ()
                    (TIntrinsic IBool)
                    ( EBinaryOperator
                        ()
                        (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IBool)
                        OGreaterThan
                    )
                    ( EVariable () (Label (TIntrinsic IInt32) "x")
                        <| EVariable () (Label (TIntrinsic IInt32) "y")
                        :| []
                    )
                )
                (EConstructor () (Label (TConstructor KType "Ordering") "GreaterThan"))
                (EConstructor () (Label (TConstructor KType "Ordering") "EqualTo"))
            )
        )
    )

orderedCompareInstance1Result =
  LP.OFunction
    "Ordered.compare"
    [Label LP.int32 "x", Label LP.int32 "y"]
    ( LP.if_
        ( LP.op
            ( LP.OLtInt32
                (LP.var (Label LP.int32 "x"))
                (LP.var (Label LP.int32 "y"))
            )
        )
        (LP.var (Label (LP.TCon "Ordering" []) "Ordered.LessThan"))
        ( LP.if_
            ( LP.op
                ( LP.OGtInt32
                    (LP.var (Label LP.int32 "x"))
                    (LP.var (Label LP.int32 "y"))
                )
            )
            (LP.var (Label (LP.TCon "Ordering" []) "Ordered.GreaterThan"))
            (LP.var (Label (LP.TCon "Ordering" []) "Ordered.EqualTo"))
        )
    )

orderedLessThanOrEqualTo =
  DFunction
    "less_than_or_equal_to"
    ( Function
        ()
        ( With
            [Trait "Ordered" (TVariable (TypeIndex KType 0))]
            (TIntrinsic IBool)
        )
        ( PVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Ordered") (TVariable (TypeIndex KType 0) :| [])) "$dict.ffef54c635ab7d00")
            <| PVariable () (Label (TVariable (TypeIndex KType 0)) "m")
            <| PVariable () (Label (TVariable (TypeIndex KType 0)) "n")
            :| []
        )
        ( ECompiledMatch
            ()
            (TIntrinsic IBool)
            ( EApplication
                ()
                (TConstructor KType "Ordering")
                (EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Ordered") (TVariable (TypeIndex KType 0) :| []) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TConstructor KType "Ordering") "compare"))
                ( EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Ordered") (TVariable (TypeIndex KType 0) :| [])) "$dict.ffef54c635ab7d00")
                    <| EVariable () (Label (TVariable (TypeIndex KType 0)) "m")
                    <| EVariable () (Label (TVariable (TypeIndex KType 0)) "n")
                    :| []
                )
            )
            ( ECompiledClause
                (Label (TConstructor KType "Ordering") "EqualTo" :| [])
                (ELiteral () (LBool True))
                <| ECompiledClause
                  (Label (TConstructor KType "Ordering") "GreaterThan" :| [])
                  (ELiteral () (LBool False))
                <| ECompiledClause
                  (Label (TConstructor KType "Ordering") "LessThan" :| [])
                  (ELiteral () (LBool True))
                :| []
            )
        )
    )

orderedLessThanOrEqualToResult =
  LP.OFunction
    "Ordered.less_than_or_equal_to"
    [ Label (LP.TCon "Ordered" [LP.TOpq]) "$dict.ffef54c635ab7d00"
    , Label LP.TOpq "m"
    , Label LP.TOpq "n"
    ]
    ( LP.match
        LP.bool
        ( LP.app
            (LP.TCon "Ordering" [])
            (LP.var (Label (LP.TCon "Ordered" [LP.TOpq] `LP.arrow` LP.TOpq `LP.arrow` LP.TOpq `LP.arrow` LP.TCon "Ordering" []) "Ordered.compare"))
            ( LP.var (Label (LP.TCon "Ordered" [LP.TOpq]) "$dict.ffef54c635ab7d00")
                <| LP.var (Label LP.TOpq "m")
                <| LP.var (Label LP.TOpq "n")
                :| []
            )
        )
        ( LP.Clause
            (Label (LP.TCon "Ordering" []) "Ordered.EqualTo" :| [])
            (LP.lit (LP.PBool True))
            <| LP.Clause
              (Label (LP.TCon "Ordering" []) "Ordered.GreaterThan" :| [])
              (LP.lit (LP.PBool False))
            <| LP.Clause
              (Label (LP.TCon "Ordering" []) "Ordered.LessThan" :| [])
              (LP.lit (LP.PBool True))
            :| []
        )
    )

binarySearchInRange =
  DFunction
    "in_range"
    ( Function
        ()
        ( With
            [ Trait "Numeric" (TVariable (TypeIndex KType 0))
            , Trait "Ordered" (TVariable (TypeIndex KType 0))
            ]
            (TIntrinsic IBool)
        )
        ( PVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TVariable (TypeIndex KType 0) :| [])) "$dict.be194a5d16952b76")
            <| PVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Ordered") (TVariable (TypeIndex KType 0) :| [])) "$dict.ffef54c635ab7d00")
            <| PVariable
              ()
              ( Label
                  ( TIntrinsic
                      ( IRecord
                          ( TRow
                              ( RExtend
                                  "max"
                                  (TVariable (TypeIndex KType 0))
                                  ( RExtend
                                      "min"
                                      (TVariable (TypeIndex KType 0))
                                      RNil
                                  )
                              )
                          )
                      )
                  )
                  "$v.0"
              )
            <| PAnnotation
              ()
              (TVariable (Parameter () "a"))
              (PVariable () (Label (TVariable (TypeIndex KType 0)) "n"))
            :| []
        )
        ( ECompiledMatch
            ()
            (TIntrinsic IBool)
            ( EVariable
                ()
                ( Label
                    ( TIntrinsic
                        ( IRecord
                            ( TRow
                                ( RExtend
                                    "max"
                                    (TVariable (TypeIndex KType 0))
                                    ( RExtend
                                        "min"
                                        (TVariable (TypeIndex KType 0))
                                        RNil
                                    )
                                )
                            )
                        )
                    )
                    "$v.0"
                )
            )
            ( ECompiledClause
                ( Label
                    ( TRow
                        ( RExtend
                            "max"
                            (TVariable (TypeIndex KType 0))
                            ( RExtend
                                "min"
                                (TVariable (TypeIndex KType 0))
                                RNil
                            )
                        )
                        `TArrow` TIntrinsic
                          ( IRecord
                              ( TRow
                                  ( RExtend
                                      "max"
                                      (TVariable (TypeIndex KType 0))
                                      ( RExtend
                                          "min"
                                          (TVariable (TypeIndex KType 0))
                                          RNil
                                      )
                                  )
                              )
                          )
                    )
                    "$Record"
                    <| Label
                      ( TRow
                          ( RExtend
                              "max"
                              (TVariable (TypeIndex KType 0))
                              ( RExtend
                                  "min"
                                  (TVariable (TypeIndex KType 0))
                                  RNil
                              )
                          )
                      )
                      "$match.8.$row.1"
                    :| []
                )
                ( EFocus
                    "max"
                    (Label (TVariable (TypeIndex KType 0)) "$row.1.field.max")
                    (Label (TIntrinsic (IRecord (TRow (RExtend "min" (TVariable (TypeIndex KType 0)) RNil)))) "$row.1.tail")
                    ( EVariable
                        ()
                        ( Label
                            ( TRow
                                ( RExtend
                                    "max"
                                    (TVariable (TypeIndex KType 0))
                                    ( RExtend
                                        "min"
                                        (TVariable (TypeIndex KType 0))
                                        RNil
                                    )
                                )
                            )
                            "$match.8.$row.1"
                        )
                    )
                    ( ECompiledMatch
                        ()
                        (TIntrinsic IBool)
                        (EVariable () (Label (TIntrinsic (IRecord (TRow (RExtend "min" (TVariable (TypeIndex KType 0)) RNil)))) "$row.1.tail"))
                        ( ECompiledClause
                            ( Label
                                ( TRow
                                    ( RExtend
                                        "min"
                                        (TVariable (TypeIndex KType 0))
                                        RNil
                                    )
                                    `TArrow` TIntrinsic
                                      ( IRecord
                                          ( TRow
                                              ( RExtend
                                                  "min"
                                                  (TVariable (TypeIndex KType 0))
                                                  RNil
                                              )
                                          )
                                      )
                                )
                                "$Record"
                                <| Label
                                  ( TRow
                                      ( RExtend
                                          "min"
                                          (TVariable (TypeIndex KType 0))
                                          RNil
                                      )
                                  )
                                  "$match.5.$row.2"
                                :| []
                            )
                            ( EFocus
                                "min"
                                (Label (TVariable (TypeIndex KType 0)) "$row.2.field.min")
                                (Label (TIntrinsic (IRecord (TRow RNil))) "$row.2.tail")
                                ( EVariable
                                    ()
                                    ( Label
                                        ( TRow
                                            ( RExtend
                                                "min"
                                                (TVariable (TypeIndex KType 0))
                                                RNil
                                            )
                                        )
                                        "$match.5.$row.2"
                                    )
                                )
                                ( ECompiledMatch
                                    ()
                                    (TIntrinsic IBool)
                                    (EVariable () (Label (TIntrinsic (IRecord (TRow RNil))) "$row.2.tail"))
                                    ( ECompiledClause
                                        ( Label (TRow RNil `TArrow` TIntrinsic (IRecord (TRow RNil))) "$Record"
                                            <| Label (TRow RNil) "$match.2._"
                                            :| []
                                        )
                                        ( EApplication
                                            ()
                                            (TIntrinsic IBool)
                                            (EBinaryOperator () (TIntrinsic IBool `TArrow` TIntrinsic IBool `TArrow` TIntrinsic IBool) OLogicalAnd)
                                            ( EApplication
                                                ()
                                                (TIntrinsic IBool)
                                                (EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Ordered") (TVariable (TypeIndex KType 0) :| []) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool) "greater_than"))
                                                ( EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Ordered") (TVariable (TypeIndex KType 0) :| [])) "$dict.ffef54c635ab7d00")
                                                    <| EVariable () (Label (TVariable (TypeIndex KType 0)) "n")
                                                    <| EVariable () (Label (TVariable (TypeIndex KType 0)) "$row.2.field.min")
                                                    :| []
                                                )
                                                <| EApplication
                                                  ()
                                                  (TIntrinsic IBool)
                                                  (EBinaryOperator () (TIntrinsic IBool `TArrow` TIntrinsic IBool `TArrow` TIntrinsic IBool) OLogicalOr)
                                                  ( EApplication
                                                      ()
                                                      (TIntrinsic IBool)
                                                      (EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Ordered") (TVariable (TypeIndex KType 0) :| []) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool) "less_than_or_equal_to"))
                                                      ( EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Ordered") (TVariable (TypeIndex KType 0) :| [])) "$dict.ffef54c635ab7d00")
                                                          <| EVariable () (Label (TVariable (TypeIndex KType 0)) "n")
                                                          <| EVariable () (Label (TVariable (TypeIndex KType 0)) "$row.1.field.max")
                                                          :| []
                                                      )
                                                      <| EApplication
                                                        ()
                                                        (TIntrinsic IBool)
                                                        (EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Ordered") (TVariable (TypeIndex KType 0) :| []) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool) "less_than_or_equal_to"))
                                                        ( EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Ordered") (TVariable (TypeIndex KType 0) :| [])) "$dict.ffef54c635ab7d00")
                                                            <| EVariable () (Label (TVariable (TypeIndex KType 0)) "$row.1.field.max")
                                                            <| EApplication
                                                              ()
                                                              (TVariable (TypeIndex KType 0))
                                                              (EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TVariable (TypeIndex KType 0) :| []) `TArrow` TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 0)) "from_int32"))
                                                              ( EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TVariable (TypeIndex KType 0) :| [])) "$dict.be194a5d16952b76")
                                                                  <| ELiteral () (LInt32 (-1))
                                                                  :| []
                                                              )
                                                            :| []
                                                        )
                                                      :| []
                                                  )
                                                :| []
                                            )
                                        )
                                        :| []
                                    )
                                )
                            )
                            :| []
                        )
                    )
                )
                :| []
            )
        )
    )

orderedInstanceOrdered =
  DInstance
    "Ordered"
    (TIntrinsic IInt32)
    [ DFunction
        "compare"
        ( Function
            ()
            (With [] (TConstructor KType "Ordering"))
            (PVariable () (Label (TIntrinsic IInt32) "x") <| PVariable () (Label (TIntrinsic IInt32) "y") :| [])
            ( EIf
                ()
                (TConstructor KType "Ordering")
                ( EApplication
                    ()
                    (TIntrinsic IBool)
                    ( EBinaryOperator
                        ()
                        (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IBool)
                        OLessThan
                    )
                    ( EVariable () (Label (TIntrinsic IInt32) "x")
                        <| EVariable () (Label (TIntrinsic IInt32) "y")
                        :| []
                    )
                )
                (EConstructor () (Label (TConstructor KType "Ordering") "LessThan"))
                ( EIf
                    ()
                    (TConstructor KType "Ordering")
                    ( EApplication
                        ()
                        (TIntrinsic IBool)
                        ( EBinaryOperator
                            ()
                            (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IBool)
                            OGreaterThan
                        )
                        ( EVariable () (Label (TIntrinsic IInt32) "x")
                            <| EVariable () (Label (TIntrinsic IInt32) "y")
                            :| []
                        )
                    )
                    (EConstructor () (Label (TConstructor KType "Ordering") "GreaterThan"))
                    (EConstructor () (Label (TConstructor KType "Ordering") "EqualTo"))
                )
            )
        )
    ]

orderedGreaterThan =
  DFunction
    "greater_than"
    ( Function
        ()
        ( With
            [Trait "Ordered" (TVariable (TypeIndex KType 1))]
            (TVariable (TypeIndex KType 1) `TArrow` TIntrinsic IBool)
        )
        ( PVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Ordered") (TVariable (TypeIndex KType 1) :| [])) "$dict.ffef54c635ab7d01")
            <| PAnnotation
              ()
              (TVariable (Parameter () "a"))
              (PVariable () (Label (TVariable (TypeIndex KType 1)) "n"))
            :| []
        )
        ( EApplication
            ()
            (TVariable (TypeIndex KType 1) `TArrow` TIntrinsic IBool)
            ( EBinaryOperator
                ()
                ( (TIntrinsic IBool `TArrow` TIntrinsic IBool)
                    `TArrow` (TVariable (TypeIndex KType 1) `TArrow` TIntrinsic IBool)
                    `TArrow` TVariable (TypeIndex KType 1)
                    `TArrow` TIntrinsic IBool
                )
                OReverseComposition
            )
            ( EVariable () (Label (TIntrinsic IBool `TArrow` TIntrinsic IBool) "not")
                <| EApplication
                  ()
                  (TVariable (TypeIndex KType 1) `TArrow` TIntrinsic IBool)
                  (EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Ordered") (TVariable (TypeIndex KType 1) :| []) `TArrow` TVariable (TypeIndex KType 1) `TArrow` TVariable (TypeIndex KType 1) `TArrow` TIntrinsic IBool) "less_than_or_equal_to"))
                  ( EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Ordered") (TVariable (TypeIndex KType 1) :| [])) "$dict.ffef54c635ab7d01")
                      <| EVariable () (Label (TVariable (TypeIndex KType 1)) "n")
                      :| []
                  )
                :| []
            )
        )
    )

unsafeParseExpr :: Text -> LP.Expr LP.Type
unsafeParseExpr t =
  case runParser expr "" (Text.stripStart t) of
    Left e ->
      error (errorBundlePretty e)
    Right r ->
      r

-- orderedGreaterThanResult :: [Module LP.Type Name (LP.Expr LP.Type)]
binarySearchInRangeResult = unsafeParseExpr <$$> binarySearchInRangeResult1

binarySearchInRangeResult1 =
  [ LP.OFunction
      "BinarySearch.in_range"
      [ Label (LP.TCon "Numeric" [LP.TOpq]) "$dict.be194a5d16952b76"
      , Label (LP.TCon "Ordered" [LP.TOpq]) "$dict.ffef54c635ab7d00"
      , Label (LP.TCon "record" [LP.RExt "max" LP.TOpq (LP.RExt "min" LP.TOpq LP.RNil)]) "$v.0"
      , Label LP.TOpq "n"
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
  ]

orderedGreaterThanResult = unsafeParseExpr <$$> orderedGreaterThanResult1

orderedGreaterThanResult1 =
  [ LP.OFunction
      "greater_than"
      [ Label (LP.TCon "Ordered" [LP.TOpq]) "$dict.ffef54c635ab7d01"
      , Label LP.TOpq "n"
      ]
      [r| 
          @<*/bool>
            ( Core$.operator__reverse_composition : (bool/bool)/(*/bool)/*/bool
            , not : bool/bool
            , @<*/bool>
                ( less_than_or_equal_to : Ordered(*)/*/*/bool
                , $dict.ffef54c635ab7d01 : Ordered(*)
                , n : *)
            )
        |]
  ]

binarySearchFromList =
  DFunction
    "from_list"
    ( Function
        ()
        ( With
            [ Trait "Numeric" (TVariable (TypeIndex KType 2))
            , Trait "Ordered" (TVariable (TypeIndex KType 2))
            ]
            (TApplication KType (TConstructor (KArrow KType KType) "Tree") (TVariable (TypeIndex KType 2) :| []))
        )
        ( PVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TVariable (TypeIndex KType 2) :| [])) "$dict.be194a5d16952b77")
            <| PVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Ordered") (TVariable (TypeIndex KType 2) :| [])) "$dict.ffef54c635ab7d02")
            <| PAnnotation
              ()
              (TIntrinsic (IList (TVariable (Parameter () "a"))))
              (PVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 2)))) "list"))
            :| []
        )
        ( EFold
            ()
            ( TApplication
                KType
                (TConstructor (KArrow KType KType) "Tree")
                (TVariable (TypeIndex KType 2) :| [])
            )
            ( EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 2)))) "list")
                <| ERecord
                  ()
                  ( TIntrinsic
                      ( IRecord
                          ( TRow
                              ( RExtend
                                  "max"
                                  (TVariable (TypeIndex KType 2))
                                  (RExtend "min" (TVariable (TypeIndex KType 2)) RNil)
                              )
                          )
                      )
                  )
                  ( Map.fromList
                      [
                        ( "min"
                        , EApplication
                            ()
                            (TVariable (TypeIndex KType 2))
                            (EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TVariable (TypeIndex KType 2) :| []) `TArrow` TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 2)) "from_int32"))
                            ( EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TVariable (TypeIndex KType 2) :| [])) "$dict.be194a5d16952b77")
                                <| ELiteral () (LInt32 0)
                                :| []
                            )
                        )
                      ,
                        ( "max"
                        , EApplication
                            ()
                            (TVariable (TypeIndex KType 2))
                            (EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TVariable (TypeIndex KType 2) :| []) `TArrow` TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 2)) "from_int32"))
                            ( EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TVariable (TypeIndex KType 2) :| [])) "$dict.be194a5d16952b77")
                                <| ELiteral () (LInt32 (-1))
                                :| []
                            )
                        )
                      ]
                  )
                  Nothing
                :| []
            )
            ( EClause
                ()
                ( PListCons
                    ()
                    (TIntrinsic (IList (TVariable (TypeIndex KType 2))))
                    (PVariable () (Label (TVariable (TypeIndex KType 2)) "p"))
                    (PAtVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 2)))) "g"))
                )
                ( CPlain
                    ()
                    []
                    ( ELambda
                        ()
                        ( PVariable
                            ()
                            ( Label
                                ( TIntrinsic
                                    ( IRecord
                                        ( TRow
                                            ( RExtend
                                                "max"
                                                (TVariable (TypeIndex KType 2))
                                                (RExtend "min" (TVariable (TypeIndex KType 2)) RNil)
                                            )
                                        )
                                    )
                                )
                                "range"
                            )
                            :| []
                        )
                        ( EIf
                            ()
                            ( TApplication
                                KType
                                (TConstructor (KArrow KType KType) "Tree")
                                (TVariable (TypeIndex KType 2) :| [])
                            )
                            ( EApplication
                                ()
                                (TIntrinsic IBool)
                                ( EBinaryOperator
                                    ()
                                    ( TArrow
                                        (TVariable (TypeIndex KType 2))
                                        (TArrow (TArrow (TVariable (TypeIndex KType 2)) (TIntrinsic IBool)) (TIntrinsic IBool))
                                    )
                                    OReverseApplication
                                )
                                ( EVariable () (Label (TVariable (TypeIndex KType 2)) "p")
                                    <| EApplication
                                      ()
                                      (TVariable (TypeIndex KType 2) `TArrow` TIntrinsic IBool)
                                      ( EVariable
                                          ()
                                          ( Label
                                              ( TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TVariable (TypeIndex KType 2) :| [])
                                                  `TArrow` TApplication KTrait (TConstructor (KArrow KType KTrait) "Ordered") (TVariable (TypeIndex KType 2) :| [])
                                                  `TArrow` TIntrinsic
                                                    ( IRecord
                                                        ( TRow
                                                            ( RExtend
                                                                "max"
                                                                (TVariable (TypeIndex KType 2))
                                                                (RExtend "min" (TVariable (TypeIndex KType 2)) RNil)
                                                            )
                                                        )
                                                    )
                                                  `TArrow` TVariable (TypeIndex KType 2)
                                                  `TArrow` TIntrinsic IBool
                                              )
                                              "in_range"
                                          )
                                      )
                                      ( EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TVariable (TypeIndex KType 2) :| [])) "$dict.be194a5d16952b77")
                                          <| EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Ordered") (TVariable (TypeIndex KType 2) :| [])) "$dict.ffef54c635ab7d02")
                                          <| EVariable
                                            ()
                                            ( Label
                                                ( TIntrinsic
                                                    ( IRecord
                                                        ( TRow
                                                            ( RExtend
                                                                "max"
                                                                (TVariable (TypeIndex KType 2))
                                                                (RExtend "min" (TVariable (TypeIndex KType 2)) RNil)
                                                            )
                                                        )
                                                    )
                                                )
                                                "range"
                                            )
                                          :| []
                                      )
                                    :| []
                                )
                            )
                            ( EApplication
                                ()
                                ( TApplication
                                    KType
                                    (TConstructor (KArrow KType KType) "Tree")
                                    (TVariable (TypeIndex KType 2) :| [])
                                )
                                ( EConstructor
                                    ()
                                    ( Label
                                        ( (TVariable (TypeIndex KType 2))
                                            `TArrow` ( TApplication
                                                        KType
                                                        (TConstructor (KArrow KType KType) "Tree")
                                                        (TVariable (TypeIndex KType 2) :| [])
                                                     )
                                            `TArrow` ( TApplication
                                                        KType
                                                        (TConstructor (KArrow KType KType) "Tree")
                                                        (TVariable (TypeIndex KType 2) :| [])
                                                     )
                                            `TArrow` ( TApplication
                                                        KType
                                                        (TConstructor (KArrow KType KType) "Tree")
                                                        (TVariable (TypeIndex KType 2) :| [])
                                                     )
                                        )
                                        "Node"
                                    )
                                )
                                ( EVariable () (Label (TVariable (TypeIndex KType 2)) "p")
                                    <| EApplication
                                      ()
                                      ( TApplication
                                          KType
                                          (TConstructor (KArrow KType KType) "Tree")
                                          (TVariable (TypeIndex KType 2) :| [])
                                      )
                                      ( EVariable
                                          ()
                                          ( Label
                                              ( ( TIntrinsic
                                                    ( IRecord
                                                        ( TRow
                                                            ( RExtend
                                                                "max"
                                                                (TVariable (TypeIndex KType 2))
                                                                (RExtend "min" (TVariable (TypeIndex KType 2)) RNil)
                                                            )
                                                        )
                                                    )
                                                )
                                                  `TArrow` ( TApplication
                                                              KType
                                                              (TConstructor (KArrow KType KType) "Tree")
                                                              (TVariable (TypeIndex KType 2) :| [])
                                                           )
                                              )
                                              "g"
                                          )
                                      )
                                      ( ERecord
                                          ()
                                          ( TIntrinsic
                                              ( IRecord
                                                  ( TRow
                                                      ( RExtend
                                                          "max"
                                                          (TVariable (TypeIndex KType 2))
                                                          (RExtend "min" (TVariable (TypeIndex KType 2)) RNil)
                                                      )
                                                  )
                                              )
                                          )
                                          ( Map.fromList
                                              [
                                                ( "min"
                                                , ESelect
                                                    ()
                                                    (Label (TVariable (TypeIndex KType 2)) "min")
                                                    ( EVariable
                                                        ()
                                                        ( Label
                                                            ( TIntrinsic
                                                                ( IRecord
                                                                    ( TRow
                                                                        ( RExtend
                                                                            "max"
                                                                            (TVariable (TypeIndex KType 2))
                                                                            (RExtend "min" (TVariable (TypeIndex KType 2)) RNil)
                                                                        )
                                                                    )
                                                                )
                                                            )
                                                            "range"
                                                        )
                                                    )
                                                )
                                              ,
                                                ( "max"
                                                , EVariable () (Label (TVariable (TypeIndex KType 2)) "p")
                                                )
                                              ]
                                          )
                                          Nothing
                                          :| []
                                      )
                                    <| EApplication
                                      ()
                                      ( TApplication
                                          KType
                                          (TConstructor (KArrow KType KType) "Tree")
                                          (TVariable (TypeIndex KType 2) :| [])
                                      )
                                      ( EVariable
                                          ()
                                          ( Label
                                              ( TIntrinsic
                                                  ( IRecord
                                                      ( TRow
                                                          ( RExtend
                                                              "max"
                                                              (TVariable (TypeIndex KType 2))
                                                              (RExtend "min" (TVariable (TypeIndex KType 2)) RNil)
                                                          )
                                                      )
                                                  )
                                                  `TArrow` TApplication KType (TConstructor (KArrow KType KType) "Tree") (TVariable (TypeIndex KType 2) :| [])
                                              )
                                              "g"
                                          )
                                      )
                                      ( ERecord
                                          ()
                                          ( TIntrinsic
                                              ( IRecord
                                                  ( TRow
                                                      ( RExtend
                                                          "max"
                                                          (TVariable (TypeIndex KType 2))
                                                          (RExtend "min" (TVariable (TypeIndex KType 2)) RNil)
                                                      )
                                                  )
                                              )
                                          )
                                          ( Map.fromList
                                              [
                                                ( "min"
                                                , EVariable () (Label (TVariable (TypeIndex KType 2)) "p")
                                                )
                                              ,
                                                ( "max"
                                                , ESelect
                                                    ()
                                                    (Label (TVariable (TypeIndex KType 2)) "max")
                                                    ( EVariable
                                                        ()
                                                        ( Label
                                                            ( TIntrinsic
                                                                ( IRecord
                                                                    ( TRow
                                                                        ( RExtend
                                                                            "max"
                                                                            (TVariable (TypeIndex KType 2))
                                                                            (RExtend "min" (TVariable (TypeIndex KType 2)) RNil)
                                                                        )
                                                                    )
                                                                )
                                                            )
                                                            "range"
                                                        )
                                                    )
                                                )
                                              ]
                                          )
                                          Nothing
                                          :| []
                                      )
                                    :| []
                                )
                            )
                            ( EApplication
                                ()
                                ( TApplication
                                    KType
                                    (TConstructor (KArrow KType KType) "Tree")
                                    (TVariable (TypeIndex KType 2) :| [])
                                )
                                ( EVariable
                                    ()
                                    ( Label
                                        ( TIntrinsic
                                            ( IRecord
                                                ( TRow
                                                    ( RExtend
                                                        "max"
                                                        (TVariable (TypeIndex KType 2))
                                                        (RExtend "min" (TVariable (TypeIndex KType 2)) RNil)
                                                    )
                                                )
                                            )
                                            `TArrow` ( TApplication
                                                        KType
                                                        (TConstructor (KArrow KType KType) "Tree")
                                                        (TVariable (TypeIndex KType 2) :| [])
                                                     )
                                        )
                                        "g"
                                    )
                                )
                                ( EVariable
                                    ()
                                    ( Label
                                        ( TIntrinsic
                                            ( IRecord
                                                ( TRow
                                                    ( RExtend
                                                        "max"
                                                        (TVariable (TypeIndex KType 2))
                                                        (RExtend "min" (TVariable (TypeIndex KType 2)) RNil)
                                                    )
                                                )
                                            )
                                        )
                                        "range"
                                    )
                                    :| []
                                )
                            )
                        )
                    )
                    :| []
                )
                <| EClause
                  ()
                  (PListLiteral () (TIntrinsic (IList (TVariable (TypeIndex KType 2)))) [])
                  ( CPlain
                      ()
                      []
                      ( EApplication
                          ()
                          ( ( TIntrinsic
                                ( IRecord
                                    ( TRow
                                        ( RExtend
                                            "max"
                                            (TVariable (TypeIndex KType 2))
                                            (RExtend "min" (TVariable (TypeIndex KType 2)) RNil)
                                        )
                                    )
                                )
                            )
                              `TArrow` ( TApplication
                                          KType
                                          (TConstructor (KArrow KType KType) "Tree")
                                          (TVariable (TypeIndex KType 2) :| [])
                                       )
                          )
                          ( EVariable
                              ()
                              ( Label
                                  ( ( TApplication
                                        KType
                                        (TConstructor (KArrow KType KType) "Tree")
                                        (TVariable (TypeIndex KType 2) :| [])
                                    )
                                      `TArrow` TIntrinsic
                                        ( IRecord
                                            ( TRow
                                                ( RExtend
                                                    "max"
                                                    (TVariable (TypeIndex KType 2))
                                                    (RExtend "min" (TVariable (TypeIndex KType 2)) RNil)
                                                )
                                            )
                                        )
                                      `TArrow` ( TApplication
                                                  KType
                                                  (TConstructor (KArrow KType KType) "Tree")
                                                  (TVariable (TypeIndex KType 2) :| [])
                                               )
                                  )
                                  "always"
                              )
                          )
                          ( EConstructor
                              ()
                              ( Label
                                  ( TApplication
                                      KType
                                      (TConstructor (KArrow KType KType) "Tree")
                                      (TVariable (TypeIndex KType 2) :| [])
                                  )
                                  "Leaf"
                              )
                              :| []
                          )
                      )
                      :| []
                  )
                :| []
            )
            ( Just
                ( ELet
                    ()
                    ( BPattern
                        ()
                        ( PVariable
                            ()
                            ( Label
                                ( TIntrinsic (IList (TVariable (TypeIndex KType 1)))
                                    `TArrow` ( TIntrinsic
                                                ( IRecord
                                                    ( TRow
                                                        ( RExtend
                                                            "max"
                                                            (TVariable (TypeIndex KType 1))
                                                            (RExtend "min" (TVariable (TypeIndex KType 1)) RNil)
                                                        )
                                                    )
                                                )
                                             )
                                    `TArrow` ( TApplication
                                                KType
                                                (TConstructor (KArrow KType KType) "Tree")
                                                (TVariable (TypeIndex KType 1) :| [])
                                             )
                                )
                                "$fold.1"
                            )
                        )
                        ( ELambda
                            ()
                            (PVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 1)))) "$fold.1.expr") :| [])
                            ( ECompiledMatch
                                ()
                                ( ( TIntrinsic
                                      ( IRecord
                                          ( TRow
                                              ( RExtend
                                                  "max"
                                                  (TVariable (TypeIndex KType 1))
                                                  (RExtend "min" (TVariable (TypeIndex KType 1)) RNil)
                                              )
                                          )
                                      )
                                  )
                                    `TArrow` ( TApplication
                                                KType
                                                (TConstructor (KArrow KType KType) "Tree")
                                                (TVariable (TypeIndex KType 1) :| [])
                                             )
                                )
                                (EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 1)))) "$fold.1.expr"))
                                ( ECompiledClause
                                    ( Label
                                        ( TVariable (TypeIndex KType 1)
                                            `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 1)))
                                            `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 1)))
                                        )
                                        "$Cons"
                                        <| Label (TVariable (TypeIndex KType 1)) "$match.10.p"
                                        <| Label (TIntrinsic (IList (TVariable (TypeIndex KType 1)))) "$match.11.g"
                                        :| []
                                    )
                                    ( ELambda
                                        ()
                                        ( PVariable
                                            ()
                                            ( Label
                                                ( TIntrinsic
                                                    ( IRecord
                                                        ( TRow
                                                            ( RExtend
                                                                "max"
                                                                (TVariable (TypeIndex KType 1))
                                                                (RExtend "min" (TVariable (TypeIndex KType 1)) RNil)
                                                            )
                                                        )
                                                    )
                                                )
                                                "range"
                                            )
                                            :| []
                                        )
                                        ( EIf
                                            ()
                                            ( TApplication
                                                KType
                                                (TConstructor (KArrow KType KType) "Tree")
                                                (TVariable (TypeIndex KType 1) :| [])
                                            )
                                            ( EApplication
                                                ()
                                                (TIntrinsic IBool)
                                                ( EBinaryOperator
                                                    ()
                                                    ( TArrow
                                                        (TVariable (TypeIndex KType 1))
                                                        (TArrow (TArrow (TVariable (TypeIndex KType 1)) (TIntrinsic IBool)) (TIntrinsic IBool))
                                                    )
                                                    OReverseApplication
                                                )
                                                ( EVariable () (Label (TVariable (TypeIndex KType 1)) "$match.10.p")
                                                    <| EApplication
                                                      ()
                                                      (TVariable (TypeIndex KType 1) `TArrow` TIntrinsic IBool)
                                                      ( EVariable
                                                          ()
                                                          ( Label
                                                              ( TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TVariable (TypeIndex KType 1) :| [])
                                                                  `TArrow` TApplication KTrait (TConstructor (KArrow KType KTrait) "Ordered") (TVariable (TypeIndex KType 1) :| [])
                                                                  `TArrow` TIntrinsic
                                                                    ( IRecord
                                                                        ( TRow
                                                                            ( RExtend
                                                                                "max"
                                                                                (TVariable (TypeIndex KType 1))
                                                                                (RExtend "min" (TVariable (TypeIndex KType 1)) RNil)
                                                                            )
                                                                        )
                                                                    )
                                                                  `TArrow` TVariable (TypeIndex KType 1)
                                                                  `TArrow` TIntrinsic IBool
                                                              )
                                                              "in_range"
                                                          )
                                                      )
                                                      ( EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TVariable (TypeIndex KType 1) :| [])) "$dict.be194a5d16952b77")
                                                          <| EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Ordered") (TVariable (TypeIndex KType 1) :| [])) "$dict.ffef54c635ab7d02")
                                                          <| EVariable
                                                            ()
                                                            ( Label
                                                                ( TIntrinsic
                                                                    ( IRecord
                                                                        ( TRow
                                                                            ( RExtend
                                                                                "max"
                                                                                (TVariable (TypeIndex KType 1))
                                                                                (RExtend "min" (TVariable (TypeIndex KType 1)) RNil)
                                                                            )
                                                                        )
                                                                    )
                                                                )
                                                                "range"
                                                            )
                                                          :| []
                                                      )
                                                    :| []
                                                )
                                            )
                                            ( EApplication
                                                ()
                                                ( TApplication
                                                    KType
                                                    (TConstructor (KArrow KType KType) "Tree")
                                                    (TVariable (TypeIndex KType 1) :| [])
                                                )
                                                ( EConstructor
                                                    ()
                                                    ( Label
                                                        ( (TVariable (TypeIndex KType 1))
                                                            `TArrow` ( TApplication
                                                                        KType
                                                                        (TConstructor (KArrow KType KType) "Tree")
                                                                        (TVariable (TypeIndex KType 1) :| [])
                                                                     )
                                                            `TArrow` ( TApplication
                                                                        KType
                                                                        (TConstructor (KArrow KType KType) "Tree")
                                                                        (TVariable (TypeIndex KType 1) :| [])
                                                                     )
                                                            `TArrow` ( TApplication
                                                                        KType
                                                                        (TConstructor (KArrow KType KType) "Tree")
                                                                        (TVariable (TypeIndex KType 1) :| [])
                                                                     )
                                                        )
                                                        "Node"
                                                    )
                                                )
                                                ( EVariable () (Label (TVariable (TypeIndex KType 1)) "$match.10.p")
                                                    <| EApplication
                                                      ()
                                                      ( TApplication
                                                          KType
                                                          (TConstructor (KArrow KType KType) "Tree")
                                                          (TVariable (TypeIndex KType 1) :| [])
                                                      )
                                                      ( EVariable
                                                          ()
                                                          ( Label
                                                              ( TIntrinsic (IList (TVariable (TypeIndex KType 1)))
                                                                  `TArrow` ( TIntrinsic
                                                                              ( IRecord
                                                                                  ( TRow
                                                                                      ( RExtend
                                                                                          "max"
                                                                                          (TVariable (TypeIndex KType 1))
                                                                                          (RExtend "min" (TVariable (TypeIndex KType 1)) RNil)
                                                                                      )
                                                                                  )
                                                                              )
                                                                           )
                                                                  `TArrow` ( TApplication
                                                                              KType
                                                                              (TConstructor (KArrow KType KType) "Tree")
                                                                              (TVariable (TypeIndex KType 1) :| [])
                                                                           )
                                                              )
                                                              "$fold.1"
                                                          )
                                                      )
                                                      ( EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 1)))) "$match.11.g")
                                                          :| [ ERecord
                                                                ()
                                                                ( TIntrinsic
                                                                    ( IRecord
                                                                        ( TRow
                                                                            ( RExtend
                                                                                "max"
                                                                                (TVariable (TypeIndex KType 1))
                                                                                (RExtend "min" (TVariable (TypeIndex KType 1)) RNil)
                                                                            )
                                                                        )
                                                                    )
                                                                )
                                                                ( Map.fromList
                                                                    [
                                                                      ( "max"
                                                                      , EVariable () (Label (TVariable (TypeIndex KType 1)) "$match.10.p")
                                                                      )
                                                                    ,
                                                                      ( "min"
                                                                      , ESelect
                                                                          ()
                                                                          (Label (TVariable (TypeIndex KType 1)) "min")
                                                                          ( EVariable
                                                                              ()
                                                                              ( Label
                                                                                  ( TIntrinsic
                                                                                      ( IRecord
                                                                                          ( TRow
                                                                                              ( RExtend
                                                                                                  "max"
                                                                                                  (TVariable (TypeIndex KType 1))
                                                                                                  (RExtend "min" (TVariable (TypeIndex KType 1)) RNil)
                                                                                              )
                                                                                          )
                                                                                      )
                                                                                  )
                                                                                  "range"
                                                                              )
                                                                          )
                                                                      )
                                                                    ]
                                                                )
                                                                Nothing
                                                             ]
                                                      )
                                                    <| EApplication
                                                      ()
                                                      ( TApplication
                                                          KType
                                                          (TConstructor (KArrow KType KType) "Tree")
                                                          (TVariable (TypeIndex KType 1) :| [])
                                                      )
                                                      ( EVariable
                                                          ()
                                                          ( Label
                                                              ( TIntrinsic (IList (TVariable (TypeIndex KType 1)))
                                                                  `TArrow` ( TIntrinsic
                                                                              ( IRecord
                                                                                  ( TRow
                                                                                      ( RExtend
                                                                                          "max"
                                                                                          (TVariable (TypeIndex KType 1))
                                                                                          (RExtend "min" (TVariable (TypeIndex KType 1)) RNil)
                                                                                      )
                                                                                  )
                                                                              )
                                                                           )
                                                                  `TArrow` ( TApplication
                                                                              KType
                                                                              (TConstructor (KArrow KType KType) "Tree")
                                                                              (TVariable (TypeIndex KType 1) :| [])
                                                                           )
                                                              )
                                                              "$fold.1"
                                                          )
                                                      )
                                                      ( EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 1)))) "$match.11.g")
                                                          <| ERecord
                                                            ()
                                                            ( TIntrinsic
                                                                ( IRecord
                                                                    ( TRow
                                                                        ( RExtend
                                                                            "max"
                                                                            (TVariable (TypeIndex KType 1))
                                                                            (RExtend "min" (TVariable (TypeIndex KType 1)) RNil)
                                                                        )
                                                                    )
                                                                )
                                                            )
                                                            ( Map.fromList
                                                                [
                                                                  ( "max"
                                                                  , ESelect
                                                                      ()
                                                                      (Label (TVariable (TypeIndex KType 1)) "max")
                                                                      ( EVariable
                                                                          ()
                                                                          ( Label
                                                                              ( TIntrinsic
                                                                                  ( IRecord
                                                                                      ( TRow
                                                                                          ( RExtend
                                                                                              "max"
                                                                                              (TVariable (TypeIndex KType 1))
                                                                                              (RExtend "min" (TVariable (TypeIndex KType 1)) RNil)
                                                                                          )
                                                                                      )
                                                                                  )
                                                                              )
                                                                              "range"
                                                                          )
                                                                      )
                                                                  )
                                                                ,
                                                                  ( "min"
                                                                  , EVariable () (Label (TVariable (TypeIndex KType 1)) "$match.10.p")
                                                                  )
                                                                ]
                                                            )
                                                            Nothing
                                                          :| []
                                                      )
                                                    :| []
                                                )
                                            )
                                            ( EApplication
                                                ()
                                                ( TApplication
                                                    KType
                                                    (TConstructor (KArrow KType KType) "Tree")
                                                    (TVariable (TypeIndex KType 1) :| [])
                                                )
                                                ( EVariable
                                                    ()
                                                    ( Label
                                                        ( TIntrinsic (IList (TVariable (TypeIndex KType 1)))
                                                            `TArrow` ( TIntrinsic
                                                                        ( IRecord
                                                                            ( TRow
                                                                                ( RExtend
                                                                                    "max"
                                                                                    (TVariable (TypeIndex KType 1))
                                                                                    (RExtend "min" (TVariable (TypeIndex KType 1)) RNil)
                                                                                )
                                                                            )
                                                                        )
                                                                     )
                                                            `TArrow` ( TApplication
                                                                        KType
                                                                        (TConstructor (KArrow KType KType) "Tree")
                                                                        (TVariable (TypeIndex KType 1) :| [])
                                                                     )
                                                        )
                                                        "$fold.1"
                                                    )
                                                )
                                                ( EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 1)))) "$match.11.g")
                                                    <| EVariable
                                                      ()
                                                      ( Label
                                                          ( TIntrinsic
                                                              ( IRecord
                                                                  ( TRow
                                                                      ( RExtend
                                                                          "max"
                                                                          (TVariable (TypeIndex KType 1))
                                                                          (RExtend "min" (TVariable (TypeIndex KType 1)) RNil)
                                                                      )
                                                                  )
                                                              )
                                                          )
                                                          "range"
                                                      )
                                                    :| []
                                                )
                                            )
                                        )
                                    )
                                    <| ECompiledClause
                                      (Label (TIntrinsic (IList (TVariable (TypeIndex KType 1)))) "$Nil" :| [])
                                      ( EApplication
                                          ()
                                          ( ( TIntrinsic
                                                ( IRecord
                                                    ( TRow
                                                        ( RExtend
                                                            "max"
                                                            (TVariable (TypeIndex KType 1))
                                                            (RExtend "min" (TVariable (TypeIndex KType 1)) RNil)
                                                        )
                                                    )
                                                )
                                            )
                                              `TArrow` ( TApplication
                                                          KType
                                                          (TConstructor (KArrow KType KType) "Tree")
                                                          (TVariable (TypeIndex KType 1) :| [])
                                                       )
                                          )
                                          ( EVariable
                                              ()
                                              ( Label
                                                  ( ( TApplication
                                                        KType
                                                        (TConstructor (KArrow KType KType) "Tree")
                                                        (TVariable (TypeIndex KType 1) :| [])
                                                    )
                                                      `TArrow` TIntrinsic
                                                        ( IRecord
                                                            ( TRow
                                                                ( RExtend
                                                                    "max"
                                                                    (TVariable (TypeIndex KType 1))
                                                                    (RExtend "min" (TVariable (TypeIndex KType 1)) RNil)
                                                                )
                                                            )
                                                        )
                                                      `TArrow` ( TApplication
                                                                  KType
                                                                  (TConstructor (KArrow KType KType) "Tree")
                                                                  (TVariable (TypeIndex KType 1) :| [])
                                                               )
                                                  )
                                                  "always"
                                              )
                                          )
                                          ( EConstructor
                                              ()
                                              ( Label
                                                  ( TApplication
                                                      KType
                                                      (TConstructor (KArrow KType KType) "Tree")
                                                      (TVariable (TypeIndex KType 1) :| [])
                                                  )
                                                  "Leaf"
                                              )
                                              :| []
                                          )
                                      )
                                    :| []
                                )
                            )
                        )
                        :| []
                    )
                    ( EApplication
                        ()
                        ( TApplication
                            KType
                            (TConstructor (KArrow KType KType) "Tree")
                            (TVariable (TypeIndex KType 2) :| [])
                        )
                        ( EVariable
                            ()
                            ( Label
                                ( TIntrinsic (IList (TVariable (TypeIndex KType 2)))
                                    `TArrow` ( TIntrinsic
                                                ( IRecord
                                                    ( TRow
                                                        ( RExtend
                                                            "max"
                                                            (TVariable (TypeIndex KType 2))
                                                            (RExtend "min" (TVariable (TypeIndex KType 2)) RNil)
                                                        )
                                                    )
                                                )
                                             )
                                    `TArrow` ( TApplication
                                                KType
                                                (TConstructor (KArrow KType KType) "Tree")
                                                (TVariable (TypeIndex KType 2) :| [])
                                             )
                                )
                                "$fold.1"
                            )
                        )
                        ( EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 2)))) "list")
                            <| ERecord
                              ()
                              ( TIntrinsic
                                  ( IRecord
                                      ( TRow
                                          ( RExtend
                                              "max"
                                              (TVariable (TypeIndex KType 2))
                                              (RExtend "min" (TVariable (TypeIndex KType 2)) RNil)
                                          )
                                      )
                                  )
                              )
                              ( Map.fromList
                                  [
                                    ( "max"
                                    , EApplication
                                        ()
                                        (TVariable (TypeIndex KType 2))
                                        (EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TVariable (TypeIndex KType 2) :| []) `TArrow` TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 2)) "from_int32"))
                                        ( EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TVariable (TypeIndex KType 2) :| [])) "$dict.be194a5d16952b77")
                                            <| ELiteral () (LInt32 (-1))
                                            :| []
                                        )
                                    )
                                  ,
                                    ( "min"
                                    , EApplication
                                        ()
                                        (TVariable (TypeIndex KType 2))
                                        (EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TVariable (TypeIndex KType 2) :| []) `TArrow` TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 2)) "from_int32"))
                                        ( EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TVariable (TypeIndex KType 2) :| [])) "$dict.be194a5d16952b77")
                                            <| ELiteral () (LInt32 0)
                                            :| []
                                        )
                                    )
                                  ]
                              )
                              Nothing
                            :| []
                        )
                    )
                )
            )
        )
    )

binarySearchFromListResult = unsafeParseExpr <$$> binarySearchFromListResult1

binarySearchFromListResult1 =
  [ LP.OFunction
      "BinarySearch.from_list"
      [ Label (LP.TCon "Numeric" [LP.TOpq]) "$dict.be194a5d16952b77"
      , Label (LP.TCon "Ordered" [LP.TOpq]) "$dict.ffef54c635ab7d02"
      , Label (LP.TCon "list" [LP.TOpq]) "list"
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
                                         , $dict.ffef54c635ab7d02 : Ordered(*)
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
  ]

binarySearchFlatten =
  DFunction
    "flatten"
    ( Function
        ()
        ( With
            []
            (TIntrinsic (IList (TVariable (TypeIndex KType 2))))
        )
        ( PAnnotation
            ()
            (TApplication () (TConstructor () "Tree") (TVariable (Parameter () "a") :| []))
            ( PVariable
                ()
                ( Label
                    ( TApplication
                        KType
                        (TConstructor (KArrow KType KType) "Tree")
                        (TVariable (TypeIndex KType 2) :| [])
                    )
                    "tree"
                )
            )
            :| []
        )
        ( EFold
            ()
            (TIntrinsic (IList (TVariable (TypeIndex KType 2))))
            ( EVariable
                ()
                ( Label
                    ( TApplication
                        KType
                        (TConstructor (KArrow KType KType) "Tree")
                        (TVariable (TypeIndex KType 2) :| [])
                    )
                    "tree"
                )
                :| []
            )
            ( EClause
                ()
                ( PConstructor
                    ()
                    ( Label
                        ( TApplication
                            KType
                            (TConstructor (KArrow KType KType) "Tree")
                            (TVariable (TypeIndex KType 2) :| [])
                        )
                        "Node"
                    )
                    [ PVariable () (Label (TVariable (TypeIndex KType 2)) "y")
                    , PAtVariable
                        ()
                        ( Label
                            ( TApplication
                                KType
                                (TConstructor (KArrow KType KType) "Tree")
                                (TVariable (TypeIndex KType 2) :| [])
                            )
                            "lhs"
                        )
                    , PAtVariable
                        ()
                        ( Label
                            ( TApplication
                                KType
                                (TConstructor (KArrow KType KType) "Tree")
                                (TVariable (TypeIndex KType 2) :| [])
                            )
                            "rhs"
                        )
                    ]
                )
                ( CPlain
                    ()
                    []
                    ( EApplication
                        ()
                        (TIntrinsic (IList (TVariable (TypeIndex KType 2))))
                        ( EBinaryOperator
                            ()
                            ( TIntrinsic (IList (TVariable (TypeIndex KType 2)))
                                `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 2)))
                                `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 2)))
                            )
                            OListConcatenation
                        )
                        ( EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 2)))) "lhs")
                            <| EListCons
                              ()
                              (TIntrinsic (IList (TVariable (TypeIndex KType 2))))
                              (EVariable () (Label (TVariable (TypeIndex KType 2)) "y"))
                              (EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 2)))) "rhs"))
                            :| []
                        )
                    )
                    :| []
                )
                <| EClause
                  ()
                  ( PConstructor
                      ()
                      ( Label
                          ( TApplication
                              KType
                              (TConstructor (KArrow KType KType) "Tree")
                              (TVariable (TypeIndex KType 2) :| [])
                          )
                          "Leaf"
                      )
                      []
                  )
                  ( CPlain
                      ()
                      []
                      (EListLiteral () (TIntrinsic (IList (TVariable (TypeIndex KType 2)))) [])
                      :| []
                  )
                :| []
            )
            ( Just
                ( ELet
                    ()
                    ( BPattern
                        ()
                        ( PVariable
                            ()
                            ( Label
                                ( TApplication
                                    KType
                                    (TConstructor (KArrow KType KType) "Tree")
                                    (TVariable (TypeIndex KType 2) :| [])
                                    `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 2)))
                                )
                                "$fold.2"
                            )
                        )
                        ( ELambda
                            ()
                            ( PVariable
                                ()
                                ( Label
                                    ( TApplication
                                        KType
                                        (TConstructor (KArrow KType KType) "Tree")
                                        (TVariable (TypeIndex KType 2) :| [])
                                    )
                                    "$fold.2.expr"
                                )
                                :| []
                            )
                            ( ECompiledMatch
                                ()
                                (TIntrinsic (IList (TVariable (TypeIndex KType 2))))
                                ( EVariable
                                    ()
                                    ( Label
                                        ( TApplication
                                            KType
                                            (TConstructor (KArrow KType KType) "Tree")
                                            (TVariable (TypeIndex KType 2) :| [])
                                        )
                                        "$fold.2.expr"
                                    )
                                )
                                ( ECompiledClause
                                    ( Label
                                        ( TApplication
                                            KType
                                            (TConstructor (KArrow KType KType) "Tree")
                                            (TVariable (TypeIndex KType 2) :| [])
                                        )
                                        "Leaf"
                                        :| []
                                    )
                                    (EListLiteral () (TIntrinsic (IList (TVariable (TypeIndex KType 2)))) [])
                                    <| ECompiledClause
                                      ( Label
                                          ( TVariable (TypeIndex KType 2)
                                              `TArrow` TApplication
                                                KType
                                                (TConstructor (KArrow KType KType) "Tree")
                                                (TVariable (TypeIndex KType 2) :| [])
                                              `TArrow` TApplication
                                                KType
                                                (TConstructor (KArrow KType KType) "Tree")
                                                (TVariable (TypeIndex KType 2) :| [])
                                              `TArrow` TApplication
                                                KType
                                                (TConstructor (KArrow KType KType) "Tree")
                                                (TVariable (TypeIndex KType 2) :| [])
                                          )
                                          "Node"
                                          <| Label (TVariable (TypeIndex KType 2)) "$match.13.y"
                                          <| Label
                                            ( TApplication
                                                KType
                                                (TConstructor (KArrow KType KType) "Tree")
                                                (TVariable (TypeIndex KType 2) :| [])
                                            )
                                            "$match.14.lhs"
                                          <| Label
                                            ( TApplication
                                                KType
                                                (TConstructor (KArrow KType KType) "Tree")
                                                (TVariable (TypeIndex KType 2) :| [])
                                            )
                                            "$match.15.rhs"
                                          :| []
                                      )
                                      ( EApplication
                                          ()
                                          (TIntrinsic (IList (TVariable (TypeIndex KType 2))))
                                          ( EBinaryOperator
                                              ()
                                              ( TIntrinsic (IList (TVariable (TypeIndex KType 2)))
                                                  `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 2)))
                                                  `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 2)))
                                              )
                                              OListConcatenation
                                          )
                                          ( EApplication
                                              ()
                                              (TIntrinsic (IList (TVariable (TypeIndex KType 2))))
                                              ( EVariable
                                                  ()
                                                  ( Label
                                                      ( TApplication
                                                          KType
                                                          (TConstructor (KArrow KType KType) "Tree")
                                                          (TVariable (TypeIndex KType 2) :| [])
                                                          `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 2)))
                                                      )
                                                      "$fold.2"
                                                  )
                                              )
                                              ( EVariable
                                                  ()
                                                  ( Label
                                                      ( TApplication
                                                          KType
                                                          (TConstructor (KArrow KType KType) "Tree")
                                                          (TVariable (TypeIndex KType 2) :| [])
                                                      )
                                                      "$match.14.lhs"
                                                  )
                                                  :| []
                                              )
                                              <| EListCons
                                                ()
                                                (TIntrinsic (IList (TVariable (TypeIndex KType 2))))
                                                (EVariable () (Label (TVariable (TypeIndex KType 2)) "$match.13.y"))
                                                ( EApplication
                                                    ()
                                                    (TIntrinsic (IList (TVariable (TypeIndex KType 2))))
                                                    ( EVariable
                                                        ()
                                                        ( Label
                                                            ( TApplication
                                                                KType
                                                                (TConstructor (KArrow KType KType) "Tree")
                                                                (TVariable (TypeIndex KType 2) :| [])
                                                                `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 2)))
                                                            )
                                                            "$fold.2"
                                                        )
                                                    )
                                                    ( EVariable
                                                        ()
                                                        ( Label
                                                            ( TApplication
                                                                KType
                                                                (TConstructor (KArrow KType KType) "Tree")
                                                                (TVariable (TypeIndex KType 2) :| [])
                                                            )
                                                            "$match.15.rhs"
                                                        )
                                                        :| []
                                                    )
                                                )
                                              :| []
                                          )
                                      )
                                    :| []
                                )
                            )
                        )
                        :| []
                    )
                    ( EApplication
                        ()
                        (TIntrinsic (IList (TVariable (TypeIndex KType 2))))
                        ( EVariable
                            ()
                            ( Label
                                ( TApplication
                                    KType
                                    (TConstructor (KArrow KType KType) "Tree")
                                    (TVariable (TypeIndex KType 2) :| [])
                                    `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 2)))
                                )
                                "$fold.2"
                            )
                        )
                        ( EVariable
                            ()
                            ( Label
                                ( TApplication
                                    KType
                                    (TConstructor (KArrow KType KType) "Tree")
                                    (TVariable (TypeIndex KType 2) :| [])
                                )
                                "tree"
                            )
                            :| []
                        )
                    )
                )
            )
        )
    )

binarySearchFlattenResult = unsafeParseExpr <$$> binarySearchFlattenResult1

binarySearchFlattenResult1 =
  [ LP.OFunction
      "BinarySearch.flatten"
      [ Label (LP.TCon "Tree" [LP.TOpq]) "tree"
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
  ]

binarySearchSort =
  DFunction
    "sort"
    ( Function
        ()
        ( With
            [ Trait "Numeric" (TVariable (TypeIndex KType 3))
            , Trait "Ordered" (TVariable (TypeIndex KType 3))
            ]
            ( TIntrinsic (IList (TVariable (TypeIndex KType 3)))
                `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 3)))
            )
        )
        ( PVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TVariable (TypeIndex KType 3) :| [])) "$dict.be194a5d16952b75")
            <| PVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Ordered") (TVariable (TypeIndex KType 3) :| [])) "$dict.ffef54c635ab7d03")
            :| []
        )
        ( EApplication
            ()
            ( TIntrinsic (IList (TVariable (TypeIndex KType 3)))
                `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 3)))
            )
            ( EBinaryOperator
                ()
                ( ( TApplication
                      KType
                      (TConstructor (KArrow KType KType) "Tree")
                      (TVariable (TypeIndex KType 3) :| [])
                      `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 3)))
                  )
                    `TArrow` ( TIntrinsic (IList (TVariable (TypeIndex KType 3)))
                                `TArrow` TApplication
                                  KType
                                  (TConstructor (KArrow KType KType) "Tree")
                                  (TVariable (TypeIndex KType 3) :| [])
                             )
                    `TArrow` ( TIntrinsic (IList (TVariable (TypeIndex KType 3)))
                                `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 3)))
                             )
                )
                OReverseComposition
            )
            ( EVariable
                ()
                ( Label
                    ( TApplication
                        KType
                        (TConstructor (KArrow KType KType) "Tree")
                        (TVariable (TypeIndex KType 3) :| [])
                        `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 3)))
                    )
                    "flatten"
                )
                <| EApplication
                  ()
                  ( TIntrinsic (IList (TVariable (TypeIndex KType 3)))
                      `TArrow` TApplication
                        KType
                        (TConstructor (KArrow KType KType) "Tree")
                        (TVariable (TypeIndex KType 3) :| [])
                  )
                  ( EVariable
                      ()
                      ( Label
                          ( TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TVariable (TypeIndex KType 3) :| [])
                              `TArrow` TApplication KTrait (TConstructor (KArrow KType KTrait) "Ordered") (TVariable (TypeIndex KType 3) :| [])
                              `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 3)))
                              `TArrow` TApplication KType (TConstructor (KArrow KType KType) "Tree") (TVariable (TypeIndex KType 3) :| [])
                          )
                          "from_list"
                      )
                  )
                  ( EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TVariable (TypeIndex KType 3) :| [])) "$dict.be194a5d16952b75")
                      <| EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Ordered") (TVariable (TypeIndex KType 3) :| [])) "$dict.ffef54c635ab7d03")
                      :| []
                  )
                :| []
            )
        )
    )

binarySearchSortResult = unsafeParseExpr <$$> binarySearchSortResult1

binarySearchSortResult1 =
  [ LP.OFunction
      "BinarySearch.sort"
      [ Label (LP.TCon "Numeric" [LP.TOpq]) "$dict.be194a5d16952b75"
      , Label (LP.TCon "Ordered" [LP.TOpq]) "$dict.ffef54c635ab7d03"
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

mainMain =
  DFunction
    "main"
    ( Function
        ()
        ( With
            []
            (TVariable (TypeIndex KType 0))
        )
        ( PLiteral () LUnit
            :| []
        )
        ( ELet
            ()
            ( BPattern
                ()
                (PVariable () (Label (TIntrinsic (IList (TIntrinsic IInt32))) "xs"))
                ( EAnnotation
                    ()
                    (TIntrinsic (IList (TIntrinsic IInt32)))
                    ( EListLiteral
                        ()
                        (TIntrinsic (IList (TIntrinsic IInt32)))
                        [ EApplication
                            ()
                            (TIntrinsic IInt32)
                            (EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TIntrinsic IInt32 :| []) `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) "from_int32"))
                            ( EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TIntrinsic IInt32 :| [])) "$dict.2967b53e939a3c94")
                                <| ELiteral () (LInt32 5)
                                :| []
                            )
                            --                            , EApplication
                            --                                ()
                            --                                (TIntrinsic IInt32)
                            --                                (EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TIntrinsic IInt32 :| []) `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) "from_int32"))
                            --                                ( EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TIntrinsic IInt32 :| [])) "$dict.2967b53e939a3c94")
                            --                                    <| ELiteral () (LInt32 3)
                            --                                    :| []
                            --                                )
                            --                            , EApplication
                            --                                ()
                            --                                (TIntrinsic IInt32)
                            --                                (EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TIntrinsic IInt32 :| []) `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) "from_int32"))
                            --                                ( EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TIntrinsic IInt32 :| [])) "$dict.2967b53e939a3c94")
                            --                                    <| ELiteral () (LInt32 7)
                            --                                    :| []
                            --                                )
                            --                            , EApplication
                            --                                ()
                            --                                (TIntrinsic IInt32)
                            --                                (EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TIntrinsic IInt32 :| []) `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) "from_int32"))
                            --                                ( EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TIntrinsic IInt32 :| [])) "$dict.2967b53e939a3c94")
                            --                                    <| ELiteral () (LInt32 2)
                            --                                    :| []
                            --                                )
                            --                            , EApplication
                            --                                ()
                            --                                (TIntrinsic IInt32)
                            --                                (EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TIntrinsic IInt32 :| []) `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) "from_int32"))
                            --                                ( EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TIntrinsic IInt32 :| [])) "$dict.2967b53e939a3c94")
                            --                                    <| ELiteral () (LInt32 1)
                            --                                    :| []
                            --                                )
                            --                            , EApplication
                            --                                ()
                            --                                (TIntrinsic IInt32)
                            --                                (EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TIntrinsic IInt32 :| []) `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) "from_int32"))
                            --                                ( EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TIntrinsic IInt32 :| [])) "$dict.2967b53e939a3c94")
                            --                                    <| ELiteral () (LInt32 6)
                            --                                    :| []
                            --                                )
                            --                            , EApplication
                            --                                ()
                            --                                (TIntrinsic IInt32)
                            --                                (EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TIntrinsic IInt32 :| []) `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) "from_int32"))
                            --                                ( EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TIntrinsic IInt32 :| [])) "$dict.2967b53e939a3c94")
                            --                                    <| ELiteral () (LInt32 4)
                            --                                    :| []
                            --                                )
                        ]
                    )
                )
                :| []
            )
            ( EApplication
                ()
                (TVariable (TypeIndex KType 0))
                (EVariable () (Label (TIntrinsic (IList (TIntrinsic IInt32)) `TArrow` TVariable (TypeIndex KType 0)) "trace"))
                ( EApplication
                    ()
                    (TIntrinsic (IList (TIntrinsic IInt32)))
                    (EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TIntrinsic IInt32 :| []) `TArrow` TApplication KTrait (TConstructor (KArrow KType KTrait) "Ordered") (TIntrinsic IInt32 :| []) `TArrow` TIntrinsic (IList (TIntrinsic IInt32)) `TArrow` TIntrinsic (IList (TIntrinsic IInt32))) "sort"))
                    ( EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TIntrinsic IInt32 :| [])) "$dict.2967b53e939a3c94")
                        <| EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Ordered") (TIntrinsic IInt32 :| [])) "$dict.b7c5e7e84eeaf782")
                        <| EVariable () (Label (TIntrinsic (IList (TIntrinsic IInt32))) "xs")
                        :| []
                    )
                    :| []
                )
            )
        )
    )

mainMainResult = unsafeParseExpr <$$> mainMainResult1

mainMainResult1 =
  [ LP.OFunction
      "Main.main"
      [Label (LP.TCon "unit" []) "_"]
      [r|
        let
          xs : list(int32) =
            @<list(int32)>
              ( $Cons : int32/list(int32)/list(int32)
              , @<int32>
                  ( BinarySearch.from_int32 : Numeric(int32)/int32/int32
                  , $dict.2967b53e939a3c94 : Numeric(int32)
                  , 5
                  )
                , $Nil : list(int32) 
              )
          in
            @<*>
              ( trace : list(int32)/*
              , @<list(int32)>
                  ( BinarySearch.sort : Numeric(int32)/Ordered(int32)/list(int32)/list(int32)
                  , $dict.2967b53e939a3c94 : Numeric(int32)
                  , $dict.b7c5e7e84eeaf782 : Ordered(int32)
                  , xs : list(int32)
                  )
              )
      |]
  ]
