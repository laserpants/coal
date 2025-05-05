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
import Noll.Module (Constant (..), Definition (..), Function (..), Module (..), Path (..))
import Text.Megaparsec (eof, runParser)
import Text.Megaparsec.Error (errorBundlePretty)
import Text.RawString.QQ

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Lang.Lowpass.Language as LP
import qualified Noll.Module as Module

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
    "compare"
    [Label LP.int32 "x", Label LP.int32 "y"]
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
    "less_than_or_equal_to"
    [ Label (LP.TCon "Ordered" [LP.TOpq]) "$dict.ffef54c635ab7d00"
    , Label LP.TOpq "m"
    , Label LP.TOpq "n"
    ]
    ( LP.match
        LP.bool
        ( LP.app
            (LP.TCon "Ordering" [])
            (LP.var (Label (LP.TCon "Ordered" [LP.TOpq] `LP.arrow` LP.TOpq `LP.arrow` LP.TOpq `LP.arrow` LP.TCon "Ordering" []) "compare"))
            ( LP.var (Label (LP.TCon "Ordered" [LP.TOpq]) "$dict.ffef54c635ab7d00")
                <| LP.var (Label LP.TOpq "m")
                <| LP.var (Label LP.TOpq "n")
                :| []
            )
        )
        ( LP.Clause
            (Label (LP.TCon "Ordering" []) "EqualTo" :| [])
            (LP.lit (LP.PBool True))
            <| LP.Clause
              (Label (LP.TCon "Ordering" []) "GreaterThan" :| [])
              (LP.lit (LP.PBool False))
            <| LP.Clause
              (Label (LP.TCon "Ordering" []) "LessThan" :| [])
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
      "in_range"
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
            ( Prelude.operator__reverse_composition : (bool/bool)/(*/bool)/*/bool
            , not : bool/bool
            , @<*/bool>
                ( less_than_or_equal_to : Ordered(*)/*/*/bool
                , $dict.ffef54c635ab7d01 : Ordered(*)
                , n : *)
            )
        |]
  ]
