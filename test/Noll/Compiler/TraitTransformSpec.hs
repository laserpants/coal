{-# LANGUAGE OverloadedStrings #-}

module Noll.Compiler.TraitTransformSpec where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Set (Set)
import Lang.Common.Environment (Environment)
import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Noll.Compiler.TraitTransform
import Noll.Language
import Noll.Module (Constant (..), Definition (..), Function (..), Module (..), Path (..))

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Lang.Common.Environment as Environment
import qualified Noll.Module as Module

-- TODO: Use RWS ?
runTraitTransformZ e v s = fst $ runWriter (evalStateT (runReaderT v e) s)

result r = runTraitTransformZ testEnvZ2 (transformConstantZ r) (freshIdIn r)

testEnvZ2 :: Environment (Scheme TypeIndex Kind (Type TypeIndex Kind))
testEnvZ2 =
  Environment.fromList
    [
      ( "from_int32"
      , Forall
          (Set.fromList [TypeIndex KType 0] :: Set (TypeIndex Kind))
          [Trait "Numeric" (TVariable (TypeIndex KType 0))]
          (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 0))
      )
    ,
      ( "greater_than"
      , Forall
          (Set.fromList [TypeIndex KType 0] :: Set (TypeIndex Kind))
          [Trait "Ordered" (TVariable (TypeIndex KType 0))]
          (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool)
      )
    ,
      ( "less_than_or_equal_to"
      , Forall
          (Set.fromList [TypeIndex KType 0] :: Set (TypeIndex Kind))
          [Trait "Ordered" (TVariable (TypeIndex KType 0))]
          (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool)
      )
    ,
      ( "compare"
      , Forall
          (Set.fromList [TypeIndex KType 0] :: Set (TypeIndex Kind))
          [Trait "Ordered" (TVariable (TypeIndex KType 0))]
          (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TConstructor KType "Ordering")
      )
    ,
      ( "from_list"
      , Forall
          (Set.fromList [TypeIndex KType 0] :: Set (TypeIndex Kind))
          [Trait "Numeric" (TVariable (TypeIndex KType 0)), Trait "Ordered" (TVariable (TypeIndex KType 0))]
          (TIntrinsic (IList (TVariable (TypeIndex KType 0))) `TArrow` (TApplication KType (TConstructor (KArrow KType KType) "Tree") (TVariable (TypeIndex KType 0) :| [])))
      )
    ,
      ( "in_range"
      , Forall
          (Set.fromList [TypeIndex KType 0] :: Set (TypeIndex Kind))
          [Trait "Numeric" (TVariable (TypeIndex KType 0)), Trait "Ordered" (TVariable (TypeIndex KType 0))]
          ( TIntrinsic (IRecord (TRow (RExtend "max" (TVariable (TypeIndex KType 0)) (RExtend "min" (TVariable (TypeIndex KType 0)) RNil))))
              `TArrow` TVariable (TypeIndex KType 0)
              `TArrow` TIntrinsic IBool
          )
      )
    ]

fixtureY1 =
  Constant
    ()
    ( With
        []
        (TIntrinsic IInt32)
    )
    ( EApplication
        ()
        (TIntrinsic IInt32)
        (EVariable () (Label (TIntrinsic IInt32 ~> TIntrinsic IInt32) "from_int32"))
        (ELiteral () (LInt32 11) :| [])
    )

fixtureY1r =
  Constant
    ()
    ( With
        []
        (TIntrinsic IInt32)
    )
    ( EDictionaryApplication
        ()
        (TIntrinsic IInt32)
        (EVariable () (Label (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) "from_int32"))
        (Trait "Numeric" (TIntrinsic IInt32) :| [])
        [ELiteral () (LInt32 11)]
    )

fixtureY2 =
  Constant
    ()
    ( With
        []
        (TVariable (TypeIndex KType 0))
    )
    ( EApplication
        ()
        (TVariable (TypeIndex KType 0))
        (EVariable () (Label (TIntrinsic IInt32 ~> TVariable (TypeIndex KType 0)) "from_int32"))
        (ELiteral () (LInt32 11) :| [])
    )

fixtureY2r =
  Constant
    ()
    ( With
        [Trait "Numeric" (TVariable (TypeIndex KType 0))]
        (TVariable (TypeIndex KType 0))
    )
    ( EDictionaryLambda
        ()
        (Trait "Numeric" (TVariable (TypeIndex KType 0)) :| [])
        ( EDictionaryApplication
            ()
            (TVariable (TypeIndex KType 0))
            (EVariable () (Label (TIntrinsic IInt32 ~> TVariable (TypeIndex KType 0)) "from_int32"))
            (Trait "Numeric" (TVariable (TypeIndex KType 0)) :| [])
            [ELiteral () (LInt32 11)]
        )
    )

fixtureX1 =
  Constant
    ()
    ( With
        []
        ( ( TIntrinsic
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
            `TArrow` TVariable (TypeIndex KType 0)
            `TArrow` TIntrinsic IBool
        )
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
        ( ELet
            ()
            ( BPattern
                ()
                (PVariable () (Label (TVariable (TypeIndex KType 0)) "min"))
                ( ESelect
                    ()
                    (Label (TVariable (TypeIndex KType 0)) "min")
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
                )
                <| BPattern
                  ()
                  (PVariable () (Label (TVariable (TypeIndex KType 0)) "max"))
                  ( ESelect
                      ()
                      (Label (TVariable (TypeIndex KType 0)) "max")
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
                  )
                :| []
            )
            ( EApplication
                ()
                (TIntrinsic IBool)
                (EBinaryOperator () (TIntrinsic IBool `TArrow` TIntrinsic IBool `TArrow` TIntrinsic IBool) OLogicalAnd)
                ( EApplication
                    ()
                    (TIntrinsic IBool)
                    (EVariable () (Label (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool) "greater_than"))
                    ( EVariable () (Label (TVariable (TypeIndex KType 0)) "n")
                        <| EVariable () (Label (TVariable (TypeIndex KType 0)) "min")
                        :| []
                    )
                    <| ( EApplication
                          ()
                          (TIntrinsic IBool)
                          (EBinaryOperator () (TIntrinsic IBool `TArrow` TIntrinsic IBool `TArrow` TIntrinsic IBool) OLogicalOr)
                       )
                      ( EApplication
                          ()
                          (TIntrinsic IBool)
                          (EVariable () (Label (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool) "less_than_or_equal_to"))
                          ( EVariable () (Label (TVariable (TypeIndex KType 0)) "n")
                              <| EVariable () (Label (TVariable (TypeIndex KType 0)) "max")
                              :| []
                          )
                          <| EApplication
                            ()
                            (TIntrinsic IBool)
                            ( EBinaryOperator
                                ()
                                (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool)
                                OEqualTo
                            )
                            ( EVariable () (Label (TVariable (TypeIndex KType 0)) "max")
                                <| EApplication
                                  ()
                                  (TVariable (TypeIndex KType 0))
                                  (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 0)) "from_int32"))
                                  (ELiteral () (LInt32 (-1)) :| [])
                                :| []
                            )
                          :| []
                      )
                    :| []
                )
            )
        )
    )

fixtureX2 =
  Constant
    ()
    ( With
        [ Trait "Numeric" (TVariable (TypeIndex KType 0))
        , Trait "Ordered" (TVariable (TypeIndex KType 0))
        ]
        ( ( TIntrinsic
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
            `TArrow` TVariable (TypeIndex KType 0)
            `TArrow` TIntrinsic IBool
        )
    )
    ( EDictionaryLambda
        ()
        ( Trait "Numeric" (TVariable (TypeIndex KType 0))
            <| Trait "Ordered" (TVariable (TypeIndex KType 0))
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
            ( ELet
                ()
                ( BPattern
                    ()
                    (PVariable () (Label (TVariable (TypeIndex KType 0)) "min"))
                    ( ESelect
                        ()
                        (Label (TVariable (TypeIndex KType 0)) "min")
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
                    )
                    <| BPattern
                      ()
                      (PVariable () (Label (TVariable (TypeIndex KType 0)) "max"))
                      ( ESelect
                          ()
                          (Label (TVariable (TypeIndex KType 0)) "max")
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
                      )
                    :| []
                )
                ( EApplication
                    ()
                    (TIntrinsic IBool)
                    (EBinaryOperator () (TIntrinsic IBool `TArrow` TIntrinsic IBool `TArrow` TIntrinsic IBool) OLogicalAnd)
                    ( EDictionaryApplication
                        ()
                        (TIntrinsic IBool)
                        (EVariable () (Label (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool) "greater_than"))
                        (Trait "Ordered" (TVariable (TypeIndex KType 0)) :| [])
                        [ EVariable () (Label (TVariable (TypeIndex KType 0)) "n")
                        , EVariable () (Label (TVariable (TypeIndex KType 0)) "min")
                        ]
                        <| ( EApplication
                              ()
                              (TIntrinsic IBool)
                              (EBinaryOperator () (TIntrinsic IBool `TArrow` TIntrinsic IBool `TArrow` TIntrinsic IBool) OLogicalOr)
                           )
                          ( EDictionaryApplication
                              ()
                              (TIntrinsic IBool)
                              (EVariable () (Label (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool) "less_than_or_equal_to"))
                              (Trait "Ordered" (TVariable (TypeIndex KType 0)) :| [])
                              [ EVariable () (Label (TVariable (TypeIndex KType 0)) "n")
                              , EVariable () (Label (TVariable (TypeIndex KType 0)) "max")
                              ]
                              <| EApplication
                                ()
                                (TIntrinsic IBool)
                                ( EBinaryOperator
                                    ()
                                    (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool)
                                    OEqualTo
                                )
                                ( EVariable () (Label (TVariable (TypeIndex KType 0)) "max")
                                    <| EDictionaryApplication
                                      ()
                                      (TVariable (TypeIndex KType 0))
                                      (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 0)) "from_int32"))
                                      (Trait "Numeric" (TVariable (TypeIndex KType 0)) :| [])
                                      [ELiteral () (LInt32 (-1))]
                                    :| []
                                )
                              :| []
                          )
                        :| []
                    )
                )
            )
        )
    )

-- less_than_or_equal_to
funLte =
  Constant
    ()
    (With [] (TVariable (TypeIndex KType 1) `TArrow` TVariable (TypeIndex KType 1) `TArrow` TIntrinsic IBool))
    ( ELambda
        ()
        ( PVariable () (Label (TVariable (TypeIndex KType 1)) "m")
            <| PVariable () (Label (TVariable (TypeIndex KType 1)) "n")
            :| []
        )
        ( ECompiledMatch
            ()
            (TIntrinsic IBool)
            ( EApplication
                ()
                (TConstructor KType "Ordering")
                (EVariable () (Label (TVariable (TypeIndex KType 1) `TArrow` TVariable (TypeIndex KType 1) `TArrow` TConstructor KType "Ordering") "compare"))
                ( EVariable () (Label (TVariable (TypeIndex KType 1)) "m")
                    <| EVariable () (Label (TVariable (TypeIndex KType 1)) "n")
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

funLte2 =
  Constant
    ()
    ( With
        [Trait "Ordered" (TVariable (TypeIndex KType 1))]
        (TVariable (TypeIndex KType 1) `TArrow` TVariable (TypeIndex KType 1) `TArrow` TIntrinsic IBool)
    )
    ( EDictionaryLambda
        ()
        ( Trait "Ordered" (TVariable (TypeIndex KType 1))
            :| []
        )
        ( ELambda
            ()
            ( PVariable () (Label (TVariable (TypeIndex KType 1)) "m")
                <| PVariable () (Label (TVariable (TypeIndex KType 1)) "n")
                :| []
            )
            ( ECompiledMatch
                ()
                (TIntrinsic IBool)
                ( EDictionaryApplication
                    ()
                    (TConstructor KType "Ordering")
                    (EVariable () (Label (TVariable (TypeIndex KType 1) `TArrow` TVariable (TypeIndex KType 1) `TArrow` TConstructor KType "Ordering") "compare"))
                    ( Trait "Ordered" (TVariable (TypeIndex KType 1))
                        :| []
                    )
                    [ EVariable () (Label (TVariable (TypeIndex KType 1)) "m")
                    , EVariable () (Label (TVariable (TypeIndex KType 1)) "n")
                    ]
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
    )

-- greater_than
funGt =
  Constant
    ()
    (With [] (TVariable (TypeIndex KType 2) `TArrow` TVariable (TypeIndex KType 2) `TArrow` TIntrinsic IBool))
    ( ELambda
        ()
        ( PAnnotation
            ()
            (TVariable (Parameter () "a"))
            (PVariable () (Label (TVariable (TypeIndex KType 2)) "n"))
            :| []
        )
        ( EApplication
            ()
            (TVariable (TypeIndex KType 2) `TArrow` TIntrinsic IBool)
            ( EBinaryOperator
                ()
                ( (TIntrinsic IBool `TArrow` TIntrinsic IBool)
                    `TArrow` (TVariable (TypeIndex KType 2) `TArrow` TIntrinsic IBool)
                    `TArrow` TVariable (TypeIndex KType 2)
                    `TArrow` TIntrinsic IBool
                )
                OReverseComposition
            )
            ( EVariable () (Label (TIntrinsic IBool `TArrow` TIntrinsic IBool) "not")
                <| EApplication
                  ()
                  (TVariable (TypeIndex KType 2) `TArrow` TIntrinsic IBool)
                  (EVariable () (Label (TVariable (TypeIndex KType 2) `TArrow` TVariable (TypeIndex KType 2) `TArrow` TIntrinsic IBool) "less_than_or_equal_to"))
                  (EVariable () (Label (TVariable (TypeIndex KType 2)) "n") :| [])
                :| []
            )
        )
    )

funGt2 =
  Constant
    ()
    ( With
        [Trait "Ordered" (TVariable (TypeIndex KType 2))]
        (TVariable (TypeIndex KType 2) `TArrow` TVariable (TypeIndex KType 2) `TArrow` TIntrinsic IBool)
    )
    ( EDictionaryLambda
        ()
        (Trait "Ordered" (TVariable (TypeIndex KType 2)) :| [])
        ( ELambda
            ()
            ( PAnnotation
                ()
                (TVariable (Parameter () "a"))
                (PVariable () (Label (TVariable (TypeIndex KType 2)) "n"))
                :| []
            )
            ( EApplication
                ()
                (TVariable (TypeIndex KType 2) `TArrow` TIntrinsic IBool)
                ( EBinaryOperator
                    ()
                    ( (TIntrinsic IBool `TArrow` TIntrinsic IBool)
                        `TArrow` (TVariable (TypeIndex KType 2) `TArrow` TIntrinsic IBool)
                        `TArrow` TVariable (TypeIndex KType 2)
                        `TArrow` TIntrinsic IBool
                    )
                    OReverseComposition
                )
                ( EVariable () (Label (TIntrinsic IBool `TArrow` TIntrinsic IBool) "not")
                    <| EDictionaryApplication
                      ()
                      (TVariable (TypeIndex KType 2) `TArrow` TIntrinsic IBool)
                      (EVariable () (Label (TVariable (TypeIndex KType 2) `TArrow` TVariable (TypeIndex KType 2) `TArrow` TIntrinsic IBool) "less_than_or_equal_to"))
                      (Trait "Ordered" (TVariable (TypeIndex KType 2)) :| [])
                      [EVariable () (Label (TVariable (TypeIndex KType 2)) "n")]
                    :| []
                )
            )
        )
    )

-- in_range

funInRange =
  Constant
      ()
      ( With
          []
          ( ( TIntrinsic
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
              `TArrow` TVariable (TypeIndex KType 0)
              `TArrow` TIntrinsic IBool
          )
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
          ( ELet
              ()
              ( BPattern
                  ()
                  (PVariable () (Label (TVariable (TypeIndex KType 0)) "min"))
                  ( ESelect
                      ()
                      (Label (TVariable (TypeIndex KType 0)) "min")
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
                  )
                  <| BPattern
                    ()
                    (PVariable () (Label (TVariable (TypeIndex KType 0)) "max"))
                    ( ESelect
                        ()
                        (Label (TVariable (TypeIndex KType 0)) "max")
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
                    )
                  :| []
              )
              ( EApplication
                  ()
                  (TIntrinsic IBool)
                  (EBinaryOperator () (TIntrinsic IBool `TArrow` TIntrinsic IBool `TArrow` TIntrinsic IBool) OLogicalAnd)
                  ( EApplication
                      ()
                      (TIntrinsic IBool)
                      (EVariable () (Label (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool) "greater_than"))
                      ( EVariable () (Label (TVariable (TypeIndex KType 0)) "n")
                          <| EVariable () (Label (TVariable (TypeIndex KType 0)) "min")
                          :| []
                      )
                      <| ( EApplication
                            ()
                            (TIntrinsic IBool)
                            (EBinaryOperator () (TIntrinsic IBool `TArrow` TIntrinsic IBool `TArrow` TIntrinsic IBool) OLogicalOr)
                         )
                        ( EApplication
                            ()
                            (TIntrinsic IBool)
                            (EVariable () (Label (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool) "less_than_or_equal_to"))
                            ( EVariable () (Label (TVariable (TypeIndex KType 0)) "n")
                                <| EVariable () (Label (TVariable (TypeIndex KType 0)) "max")
                                :| []
                            )
                            <| EApplication
                              ()
                              (TIntrinsic IBool)
                              ( EBinaryOperator
                                  ()
                                  (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool)
                                  OEqualTo
                              )
                              ( EVariable () (Label (TVariable (TypeIndex KType 0)) "max")
                                  <| EApplication
                                    ()
                                    (TVariable (TypeIndex KType 0))
                                    (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 0)) "from_int32"))
                                    (ELiteral () (LInt32 (-1)) :| [])
                                  :| []
                              )
                            :| []
                        )
                      :| []
                  )
              )
          )
      )

funInRange2 =
  ( Constant
      ()
      ( With
          [ Trait "Numeric" (TVariable (TypeIndex KType 0))
          , Trait "Ordered" (TVariable (TypeIndex KType 0))
          ]
          ( ( TIntrinsic
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
              `TArrow` TVariable (TypeIndex KType 0)
              `TArrow` TIntrinsic IBool
          )
      )
      ( EDictionaryLambda
          ()
          ( Trait "Numeric" (TVariable (TypeIndex KType 0))
              <| Trait "Ordered" (TVariable (TypeIndex KType 0))
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
              ( ELet
                  ()
                  ( BPattern
                      ()
                      (PVariable () (Label (TVariable (TypeIndex KType 0)) "min"))
                      ( ESelect
                          ()
                          (Label (TVariable (TypeIndex KType 0)) "min")
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
                      )
                      <| BPattern
                        ()
                        (PVariable () (Label (TVariable (TypeIndex KType 0)) "max"))
                        ( ESelect
                            ()
                            (Label (TVariable (TypeIndex KType 0)) "max")
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
                        )
                      :| []
                  )
                  ( EApplication
                      ()
                      (TIntrinsic IBool)
                      (EBinaryOperator () (TIntrinsic IBool `TArrow` TIntrinsic IBool `TArrow` TIntrinsic IBool) OLogicalAnd)
                      ( EDictionaryApplication
                          ()
                          (TIntrinsic IBool)
                          (EVariable () (Label (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool) "greater_than"))
                          (Trait "Ordered" (TVariable (TypeIndex KType 0)) :| [])
                          [ EVariable () (Label (TVariable (TypeIndex KType 0)) "n")
                          , EVariable () (Label (TVariable (TypeIndex KType 0)) "min")
                          ]
                          <| ( EApplication
                                ()
                                (TIntrinsic IBool)
                                (EBinaryOperator () (TIntrinsic IBool `TArrow` TIntrinsic IBool `TArrow` TIntrinsic IBool) OLogicalOr)
                             )
                            ( EDictionaryApplication
                                ()
                                (TIntrinsic IBool)
                                (EVariable () (Label (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool) "less_than_or_equal_to"))
                                (Trait "Ordered" (TVariable (TypeIndex KType 0)) :| [])
                                [ EVariable () (Label (TVariable (TypeIndex KType 0)) "n")
                                , EVariable () (Label (TVariable (TypeIndex KType 0)) "max")
                                ]
                                <| EApplication
                                  ()
                                  (TIntrinsic IBool)
                                  ( EBinaryOperator
                                      ()
                                      (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool)
                                      OEqualTo
                                  )
                                  ( EVariable () (Label (TVariable (TypeIndex KType 0)) "max")
                                      <| EDictionaryApplication
                                        ()
                                        (TVariable (TypeIndex KType 0))
                                        (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 0)) "from_int32"))
                                        (Trait "Numeric" (TVariable (TypeIndex KType 0)) :| [])
                                        [ELiteral () (LInt32 (-1))]
                                      :| []
                                  )
                                :| []
                            )
                          :| []
                      )
                  )
              )
          )
      )
  )

-- from_list
funFromList =
  ( Constant
      ()
      (With [] (TIntrinsic (IList (TVariable (TypeIndex KType 2))) `TArrow` TApplication KType (TConstructor (KArrow KType KType) "Tree") (TVariable (TypeIndex KType 2) :| [])))
      ( ELambda
          ()
          ( PAnnotation
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
                              (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 2)) "from_int32"))
                              (ELiteral () (LInt32 0) :| [])
                          )
                        ,
                          ( "max"
                          , EApplication
                              ()
                              (TVariable (TypeIndex KType 2))
                              (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 2)) "from_int32"))
                              (ELiteral () (LInt32 (-1)) :| [])
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
                                                    `TArrow` TVariable (TypeIndex KType 2)
                                                    `TArrow` TIntrinsic IBool
                                                )
                                                "in_range"
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
                  ( ERecursiveLet
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
                                      <| Label (TVariable (TypeIndex KType 1)) "$match.3.p"
                                      <| Label (TIntrinsic (IList (TVariable (TypeIndex KType 1)))) "$match.4.g"
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
                                              ( EVariable () (Label (TVariable (TypeIndex KType 1)) "$match.3.p")
                                                  <| EApplication
                                                    ()
                                                    (TVariable (TypeIndex KType 1) `TArrow` TIntrinsic IBool)
                                                    ( EVariable
                                                        ()
                                                        ( Label
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
                                                                `TArrow` (TVariable (TypeIndex KType 1))
                                                                `TArrow` (TIntrinsic IBool)
                                                            )
                                                            "in_range"
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
                                              ( EVariable () (Label (TVariable (TypeIndex KType 1)) "$match.3.p")
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
                                                    ( EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 1)))) "$match.4.g")
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
                                                                    , EVariable () (Label (TVariable (TypeIndex KType 1)) "$match.3.p")
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
                                                    ( EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 1)))) "$match.4.g")
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
                                                                , EVariable () (Label (TVariable (TypeIndex KType 1)) "$match.3.p")
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
                                              ( EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 1)))) "$match.4.g")
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
                                          (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 2)) "from_int32"))
                                          (ELiteral () (LInt32 (-1)) :| [])
                                      )
                                    ,
                                      ( "min"
                                      , EApplication
                                          ()
                                          (TVariable (TypeIndex KType 2))
                                          (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 2)) "from_int32"))
                                          (ELiteral () (LInt32 0) :| [])
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
  )

funFromList2 =
  ( Constant
      ()
      ( With
          [ Trait "Numeric" (TVariable (TypeIndex KType 2))
          , Trait "Ordered" (TVariable (TypeIndex KType 2))
          ]
          (TIntrinsic (IList (TVariable (TypeIndex KType 2))) `TArrow` TApplication KType (TConstructor (KArrow KType KType) "Tree") (TVariable (TypeIndex KType 2) :| []))
      )
      ( EDictionaryLambda
          ()
          ( Trait "Numeric" (TVariable (TypeIndex KType 2))
              <| Trait "Ordered" (TVariable (TypeIndex KType 2))
              :| []
          )
          ( ELambda
              ()
              ( PAnnotation
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
                              , EDictionaryApplication
                                  ()
                                  (TVariable (TypeIndex KType 2))
                                  (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 2)) "from_int32"))
                                  (Trait "Numeric" (TVariable (TypeIndex KType 2)) :| [])
                                  [ELiteral () (LInt32 0)]
                              )
                            ,
                              ( "max"
                              , EDictionaryApplication
                                  ()
                                  (TVariable (TypeIndex KType 2))
                                  (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 2)) "from_int32"))
                                  (Trait "Numeric" (TVariable (TypeIndex KType 2)) :| [])
                                  [ELiteral () (LInt32 (-1))]
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
                                          <| EDictionaryApplication
                                            ()
                                            (TVariable (TypeIndex KType 2) `TArrow` TIntrinsic IBool)
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
                                                        `TArrow` TVariable (TypeIndex KType 2)
                                                        `TArrow` TIntrinsic IBool
                                                    )
                                                    "in_range"
                                                )
                                            )
                                            ( Trait "Numeric" (TVariable (TypeIndex KType 2))
                                                <| Trait "Ordered" (TVariable (TypeIndex KType 2))
                                                :| []
                                            )
                                            [ EVariable
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
                                            ]
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
                                              <| Label (TVariable (TypeIndex KType 1)) "$match.3.p"
                                              <| Label (TIntrinsic (IList (TVariable (TypeIndex KType 1)))) "$match.4.g"
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
                                                      ( EVariable () (Label (TVariable (TypeIndex KType 1)) "$match.3.p")
                                                          <| EDictionaryApplication
                                                            ()
                                                            (TVariable (TypeIndex KType 1) `TArrow` TIntrinsic IBool)
                                                            ( EVariable
                                                                ()
                                                                ( Label
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
                                                                        `TArrow` (TVariable (TypeIndex KType 1))
                                                                        `TArrow` (TIntrinsic IBool)
                                                                    )
                                                                    "in_range"
                                                                )
                                                            )
                                                            ( Trait "Numeric" (TVariable (TypeIndex KType 1))
                                                                <| Trait "Ordered" (TVariable (TypeIndex KType 1))
                                                                :| []
                                                            )
                                                            [ EVariable
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
                                                            ]
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
                                                      ( EVariable () (Label (TVariable (TypeIndex KType 1)) "$match.3.p")
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
                                                            ( EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 1)))) "$match.4.g")
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
                                                                            , EVariable () (Label (TVariable (TypeIndex KType 1)) "$match.3.p")
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
                                                            ( EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 1)))) "$match.4.g")
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
                                                                        , EVariable () (Label (TVariable (TypeIndex KType 1)) "$match.3.p")
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
                                                      ( EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 1)))) "$match.4.g")
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
                                          , EDictionaryApplication
                                              ()
                                              (TVariable (TypeIndex KType 2))
                                              (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 2)) "from_int32"))
                                              (Trait "Numeric" (TVariable (TypeIndex KType 2)) :| [])
                                              [ELiteral () (LInt32 (-1))]
                                          )
                                        ,
                                          ( "min"
                                          , EDictionaryApplication
                                              ()
                                              (TVariable (TypeIndex KType 2))
                                              (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 2)) "from_int32"))
                                              (Trait "Numeric" (TVariable (TypeIndex KType 2)) :| [])
                                              [ELiteral () (LInt32 0)]
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
      )
  )

-- sort
funSort =
  ( Constant
      ()
      ( With
          []
          ( TIntrinsic (IList (TVariable (TypeIndex KType 5)))
              `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 5)))
          )
      )
      ( EApplication
          ()
          ( TIntrinsic (IList (TVariable (TypeIndex KType 5)))
              `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 5)))
          )
          ( EBinaryOperator
              ()
              ( ( ( TApplication
                      KType
                      (TConstructor (KArrow KType KType) "Tree")
                      (TVariable (TypeIndex KType 5) :| [])
                  )
                    `TArrow` (TIntrinsic (IList (TVariable (TypeIndex KType 5))))
                )
                  `TArrow` ( (TIntrinsic (IList (TVariable (TypeIndex KType 5))))
                              `TArrow` ( TApplication
                                          KType
                                          (TConstructor (KArrow KType KType) "Tree")
                                          (TVariable (TypeIndex KType 5) :| [])
                                       )
                           )
                  `TArrow` ( (TIntrinsic (IList (TVariable (TypeIndex KType 5))))
                              `TArrow` (TIntrinsic (IList (TVariable (TypeIndex KType 5))))
                           )
              )
              OReverseComposition
          )
          ( EVariable
              ()
              ( Label
                  ( ( TApplication
                        KType
                        (TConstructor (KArrow KType KType) "Tree")
                        (TVariable (TypeIndex KType 5) :| [])
                    )
                      `TArrow` (TIntrinsic (IList (TVariable (TypeIndex KType 5))))
                  )
                  "flatten"
              )
              <| EVariable
                ()
                ( Label
                    ( (TIntrinsic (IList (TVariable (TypeIndex KType 5))))
                        `TArrow` ( TApplication
                                    KType
                                    (TConstructor (KArrow KType KType) "Tree")
                                    (TVariable (TypeIndex KType 5) :| [])
                                 )
                    )
                    "from_list"
                )
              :| []
          )
      )
  )

funSort2 =
  ( Constant
      ()
      ( With
          [ Trait "Numeric" (TVariable (TypeIndex KType 5))
          , Trait "Ordered" (TVariable (TypeIndex KType 5))
          ]
          ( TIntrinsic (IList (TVariable (TypeIndex KType 5)))
              `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 5)))
          )
      )
      ( EDictionaryLambda
          ()
          ( Trait "Numeric" (TVariable (TypeIndex KType 5))
              <| Trait "Ordered" (TVariable (TypeIndex KType 5))
              :| []
          )
          ( EApplication
              ()
              ( TIntrinsic (IList (TVariable (TypeIndex KType 5)))
                  `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 5)))
              )
              ( EBinaryOperator
                  ()
                  ( ( ( TApplication
                          KType
                          (TConstructor (KArrow KType KType) "Tree")
                          (TVariable (TypeIndex KType 5) :| [])
                      )
                        `TArrow` (TIntrinsic (IList (TVariable (TypeIndex KType 5))))
                    )
                      `TArrow` ( (TIntrinsic (IList (TVariable (TypeIndex KType 5))))
                                  `TArrow` ( TApplication
                                              KType
                                              (TConstructor (KArrow KType KType) "Tree")
                                              (TVariable (TypeIndex KType 5) :| [])
                                           )
                               )
                      `TArrow` ( (TIntrinsic (IList (TVariable (TypeIndex KType 5))))
                                  `TArrow` (TIntrinsic (IList (TVariable (TypeIndex KType 5))))
                               )
                  )
                  OReverseComposition
              )
              ( EVariable
                  ()
                  ( Label
                      ( ( TApplication
                            KType
                            (TConstructor (KArrow KType KType) "Tree")
                            (TVariable (TypeIndex KType 5) :| [])
                        )
                          `TArrow` (TIntrinsic (IList (TVariable (TypeIndex KType 5))))
                      )
                      "flatten"
                  )
                  <| EDictionaryApplication
                    ()
                    ( (TIntrinsic (IList (TVariable (TypeIndex KType 5))))
                        `TArrow` ( TApplication
                                    KType
                                    (TConstructor (KArrow KType KType) "Tree")
                                    (TVariable (TypeIndex KType 5) :| [])
                                 )
                    )
                    (EVariable () (Label ((TIntrinsic (IList (TVariable (TypeIndex KType 5)))) `TArrow` (TApplication KType (TConstructor (KArrow KType KType) "Tree") (TVariable (TypeIndex KType 5) :| []))) "from_list"))
                    ( Trait "Numeric" (TVariable (TypeIndex KType 5))
                        <| Trait "Ordered" (TVariable (TypeIndex KType 5))
                        :| []
                    )
                    []
                  :| []
              )
          )
      )
  )

-- flatten

-- in_range

funInRangeA =
  Constant
    ()
    ( With
        []
        ( ( TIntrinsic
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
            `TArrow` TVariable (TypeIndex KType 0)
            `TArrow` TIntrinsic IBool
        )
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
                    ( ( TRow
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
                    <| ( Label
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
                                            <| (Label (TRow RNil) "$match.2._")
                                            :| []
                                        )
                                        ( EApplication
                                            ()
                                            (TIntrinsic IBool)
                                            (EBinaryOperator () (TIntrinsic IBool `TArrow` TIntrinsic IBool `TArrow` TIntrinsic IBool) OLogicalAnd)
                                            ( EApplication
                                                ()
                                                (TIntrinsic IBool)
                                                (EVariable () (Label (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool) "greater_than"))
                                                ( EVariable () (Label (TVariable (TypeIndex KType 0)) "n")
                                                    <| EVariable () (Label (TVariable (TypeIndex KType 0)) "$row.2.field.min")
                                                    :| []
                                                )
                                                <| ( EApplication
                                                      ()
                                                      (TIntrinsic IBool)
                                                      (EBinaryOperator () (TIntrinsic IBool `TArrow` TIntrinsic IBool `TArrow` TIntrinsic IBool) OLogicalOr)
                                                   )
                                                  ( EApplication
                                                      ()
                                                      (TIntrinsic IBool)
                                                      (EVariable () (Label (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool) "less_than_or_equal_to"))
                                                      ( EVariable () (Label (TVariable (TypeIndex KType 0)) "n")
                                                          <| EVariable () (Label (TVariable (TypeIndex KType 0)) "$row.1.field.max")
                                                          :| []
                                                      )
                                                      <| EApplication
                                                        ()
                                                        (TIntrinsic IBool)
                                                        ( EBinaryOperator
                                                            ()
                                                            (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool)
                                                            OEqualTo
                                                        )
                                                        ( EVariable () (Label (TVariable (TypeIndex KType 0)) "$row.1.field.max")
                                                            <| EApplication
                                                              ()
                                                              (TVariable (TypeIndex KType 0))
                                                              (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 0)) "from_int32"))
                                                              (ELiteral () (LInt32 (-1)) :| [])
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

funInRangeA2 =
  Constant
    ()
    ( With
        [ Trait "Numeric" (TVariable (TypeIndex KType 0))
        , Trait "Ordered" (TVariable (TypeIndex KType 0))
        ]
        ( ( TIntrinsic
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
            `TArrow` TVariable (TypeIndex KType 0)
            `TArrow` TIntrinsic IBool
        )
    )
    ( EDictionaryLambda
        ()
        ( Trait "Numeric" (TVariable (TypeIndex KType 0))
            <| Trait "Ordered" (TVariable (TypeIndex KType 0))
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
                        ( ( TRow
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
                        <| ( Label
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
                                                <| (Label (TRow RNil) "$match.2._")
                                                :| []
                                            )
                                            ( EApplication
                                                ()
                                                (TIntrinsic IBool)
                                                (EBinaryOperator () (TIntrinsic IBool `TArrow` TIntrinsic IBool `TArrow` TIntrinsic IBool) OLogicalAnd)
                                                ( EDictionaryApplication
                                                    ()
                                                    (TIntrinsic IBool)
                                                    (EVariable () (Label (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool) "greater_than"))
                                                    (Trait "Ordered" (TVariable (TypeIndex KType 0)) :| [])
                                                    [ EVariable () (Label (TVariable (TypeIndex KType 0)) "n")
                                                    , EVariable () (Label (TVariable (TypeIndex KType 0)) "$row.2.field.min")
                                                    ]
                                                    <| ( EApplication
                                                          ()
                                                          (TIntrinsic IBool)
                                                          (EBinaryOperator () (TIntrinsic IBool `TArrow` TIntrinsic IBool `TArrow` TIntrinsic IBool) OLogicalOr)
                                                       )
                                                      ( EDictionaryApplication
                                                          ()
                                                          (TIntrinsic IBool)
                                                          (EVariable () (Label (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool) "less_than_or_equal_to"))
                                                          (Trait "Ordered" (TVariable (TypeIndex KType 0)) :| [])
                                                          [ EVariable () (Label (TVariable (TypeIndex KType 0)) "n")
                                                          , EVariable () (Label (TVariable (TypeIndex KType 0)) "$row.1.field.max")
                                                          ]
                                                          <| EApplication
                                                            ()
                                                            (TIntrinsic IBool)
                                                            ( EBinaryOperator
                                                                ()
                                                                (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool)
                                                                OEqualTo
                                                            )
                                                            ( EVariable () (Label (TVariable (TypeIndex KType 0)) "$row.1.field.max")
                                                                <| EDictionaryApplication
                                                                  ()
                                                                  (TVariable (TypeIndex KType 0))
                                                                  (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 0)) "from_int32"))
                                                                  (Trait "Numeric" (TVariable (TypeIndex KType 0)) :| [])
                                                                  [ELiteral () (LInt32 (-1))]
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
    )

funFlatten =
  ( Constant
      ()
      ( With
          []
          ( ( TApplication
                KType
                (TConstructor (KArrow KType KType) "Tree")
                (TVariable (TypeIndex KType 4) :| [])
            )
              `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 4)))
          )
      )
      ( ELambda
          ()
          ( PAnnotation
              ()
              (TApplication () (TConstructor () "Tree") (TVariable (Parameter () "a") :| []))
              ( PVariable
                  ()
                  ( Label
                      ( TApplication
                          KType
                          (TConstructor (KArrow KType KType) "Tree")
                          (TVariable (TypeIndex KType 4) :| [])
                      )
                      "tree"
                  )
              )
              :| []
          )
          ( EFold
              ()
              (TIntrinsic (IList (TVariable (TypeIndex KType 4))))
              ( EVariable
                  ()
                  ( Label
                      ( TApplication
                          KType
                          (TConstructor (KArrow KType KType) "Tree")
                          (TVariable (TypeIndex KType 4) :| [])
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
                              (TVariable (TypeIndex KType 4) :| [])
                          )
                          "Node"
                      )
                      [ PVariable () (Label (TVariable (TypeIndex KType 4)) "y")
                      , PAtVariable
                          ()
                          ( Label
                              ( TApplication
                                  KType
                                  (TConstructor (KArrow KType KType) "Tree")
                                  (TVariable (TypeIndex KType 4) :| [])
                              )
                              "lhs"
                          )
                      , PAtVariable
                          ()
                          ( Label
                              ( TApplication
                                  KType
                                  (TConstructor (KArrow KType KType) "Tree")
                                  (TVariable (TypeIndex KType 4) :| [])
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
                          (TIntrinsic (IList (TVariable (TypeIndex KType 4))))
                          ( EBinaryOperator
                              ()
                              ( (TIntrinsic (IList (TVariable (TypeIndex KType 4))))
                                  `TArrow` (TIntrinsic (IList (TVariable (TypeIndex KType 4))))
                                  `TArrow` (TIntrinsic (IList (TVariable (TypeIndex KType 4))))
                              )
                              OListConcatenation
                          )
                          ( EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 4)))) "lhs")
                              <| EListCons
                                ()
                                (TIntrinsic (IList (TVariable (TypeIndex KType 4))))
                                (EVariable () (Label (TVariable (TypeIndex KType 4)) "y"))
                                (EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 4)))) "rhs"))
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
                                (TVariable (TypeIndex KType 4) :| [])
                            )
                            "Leaf"
                        )
                        []
                    )
                    ( CPlain
                        ()
                        []
                        (EListLiteral () (TIntrinsic (IList (TVariable (TypeIndex KType 4)))) [])
                        :| []
                    )
                  :| []
              )
              ( Just
                  ( ERecursiveLet
                      ()
                      ( PVariable
                          ()
                          ( Label
                              ( ( TApplication
                                    KType
                                    (TConstructor (KArrow KType KType) "Tree")
                                    (TVariable (TypeIndex KType 3) :| [])
                                )
                                  `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 3)))
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
                                      (TVariable (TypeIndex KType 3) :| [])
                                  )
                                  "$fold.2.expr"
                              )
                              :| []
                          )
                          ( ECompiledMatch
                              ()
                              (TIntrinsic (IList (TVariable (TypeIndex KType 3))))
                              ( EVariable
                                  ()
                                  ( Label
                                      ( TApplication
                                          KType
                                          (TConstructor (KArrow KType KType) "Tree")
                                          (TVariable (TypeIndex KType 3) :| [])
                                      )
                                      "$fold.2.expr"
                                  )
                              )
                              ( ECompiledClause
                                  ( Label
                                      ( TApplication
                                          KType
                                          (TConstructor (KArrow KType KType) "Tree")
                                          (TVariable (TypeIndex KType 3) :| [])
                                      )
                                      "Leaf"
                                      :| []
                                  )
                                  (EListLiteral () (TIntrinsic (IList (TVariable (TypeIndex KType 3)))) [])
                                  <| ECompiledClause
                                    ( Label
                                        ( TVariable (TypeIndex KType 3)
                                            `TArrow` TApplication
                                              KType
                                              (TConstructor (KArrow KType KType) "Tree")
                                              (TVariable (TypeIndex KType 3) :| [])
                                            `TArrow` TApplication
                                              KType
                                              (TConstructor (KArrow KType KType) "Tree")
                                              (TVariable (TypeIndex KType 3) :| [])
                                            `TArrow` TApplication
                                              KType
                                              (TConstructor (KArrow KType KType) "Tree")
                                              (TVariable (TypeIndex KType 3) :| [])
                                        )
                                        "Node"
                                        <| Label (TVariable (TypeIndex KType 3)) "$match.13.y"
                                        <| Label
                                          ( TApplication
                                              KType
                                              (TConstructor (KArrow KType KType) "Tree")
                                              (TVariable (TypeIndex KType 3) :| [])
                                          )
                                          "$match.14.lhs"
                                        <| Label
                                          ( TApplication
                                              KType
                                              (TConstructor (KArrow KType KType) "Tree")
                                              (TVariable (TypeIndex KType 3) :| [])
                                          )
                                          "$match.15.rhs"
                                        :| []
                                    )
                                    ( EApplication
                                        ()
                                        (TIntrinsic (IList (TVariable (TypeIndex KType 3))))
                                        ( EBinaryOperator
                                            ()
                                            ( TIntrinsic (IList (TVariable (TypeIndex KType 3)))
                                                `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 3)))
                                                `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 3)))
                                            )
                                            OListConcatenation
                                        )
                                        ( EApplication
                                            ()
                                            (TIntrinsic (IList (TVariable (TypeIndex KType 3))))
                                            ( EVariable
                                                ()
                                                ( Label
                                                    ( ( TApplication
                                                          KType
                                                          (TConstructor (KArrow KType KType) "Tree")
                                                          (TVariable (TypeIndex KType 3) :| [])
                                                      )
                                                        `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 3)))
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
                                                        (TVariable (TypeIndex KType 3) :| [])
                                                    )
                                                    "$match.14.lhs"
                                                )
                                                :| []
                                            )
                                            <| EListCons
                                              ()
                                              (TIntrinsic (IList (TVariable (TypeIndex KType 3))))
                                              (EVariable () (Label (TVariable (TypeIndex KType 3)) "$match.13.y"))
                                              ( EApplication
                                                  ()
                                                  (TIntrinsic (IList (TVariable (TypeIndex KType 3))))
                                                  ( EVariable
                                                      ()
                                                      ( Label
                                                          ( ( TApplication
                                                                KType
                                                                (TConstructor (KArrow KType KType) "Tree")
                                                                (TVariable (TypeIndex KType 3) :| [])
                                                            )
                                                              `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 3)))
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
                                                              (TVariable (TypeIndex KType 3) :| [])
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
                      ( EApplication
                          ()
                          (TIntrinsic (IList (TVariable (TypeIndex KType 4))))
                          ( EVariable
                              ()
                              ( Label
                                  ( ( TApplication
                                        KType
                                        (TConstructor (KArrow KType KType) "Tree")
                                        (TVariable (TypeIndex KType 4) :| [])
                                    )
                                      `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 4)))
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
                                      (TVariable (TypeIndex KType 4) :| [])
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
  )

funFlatten2 =
  ( Constant
      ()
      ( With
          []
          ( ( TApplication
                KType
                (TConstructor (KArrow KType KType) "Tree")
                (TVariable (TypeIndex KType 4) :| [])
            )
              `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 4)))
          )
      )
      ( ELambda
          ()
          ( PAnnotation
              ()
              (TApplication () (TConstructor () "Tree") (TVariable (Parameter () "a") :| []))
              ( PVariable
                  ()
                  ( Label
                      ( TApplication
                          KType
                          (TConstructor (KArrow KType KType) "Tree")
                          (TVariable (TypeIndex KType 4) :| [])
                      )
                      "tree"
                  )
              )
              :| []
          )
          ( EFold
              ()
              (TIntrinsic (IList (TVariable (TypeIndex KType 4))))
              ( EVariable
                  ()
                  ( Label
                      ( TApplication
                          KType
                          (TConstructor (KArrow KType KType) "Tree")
                          (TVariable (TypeIndex KType 4) :| [])
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
                              (TVariable (TypeIndex KType 4) :| [])
                          )
                          "Node"
                      )
                      [ PVariable () (Label (TVariable (TypeIndex KType 4)) "y")
                      , PAtVariable
                          ()
                          ( Label
                              ( TApplication
                                  KType
                                  (TConstructor (KArrow KType KType) "Tree")
                                  (TVariable (TypeIndex KType 4) :| [])
                              )
                              "lhs"
                          )
                      , PAtVariable
                          ()
                          ( Label
                              ( TApplication
                                  KType
                                  (TConstructor (KArrow KType KType) "Tree")
                                  (TVariable (TypeIndex KType 4) :| [])
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
                          (TIntrinsic (IList (TVariable (TypeIndex KType 4))))
                          ( EBinaryOperator
                              ()
                              ( (TIntrinsic (IList (TVariable (TypeIndex KType 4))))
                                  `TArrow` (TIntrinsic (IList (TVariable (TypeIndex KType 4))))
                                  `TArrow` (TIntrinsic (IList (TVariable (TypeIndex KType 4))))
                              )
                              OListConcatenation
                          )
                          ( EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 4)))) "lhs")
                              <| EListCons
                                ()
                                (TIntrinsic (IList (TVariable (TypeIndex KType 4))))
                                (EVariable () (Label (TVariable (TypeIndex KType 4)) "y"))
                                (EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 4)))) "rhs"))
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
                                (TVariable (TypeIndex KType 4) :| [])
                            )
                            "Leaf"
                        )
                        []
                    )
                    ( CPlain
                        ()
                        []
                        (EListLiteral () (TIntrinsic (IList (TVariable (TypeIndex KType 4)))) [])
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
                                  ( ( TApplication
                                        KType
                                        (TConstructor (KArrow KType KType) "Tree")
                                        (TVariable (TypeIndex KType 3) :| [])
                                    )
                                      `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 3)))
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
                                          (TVariable (TypeIndex KType 3) :| [])
                                      )
                                      "$fold.2.expr"
                                  )
                                  :| []
                              )
                              ( ECompiledMatch
                                  ()
                                  (TIntrinsic (IList (TVariable (TypeIndex KType 3))))
                                  ( EVariable
                                      ()
                                      ( Label
                                          ( TApplication
                                              KType
                                              (TConstructor (KArrow KType KType) "Tree")
                                              (TVariable (TypeIndex KType 3) :| [])
                                          )
                                          "$fold.2.expr"
                                      )
                                  )
                                  ( ECompiledClause
                                      ( Label
                                          ( TApplication
                                              KType
                                              (TConstructor (KArrow KType KType) "Tree")
                                              (TVariable (TypeIndex KType 3) :| [])
                                          )
                                          "Leaf"
                                          :| []
                                      )
                                      (EListLiteral () (TIntrinsic (IList (TVariable (TypeIndex KType 3)))) [])
                                      <| ECompiledClause
                                        ( Label
                                            ( TVariable (TypeIndex KType 3)
                                                `TArrow` TApplication
                                                  KType
                                                  (TConstructor (KArrow KType KType) "Tree")
                                                  (TVariable (TypeIndex KType 3) :| [])
                                                `TArrow` TApplication
                                                  KType
                                                  (TConstructor (KArrow KType KType) "Tree")
                                                  (TVariable (TypeIndex KType 3) :| [])
                                                `TArrow` TApplication
                                                  KType
                                                  (TConstructor (KArrow KType KType) "Tree")
                                                  (TVariable (TypeIndex KType 3) :| [])
                                            )
                                            "Node"
                                            <| Label (TVariable (TypeIndex KType 3)) "$match.13.y"
                                            <| Label
                                              ( TApplication
                                                  KType
                                                  (TConstructor (KArrow KType KType) "Tree")
                                                  (TVariable (TypeIndex KType 3) :| [])
                                              )
                                              "$match.14.lhs"
                                            <| Label
                                              ( TApplication
                                                  KType
                                                  (TConstructor (KArrow KType KType) "Tree")
                                                  (TVariable (TypeIndex KType 3) :| [])
                                              )
                                              "$match.15.rhs"
                                            :| []
                                        )
                                        ( EApplication
                                            ()
                                            (TIntrinsic (IList (TVariable (TypeIndex KType 3))))
                                            ( EBinaryOperator
                                                ()
                                                ( TIntrinsic (IList (TVariable (TypeIndex KType 3)))
                                                    `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 3)))
                                                    `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 3)))
                                                )
                                                OListConcatenation
                                            )
                                            ( EApplication
                                                ()
                                                (TIntrinsic (IList (TVariable (TypeIndex KType 3))))
                                                ( EVariable
                                                    ()
                                                    ( Label
                                                        ( ( TApplication
                                                              KType
                                                              (TConstructor (KArrow KType KType) "Tree")
                                                              (TVariable (TypeIndex KType 3) :| [])
                                                          )
                                                            `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 3)))
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
                                                            (TVariable (TypeIndex KType 3) :| [])
                                                        )
                                                        "$match.14.lhs"
                                                    )
                                                    :| []
                                                )
                                                <| EListCons
                                                  ()
                                                  (TIntrinsic (IList (TVariable (TypeIndex KType 3))))
                                                  (EVariable () (Label (TVariable (TypeIndex KType 3)) "$match.13.y"))
                                                  ( EApplication
                                                      ()
                                                      (TIntrinsic (IList (TVariable (TypeIndex KType 3))))
                                                      ( EVariable
                                                          ()
                                                          ( Label
                                                              ( ( TApplication
                                                                    KType
                                                                    (TConstructor (KArrow KType KType) "Tree")
                                                                    (TVariable (TypeIndex KType 3) :| [])
                                                                )
                                                                  `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 3)))
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
                                                                  (TVariable (TypeIndex KType 3) :| [])
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
                          (TIntrinsic (IList (TVariable (TypeIndex KType 4))))
                          ( EVariable
                              ()
                              ( Label
                                  ( ( TApplication
                                        KType
                                        (TConstructor (KArrow KType KType) "Tree")
                                        (TVariable (TypeIndex KType 4) :| [])
                                    )
                                      `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 4)))
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
                                      (TVariable (TypeIndex KType 4) :| [])
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
  )

funFromListA =
  ( Constant
      ()
      (With [] (TIntrinsic (IList (TVariable (TypeIndex KType 2))) `TArrow` TApplication KType (TConstructor (KArrow KType KType) "Tree") (TVariable (TypeIndex KType 2) :| [])))
      ( ELambda
          ()
          ( PAnnotation
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
                              (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 2)) "from_int32"))
                              (ELiteral () (LInt32 0) :| [])
                          )
                        ,
                          ( "max"
                          , EApplication
                              ()
                              (TVariable (TypeIndex KType 2))
                              (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 2)) "from_int32"))
                              (ELiteral () (LInt32 (-1)) :| [])
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
                                                    `TArrow` TVariable (TypeIndex KType 2)
                                                    `TArrow` TIntrinsic IBool
                                                )
                                                "in_range"
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
                                                                    `TArrow` (TVariable (TypeIndex KType 1))
                                                                    `TArrow` (TIntrinsic IBool)
                                                                )
                                                                "in_range"
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
                                          (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 2)) "from_int32"))
                                          (ELiteral () (LInt32 (-1)) :| [])
                                      )
                                    ,
                                      ( "min"
                                      , EApplication
                                          ()
                                          (TVariable (TypeIndex KType 2))
                                          (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 2)) "from_int32"))
                                          (ELiteral () (LInt32 0) :| [])
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
  )

funFromListA2 =
  ( Constant
      ()
      ( With
          [ Trait "Numeric" (TVariable (TypeIndex KType 2))
          , Trait "Ordered" (TVariable (TypeIndex KType 2))
          ]
          (TIntrinsic (IList (TVariable (TypeIndex KType 2))) `TArrow` TApplication KType (TConstructor (KArrow KType KType) "Tree") (TVariable (TypeIndex KType 2) :| []))
      )
      ( EDictionaryLambda
          ()
          ( Trait "Numeric" (TVariable (TypeIndex KType 2))
              <| Trait "Ordered" (TVariable (TypeIndex KType 2))
              :| []
          )
          ( ELambda
              ()
              ( PAnnotation
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
                              , EDictionaryApplication
                                  ()
                                  (TVariable (TypeIndex KType 2))
                                  (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 2)) "from_int32"))
                                  (Trait "Numeric" (TVariable (TypeIndex KType 0)) :| [])
                                  [ELiteral () (LInt32 0)]
                              )
                            ,
                              ( "max"
                              , EDictionaryApplication
                                  ()
                                  (TVariable (TypeIndex KType 2))
                                  (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 2)) "from_int32"))
                                  (Trait "Numeric" (TVariable (TypeIndex KType 0)) :| [])
                                  [ELiteral () (LInt32 (-1))]
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
                                          <| EDictionaryApplication
                                            ()
                                            (TVariable (TypeIndex KType 2) `TArrow` TIntrinsic IBool)
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
                                                        `TArrow` TVariable (TypeIndex KType 2)
                                                        `TArrow` TIntrinsic IBool
                                                    )
                                                    "in_range"
                                                )
                                            )
                                            ( Trait "Numeric" (TVariable (TypeIndex KType 2))
                                                <| Trait "Ordered" (TVariable (TypeIndex KType 2))
                                                :| []
                                            )
                                            [ EVariable
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
                                            ]
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
                                                          <| EDictionaryApplication
                                                            ()
                                                            (TVariable (TypeIndex KType 1) `TArrow` TIntrinsic IBool)
                                                            ( EVariable
                                                                ()
                                                                ( Label
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
                                                                        `TArrow` (TVariable (TypeIndex KType 1))
                                                                        `TArrow` (TIntrinsic IBool)
                                                                    )
                                                                    "in_range"
                                                                )
                                                            )
                                                            ( Trait "Numeric" (TVariable (TypeIndex KType 2))
                                                                <| Trait "Ordered" (TVariable (TypeIndex KType 2))
                                                                :| []
                                                            )
                                                            [ EVariable
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
                                                            ]
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
                                          , EDictionaryApplication
                                              ()
                                              (TVariable (TypeIndex KType 2))
                                              (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 2)) "from_int32"))
                                              (Trait "Numeric" (TVariable (TypeIndex KType 0)) :| [])
                                              [ELiteral () (LInt32 (-1))]
                                          )
                                        ,
                                          ( "min"
                                          , EDictionaryApplication
                                              ()
                                              (TVariable (TypeIndex KType 2))
                                              (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 2)) "from_int32"))
                                              (Trait "Numeric" (TVariable (TypeIndex KType 0)) :| [])
                                              [ELiteral () (LInt32 0)]
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
      )
  )

fromMain =
  ( Constant
      ()
      (With [] (TIntrinsic IUnit `TArrow` TVariable (TypeIndex KType 0)))
      ( ELambda
          ()
          (PLiteral () LUnit :| [])
          ( ELet
              ()
              ( BPattern
                  ()
                  (PVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 1)))) "xs"))
                  ( EAnnotation
                      ()
                      (TIntrinsic (IList (TVariable (Parameter () "a"))))
                      ( EListLiteral
                          ()
                          (TIntrinsic (IList (TVariable (TypeIndex KType 1))))
                          [ EApplication () (TVariable (TypeIndex KType 1)) (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 1)) "from_int32")) (ELiteral () (LInt32 5) :| [])
                          , EApplication () (TVariable (TypeIndex KType 1)) (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 1)) "from_int32")) (ELiteral () (LInt32 3) :| [])
                          , EApplication () (TVariable (TypeIndex KType 1)) (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 1)) "from_int32")) (ELiteral () (LInt32 7) :| [])
                          , EApplication () (TVariable (TypeIndex KType 1)) (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 1)) "from_int32")) (ELiteral () (LInt32 2) :| [])
                          , EApplication () (TVariable (TypeIndex KType 1)) (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 1)) "from_int32")) (ELiteral () (LInt32 1) :| [])
                          , EApplication () (TVariable (TypeIndex KType 1)) (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 1)) "from_int32")) (ELiteral () (LInt32 6) :| [])
                          , EApplication () (TVariable (TypeIndex KType 1)) (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 1)) "from_int32")) (ELiteral () (LInt32 4) :| [])
                          ]
                      )
                  )
                  :| []
              )
              ( EApplication
                  ()
                  (TVariable (TypeIndex KType 0))
                  (EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 2))) `TArrow` TVariable (TypeIndex KType 0)) "trace"))
                  ( EApplication
                      ()
                      (TIntrinsic (IList (TVariable (TypeIndex KType 2))))
                      (EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 2))) `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 2)))) "sort"))
                      ( EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 2)))) "xs")
                          :| []
                      )
                      :| []
                  )
              )
          )
      )
  )

fromMain2 =
  ( Constant
      ()
      ( With
          [Trait "Numeric" (TVariable (TypeIndex KType 2))]
          (TIntrinsic IUnit `TArrow` TVariable (TypeIndex KType 0))
      )
      ( EDictionaryLambda
          ()
          (Trait "Numeric" (TVariable (TypeIndex KType 2)) :| [])
          ( ELambda
              ()
              (PLiteral () LUnit :| [])
              ( ELet
                  ()
                  ( BPattern
                      ()
                      (PVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 1)))) "xs"))
                      ( EDictionaryLambda
                          ()
                          (Trait "Numeric" (TVariable (TypeIndex KType 1)) :| [])
                          ( EAnnotation
                              ()
                              (TIntrinsic (IList (TVariable (Parameter () "a"))))
                              ( EListLiteral
                                  ()
                                  (TIntrinsic (IList (TVariable (TypeIndex KType 1))))
                                  [ -- EApplication () (TVariable (TypeIndex KType 1)) (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 1)) "from_int32")) (ELiteral () (LInt32 5) :| [])
                                    EDictionaryApplication
                                      ()
                                      (TVariable (TypeIndex KType 1))
                                      (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 1)) "from_int32"))
                                      (Trait "Numeric" (TVariable (TypeIndex KType 1)) :| [])
                                      [ELiteral () (LInt32 5)]
                                  , EDictionaryApplication
                                      ()
                                      (TVariable (TypeIndex KType 1))
                                      (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 1)) "from_int32"))
                                      (Trait "Numeric" (TVariable (TypeIndex KType 1)) :| [])
                                      [ELiteral () (LInt32 3)]
                                  , EDictionaryApplication
                                      ()
                                      (TVariable (TypeIndex KType 1))
                                      (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 1)) "from_int32"))
                                      (Trait "Numeric" (TVariable (TypeIndex KType 1)) :| [])
                                      [ELiteral () (LInt32 7)]
                                  , EDictionaryApplication
                                      ()
                                      (TVariable (TypeIndex KType 1))
                                      (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 1)) "from_int32"))
                                      (Trait "Numeric" (TVariable (TypeIndex KType 1)) :| [])
                                      [ELiteral () (LInt32 2)]
                                  , EDictionaryApplication
                                      ()
                                      (TVariable (TypeIndex KType 1))
                                      (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 1)) "from_int32"))
                                      (Trait "Numeric" (TVariable (TypeIndex KType 1)) :| [])
                                      [ELiteral () (LInt32 1)]
                                  , EDictionaryApplication
                                      ()
                                      (TVariable (TypeIndex KType 1))
                                      (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 1)) "from_int32"))
                                      (Trait "Numeric" (TVariable (TypeIndex KType 1)) :| [])
                                      [ELiteral () (LInt32 6)]
                                  , EDictionaryApplication
                                      ()
                                      (TVariable (TypeIndex KType 1))
                                      (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 1)) "from_int32"))
                                      (Trait "Numeric" (TVariable (TypeIndex KType 1)) :| [])
                                      [ELiteral () (LInt32 4)]
                                  ]
                              )
                          )
                      )
                      :| []
                  )
                  ( EApplication
                      ()
                      (TVariable (TypeIndex KType 0))
                      (EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 2))) `TArrow` TVariable (TypeIndex KType 0)) "trace"))
                      ( EApplication
                          ()
                          (TIntrinsic (IList (TVariable (TypeIndex KType 2))))
                          (EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 2))) `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 2)))) "sort"))
                          ( EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 2)))) "xs")
                              :| []
                          )
                          :| []
                      )
                  )
              )
          )
      )
  )
