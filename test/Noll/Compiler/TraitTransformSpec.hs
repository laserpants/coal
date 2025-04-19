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

result = runTraitTransformZ testEnvZ2 (transformConstantZ fixtureX1) (freshIdIn fixtureX1)

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
      --      ( "compare"
      --      , Forall
      --          (Set.fromList [TypeIndex KType 0] :: Set (TypeIndex Kind))
      --          [Trait "Ordered" (TVariable (TypeIndex KType 0))]
      --          (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TConstructor KType "Ordering")
      --      )
    ]

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
