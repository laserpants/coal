{-# LANGUAGE OverloadedStrings #-}

module Noll.Set5.Test10 where

import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Noll.Language
import Noll.Module (Constant (..), Definition (..), Function (..), Module (..), Path (..))
import Noll.SystemF.Constraint
import Noll.SystemF.Constraint.Solver
import Noll.SystemF.Substitution (Substitutable (..), Substitution (..), mapsTo)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Noll.Module as Module

prog1_10 :: [Module () Kind IndexedType]
prog1_10 =
  [ moduleMain
  ]

moduleMain :: Module () Kind IndexedType
moduleMain =
  Module.fromDefinitionList
    (Path ["Main"])
    -- Exports
    []
    -- Definitions
    [ DConstant
        "main"
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
                        ( PVariable
                            ()
                            ( Label
                                ( TIntrinsic INat
                                    `TArrow` TIntrinsic IString
                                    `TArrow` TIntrinsic IString
                                )
                                "f"
                            )
                        )
                        ( ELambda
                            ()
                            (PVariable () (Label (TIntrinsic INat) "n") :| [])
                            ( EFold
                                ()
                                (TIntrinsic IString `TArrow` TIntrinsic IString)
                                (EVariable () (Label (TIntrinsic INat) "n") :| [])
                                ( EClause
                                    ()
                                    ( PConstructor
                                        ()
                                        (Label (TIntrinsic INat) "Zero")
                                        []
                                    )
                                    ( CPlain
                                        ()
                                        []
                                        ( ELambda
                                            ()
                                            (PVariable () (Label (TIntrinsic IString) "s") :| [])
                                            ( EVariable () (Label (TIntrinsic IString) "s")
                                            )
                                        )
                                        :| []
                                    )
                                    <| EClause
                                      ()
                                      ( PConstructor
                                          ()
                                          (Label (TIntrinsic INat) "Succ")
                                          [ PAtVariable () (Label (TIntrinsic INat) "f")
                                          ]
                                      )
                                      ( CPlain
                                          ()
                                          []
                                          ( ELambda
                                              ()
                                              (PVariable () (Label (TIntrinsic IString) "s") :| [])
                                              ( EApplication
                                                  ()
                                                  (TIntrinsic IString)
                                                  (EVariable () (Label (TIntrinsic IString `TArrow` TIntrinsic IString) "f"))
                                                  ( EApplication
                                                      ()
                                                      (TIntrinsic IString)
                                                      ( EBinaryOperator
                                                          ()
                                                          (TIntrinsic IString `TArrow` TIntrinsic IString `TArrow` TIntrinsic IString)
                                                          OStringConcatenation
                                                      )
                                                      ( ELiteral () (LString "a")
                                                          <| EVariable () (Label (TIntrinsic IString) "s")
                                                          :| []
                                                      )
                                                      :| []
                                                  )
                                              )
                                          )
                                          :| []
                                      )
                                    :| []
                                )
                                ( Just
                                    ( ERecursiveLet
                                        ()
                                        (PVariable () (Label (TIntrinsic INat `TArrow` TIntrinsic IString `TArrow` TIntrinsic IString) "$fold.1"))
                                        ( ELambda
                                            ()
                                            (PVariable () (Label (TIntrinsic INat) "$fold.1.expr") :| [])
                                            ( ECompiledMatch
                                                ()
                                                (TIntrinsic IString `TArrow` TIntrinsic IString)
                                                (EVariable () (Label (TIntrinsic INat) "$fold.1.expr"))
                                                ( ECompiledClause
                                                    (Label (TIntrinsic INat `TArrow` TIntrinsic INat) "Succ" <| Label (TIntrinsic INat) "$match.1.f" :| [])
                                                    ( ELambda
                                                        ()
                                                        (PVariable () (Label (TIntrinsic IString) "s") :| [])
                                                        ( EApplication
                                                            ()
                                                            (TIntrinsic IString)
                                                            ( EVariable
                                                                ()
                                                                ( Label
                                                                    ( TIntrinsic INat `TArrow` TIntrinsic IString `TArrow` TIntrinsic IString
                                                                    )
                                                                    "$fold.1"
                                                                )
                                                            )
                                                            ( EVariable () (Label (TIntrinsic INat) "$match.1.f")
                                                                <| EApplication
                                                                  ()
                                                                  (TIntrinsic IString)
                                                                  ( EBinaryOperator
                                                                      ()
                                                                      (TIntrinsic IString `TArrow` TIntrinsic IString `TArrow` TIntrinsic IString)
                                                                      OStringConcatenation
                                                                  )
                                                                  ( ELiteral () (LString "a")
                                                                      <| EVariable () (Label (TIntrinsic IString) "s")
                                                                      :| []
                                                                  )
                                                                :| []
                                                            )
                                                        )
                                                    )
                                                    <| ECompiledClause
                                                      (Label (TIntrinsic INat) "Zero" :| [])
                                                      ( ELambda
                                                          ()
                                                          (PVariable () (Label (TIntrinsic IString) "s") :| [])
                                                          ( EVariable () (Label (TIntrinsic IString) "s")
                                                          )
                                                      )
                                                    :| []
                                                )
                                            )
                                        )
                                        ( EApplication
                                            ()
                                            (TIntrinsic IString `TArrow` TIntrinsic IString)
                                            (EVariable () (Label (TIntrinsic INat `TArrow` TIntrinsic IString `TArrow` TIntrinsic IString) "$fold.1"))
                                            (EVariable () (Label (TIntrinsic INat) "n") :| [])
                                        )
                                    )
                                )
                            )
                        )
                        :| []
                    )
                    ( EApplication
                        ()
                        (TVariable (TypeIndex KType 0))
                        (EVariable () (Label (TIntrinsic IString `TArrow` TVariable (TypeIndex KType 0)) "trace_string"))
                        ( EApplication
                            ()
                            (TIntrinsic IString)
                            (EVariable () (Label (TIntrinsic INat `TArrow` TIntrinsic IString `TArrow` TIntrinsic IString) "f"))
                            ( EApplication
                                ()
                                (TIntrinsic INat)
                                ( EApplication
                                    ()
                                    (TIntrinsic IInt32 `TArrow` TIntrinsic INat)
                                    (EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TIntrinsic INat :| []) `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic INat) "from_int32"))
                                    ( ERecord
                                        ()
                                        (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TIntrinsic INat :| []))
                                        ( Map.fromList
                                            [
                                              ( "from_int32"
                                              , EVariable () (Label{labelTag = TArrow (TIntrinsic IInt32) (TIntrinsic INat), labelName = "from_int32__$instance_Numeric(Intrinsic(Nat))"})
                                              )
                                            ]
                                        )
                                        Nothing
                                        :| []
                                    )
                                )
                                ( ELiteral () (LInt32 5)
                                    :| []
                                )
                                <| ELiteral () (LString "")
                                :| []
                            )
                            :| []
                        )
                    )
                )
            )
        )
    ]
