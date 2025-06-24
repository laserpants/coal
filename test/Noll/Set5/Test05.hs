{-# LANGUAGE OverloadedStrings #-}

module Noll.Set5.Test05 where

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

prog1_05 :: [Module () Kind IndexedType]
prog1_05 =
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
                                        ( EMatch
                                            ()
                                            (TIntrinsic IString `TArrow` TIntrinsic IString)
                                            (EVariable () (Label (TIntrinsic INat) "$fold.1.expr"))
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
                                                      [ PVariable
                                                          ()
                                                          ( Label
                                                              (TIntrinsic INat)
                                                              "f"
                                                          )
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
                                                              ( EVariable
                                                                  ()
                                                                  ( Label
                                                                      ( TIntrinsic INat `TArrow` TIntrinsic IString `TArrow` TIntrinsic IString
                                                                      )
                                                                      "$fold.1"
                                                                  )
                                                              )
                                                              ( EVariable () (Label (TIntrinsic INat) "f")
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
                                                      :| []
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
                            (EVariable () (Label (TIntrinsic IInt32 `TArrow` TIntrinsic INat) "from_int32"))
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
