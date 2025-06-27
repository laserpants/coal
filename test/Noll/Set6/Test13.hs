{-# LANGUAGE OverloadedStrings #-}

module Noll.Set6.Test13 where

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

prog1_13 :: [Module () Kind IndexedType]
prog1_13 =
  [ moduleMain
  ]

moduleMain :: Module () Kind IndexedType
moduleMain =
  Module.fromDefinitionList
    (Path ["Main"])
    -- Exports
    []
    -- Definitions
    [ DImport (Path ["Core$"]) ["trace_string"]
    , DImport (Path ["Core$"]) ["operator__string_concatenation"]
    , DFunction
        "main"
        ( Function
            ()
            (With [] (TVariable (TypeIndex KType 0)))
            (PLiteral () LUnit :| [])
            ( ELet
                ()
                ( BPattern
                    ()
                    ( PVariable
                        ()
                        ( Label
                            ( TConstructor KType "$Nat"
                                `TArrow` TIntrinsic IString
                                `TArrow` TIntrinsic IString
                            )
                            "f"
                        )
                    )
                    ( ELambda
                        ()
                        (PVariable () (Label (TConstructor KType "$Nat") "n") :| [])
                        ( EFold
                            ()
                            (TIntrinsic IString `TArrow` TIntrinsic IString)
                            (EVariable () (Label (TConstructor KType "$Nat") "n") :| [])
                            ( EClause
                                ()
                                ( PConstructor
                                    ()
                                    (Label (TConstructor KType "$Nat") "Zero")
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
                                      (Label (TConstructor KType "$Nat") "Succ")
                                      [ PAtVariable () (Label (TConstructor KType "$Nat") "f")
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
                                    (PVariable () (Label (TConstructor KType "$Nat" `TArrow` TIntrinsic IString `TArrow` TIntrinsic IString) "$fold.1"))
                                    ( ELambda
                                        ()
                                        (PVariable () (Label (TConstructor KType "$Nat") "$fold.1.expr") :| [])
                                        ( ECompiledMatch
                                            ()
                                            (TIntrinsic IString `TArrow` TIntrinsic IString)
                                            (EVariable () (Label (TConstructor KType "$Nat") "$fold.1.expr"))
                                            ( ECompiledClause
                                                (Label (TConstructor KType "$Nat" `TArrow` TConstructor KType "$Nat") "$Succ" <| Label (TIntrinsic IInt32) "$succ.1" :| [])
                                                ( ERecursiveLet
                                                    ()
                                                    (PVariable () (Label (TConstructor KType "$Nat") "$match.1.f"))
                                                    ( EIf
                                                        ()
                                                        (TIntrinsic IInt32)
                                                        ( EApplication
                                                            ()
                                                            (TIntrinsic IBool)
                                                            (EBinaryOperator () (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IBool) OEqualTo)
                                                            ( EVariable () (Label (TIntrinsic IInt32) "$succ.1")
                                                                <| ELiteral () (LInt32 0)
                                                                :| []
                                                            )
                                                        )
                                                        (EConstructor () (Label (TConstructor KType "$Nat") "$Zero"))
                                                        ( EApplication
                                                            ()
                                                            (TConstructor KType "$Nat")
                                                            (EConstructor () (Label (TIntrinsic IInt32 `TArrow` TConstructor KType "$Nat") "$Succ"))
                                                            ( EApplication
                                                                ()
                                                                (TIntrinsic IInt32)
                                                                (EBinaryOperator () (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) OSubtraction)
                                                                ( EVariable () (Label (TIntrinsic IInt32) "$succ.1")
                                                                    <| ELiteral () (LInt32 1)
                                                                    :| []
                                                                )
                                                                :| []
                                                            )
                                                        )
                                                    )
                                                    ( ELambda
                                                        ()
                                                        (PVariable () (Label (TIntrinsic IString) "s") :| [])
                                                        ( EApplication
                                                            ()
                                                            (TIntrinsic IString)
                                                            ( EVariable
                                                                ()
                                                                ( Label
                                                                    ( TConstructor KType "$Nat" `TArrow` TIntrinsic IString `TArrow` TIntrinsic IString
                                                                    )
                                                                    "$fold.1"
                                                                )
                                                            )
                                                            ( EVariable () (Label (TConstructor KType "$Nat") "$match.1.f")
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
                                                )
                                                <| ECompiledClause
                                                  (Label (TConstructor KType "$Nat") "$Zero" :| [])
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
                                        (EVariable () (Label (TConstructor KType "$Nat" `TArrow` TIntrinsic IString `TArrow` TIntrinsic IString) "$fold.1"))
                                        (EVariable () (Label (TConstructor KType "$Nat") "n") :| [])
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
                        (EVariable () (Label (TConstructor KType "$Nat" `TArrow` TIntrinsic IString `TArrow` TIntrinsic IString) "f"))
                        ( EApplication
                            ()
                            (TConstructor KType "$Nat")
                            (EConstructor () (Label (TIntrinsic IInt32 `TArrow` TConstructor KType "$Nat") "$Succ"))
                            ( EApplication
                                ()
                                (TIntrinsic IInt32)
                                (EVariable () (Label (TConstructor KType "$Nat" `TArrow` TIntrinsic IInt32) "Core$.unpack_nat"))
                                ( EApplication
                                    ()
                                    (TConstructor KType "$Nat")
                                    (EConstructor () (Label (TIntrinsic IInt32 `TArrow` TConstructor KType "$Nat") "$Succ"))
                                    ( EApplication
                                        ()
                                        (TIntrinsic IInt32)
                                        (EVariable () (Label (TConstructor KType "$Nat" `TArrow` TIntrinsic IInt32) "Core$.unpack_nat"))
                                        ( EApplication
                                            ()
                                            (TConstructor KType "$Nat")
                                            (EConstructor () (Label (TIntrinsic IInt32 `TArrow` TConstructor KType "$Nat") "$Succ"))
                                            ( EApplication
                                                ()
                                                (TIntrinsic IInt32)
                                                (EVariable () (Label (TConstructor KType "$Nat" `TArrow` TIntrinsic IInt32) "Core$.unpack_nat"))
                                                ( EApplication
                                                    ()
                                                    (TConstructor KType "$Nat")
                                                    (EConstructor () (Label (TIntrinsic IInt32 `TArrow` TConstructor KType "$Nat") "$Succ"))
                                                    ( EApplication
                                                        ()
                                                        (TIntrinsic IInt32)
                                                        (EVariable () (Label (TConstructor KType "$Nat" `TArrow` TIntrinsic IInt32) "Core$.unpack_nat"))
                                                        ( EApplication
                                                            ()
                                                            (TConstructor KType "$Nat")
                                                            (EConstructor () (Label (TIntrinsic IInt32 `TArrow` TConstructor KType "$Nat") "$Succ"))
                                                            ( EApplication
                                                                ()
                                                                (TIntrinsic IInt32)
                                                                (EVariable () (Label (TConstructor KType "$Nat" `TArrow` TIntrinsic IInt32) "Core$.unpack_nat"))
                                                                (EConstructor () (Label (TConstructor KType "$Nat") "$Zero") :| [])
                                                                :| []
                                                            )
                                                            :| []
                                                        )
                                                        :| []
                                                    )
                                                    :| []
                                                )
                                                :| []
                                            )
                                            :| []
                                        )
                                        :| []
                                    )
                                    :| []
                                )
                                :| []
                            )
                            :| []
                        )
                        :| []
                    )
                )
            )
        )
    ]
