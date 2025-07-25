{-# LANGUAGE OverloadedStrings #-}

module Noll.Set6.Test12 where

import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Common.Label (Label (..))
import Noll.Language
import Noll.Language.Module (Constant (..), Definition (..), Function (..), Module (..), Path (..))
import Noll.TypeSystem.Constraint
import Noll.TypeSystem.Constraint.Solver
import Noll.TypeSystem.Substitution (Substitutable (..), Substitution (..), mapsTo)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Noll.Language.Module as Module

prog1_12 :: [Module () Kind IndexedType]
prog1_12 =
  [ moduleMain
  ]

--
-- let
--   f =
--     fn(n) =>
--       fold(n) {
--         | Zero =>
--             fn(s) => s
--         | Succ(@f) =>
--             fn(s) => f("a" +++ s)
--       }
--   in
--     trace_string(f(Succ(Succ(Succ(Succ(Succ(Zero))))), ""))
--
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
                            (EConstructor () (Label (TIntrinsic INat `TArrow` TIntrinsic INat) "Succ"))
                            ( EApplication
                                ()
                                (TIntrinsic INat)
                                (EConstructor () (Label (TIntrinsic INat `TArrow` TIntrinsic INat) "Succ"))
                                ( EApplication
                                    ()
                                    (TIntrinsic INat)
                                    (EConstructor () (Label (TIntrinsic INat `TArrow` TIntrinsic INat) "Succ"))
                                    ( EApplication
                                        ()
                                        (TIntrinsic INat)
                                        (EConstructor () (Label (TIntrinsic INat `TArrow` TIntrinsic INat) "Succ"))
                                        ( EApplication
                                            ()
                                            (TIntrinsic INat)
                                            (EConstructor () (Label (TIntrinsic INat `TArrow` TIntrinsic INat) "Succ"))
                                            ( EConstructor () (Label (TIntrinsic INat) "Zero")
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
