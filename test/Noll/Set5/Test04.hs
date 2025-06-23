{-# LANGUAGE OverloadedStrings #-}

module Noll.Set5.Test04 where

import Noll.SystemF.Constraint
import Noll.SystemF.Substitution (Substitutable (..), Substitution (..), mapsTo)
import Noll.SystemF.Constraint.Solver
import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Noll.Language
import Noll.Module (Constant (..), Definition (..), Function (..), Module (..), Path (..))

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Noll.Module as Module

-- Add type info
prog1_04 :: [Module () Kind IndexedType]
prog1_04 =
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
--     f(5, "")
--
moduleMain :: Module () Kind IndexedType
moduleMain =
  Module.fromDefinitionList
    (Path ["Main"])
    -- Exports
    []
    -- Definitions
    [ DFunction
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
                                        ( EVariable () (Label (TIntrinsic IString) "s")
                                        )
                                    )
                                    :| []
                                )
                                <| EClause
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
                                                        ( EVariable () (Label (TIntrinsic IString) "s")
                                                        )
                                                    )
                                                    :| []
                                                )
                                                <| EClause
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
                                                          ( EApplication
                                                              ()
                                                              (TIntrinsic IString)
                                                              ( EVariable
                                                                  ()
                                                                  ( Label
                                                                      ( TIntrinsic IString `TArrow` TIntrinsic IString
                                                                      )
                                                                      "f"
                                                                  )
                                                              )
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
                    (TIntrinsic IString)
                    ( EVariable
                        ()
                        ( Label
                            ( TIntrinsic IInt32
                                `TArrow` TIntrinsic IString
                                `TArrow` TIntrinsic IString
                            )
                            "f"
                        )
                    )
                    ( ELiteral () (LInt32 5)
                        <| ELiteral () (LString "")
                        :| []
                    )
                )
            )
        )
    ]

banan :: (Substitution, Int, [()])
banan =
  solveConstraints 200
    []

banan22 :: (Substitution, Int, [()])
banan22 =
  solveConstraints 200
    [ Equality () [TVariable (TypeIndex KType 3), TArrow (TIntrinsic IInt32) (TVariable (TypeIndex KType 2))]
    , Equality () [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)]
    , Implicit () (TVariable (TypeIndex KType 3)) (TVariable (TypeIndex KType 0)) (Monomorphic (Set.fromList [TypeIndex KType 1]))
    ]
