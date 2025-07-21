{-# LANGUAGE OverloadedStrings #-}

module Noll.Set2.Test04 where

import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Noll.Language
import Noll.Language.Type.Intrinsic
import Noll.Module

import qualified Noll.Module as Module

-- Add type info
prog2_04 :: [Module () Kind IndexedType]
prog2_04 =
  [ moduleFoo
  ]

pairType :: IndexedType
pairType = TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)])

moduleFoo :: Module () Kind IndexedType
moduleFoo =
  Module.fromDefinitionList
    (Path ["Foo"])
    -- Exports
    []
    -- Definitions
    [ DTrait
        "Show"
        []
        (Parameter KType "a")
        [
          ( "show"
          , TVariable (Parameter () "a") `TArrow` TIntrinsic IString
          )
        ]
    , -- instance Show(string)
      DInstance
        "Show"
        (TIntrinsic IString)
        [ DFunction
            "show"
            ( Function
                ()
                (With [] (TIntrinsic IString))
                (PVariable () (Label (TIntrinsic IString) "s") :| [])
                (EVariable () (Label (TIntrinsic IString) "s"))
            )
        ]
    , -- instance Show(int32)
      DInstance
        "Show"
        (TIntrinsic IInt32)
        [ DFunction
            "show"
            ( Function
                ()
                (With [] (TIntrinsic IString))
                (PVariable () (Label (TIntrinsic IInt32) "n") :| [])
                (ELiteral () (LString "TODO"))
            )
        ]
    , -- instance Show((a, b)) with Show(a), Show(b)
      DInstance
        "Show"
        (TIntrinsic (ITuple [TVariable (Parameter () "a"), TVariable (Parameter () "b")]))
        [ DFunction
            "show"
            ( Function
                ()
                (With [] (TIntrinsic IString))
                (PVariable () (Label pairType "p") :| [])
                ( EMatch
                    ()
                    (TIntrinsic IString)
                    (EVariable () (Label pairType "p"))
                    ( EClause
                        ()
                        ( PTuple
                            ()
                            pairType
                            ( PVariable () (Label (TVariable (TypeIndex KType 0)) "x")
                                <| PVariable () (Label (TVariable (TypeIndex KType 1)) "y")
                                :| []
                            )
                        )
                        ( CPlain
                            ()
                            []
                            ( EApplication
                                ()
                                (TIntrinsic IString)
                                ( EBinaryOperator
                                    ()
                                    (TIntrinsic IString `TArrow` TIntrinsic IString `TArrow` TIntrinsic IString)
                                    OStringConcatenation
                                )
                                ( EApplication
                                    ()
                                    (TIntrinsic IString)
                                    (EVariable () (Label (TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IString) "show"))
                                    (EVariable () (Label (TVariable (TypeIndex KType 0)) "x") :| [])
                                    <| EApplication
                                      ()
                                      (TIntrinsic IString)
                                      (EVariable () (Label (TVariable (TypeIndex KType 1) `TArrow` TIntrinsic IString) "show"))
                                      (EVariable () (Label (TVariable (TypeIndex KType 1)) "y") :| [])
                                    :| []
                                )
                            )
                            :| []
                        )
                        :| []
                    )
                )
            )
        ]
    , -- instance Show(list(a)) with Show(a)
      DInstance
        "Show"
        (TIntrinsic (IList (TVariable (Parameter () "a"))))
        [ DFunction
            "show"
            ( Function
                ()
                (With [] (TIntrinsic IString))
                (PVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 0)))) "lst") :| [])
                ( EMatch
                    ()
                    (TIntrinsic IString)
                    (EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 0)))) "lst"))
                    ( EClause
                        ()
                        ( PListCons
                            ()
                            (TIntrinsic (IList (TVariable (TypeIndex KType 0))))
                            (PVariable () (Label (TVariable (TypeIndex KType 0)) "x"))
                            (PAny () (TIntrinsic (IList (TVariable (TypeIndex KType 0)))))
                        )
                        ( CPlain
                            ()
                            []
                            ( EApplication
                                ()
                                (TIntrinsic IString)
                                (EVariable () (Label (TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IString) "show"))
                                (EVariable () (Label (TVariable (TypeIndex KType 0)) "x") :| [])
                            )
                            :| []
                        )
                        :| []
                    )
                )
            )
        ]
    , DConstant
        "foo"
        ( Constant
            ()
            (With [] (TIntrinsic IString))
            ( ELet
                ()
                ( BPattern
                    ()
                    (PVariable () (Label (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString])) "p"))
                    ( ETuple
                        ()
                        (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]))
                        ( ELiteral () (LInt32 1)
                            <| ELiteral () (LString "hello")
                            :| []
                        )
                    )
                    :| []
                )
                ( EApplication
                    ()
                    (TIntrinsic IString)
                    (EVariable () (Label (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]) `TArrow` TIntrinsic IString) "show"))
                    (EVariable () (Label (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString])) "p") :| [])
                )
            )
        )
    , DFunction
        "baz"
        ( Function
            ()
            (With [] (TIntrinsic IString))
            ( PVariable () (Label (TVariable (TypeIndex KType 0)) "x")
                <| PVariable () (Label (TVariable (TypeIndex KType 1)) "y")
                :| []
            )
            ( ELet
                ()
                ( BPattern
                    ()
                    (PVariable () (Label pairType "p"))
                    ( ETuple
                        ()
                        pairType
                        ( EVariable () (Label (TVariable (TypeIndex KType 0)) "x")
                            <| EVariable () (Label (TVariable (TypeIndex KType 1)) "y")
                            :| []
                        )
                    )
                    :| []
                )
                ( EApplication
                    ()
                    (TIntrinsic IString)
                    (EVariable () (Label (pairType `TArrow` TIntrinsic IString) "show"))
                    (EVariable () (Label pairType "p") :| [])
                )
            )
        )
    , DFunction
        "bar"
        ( Function
            ()
            (With [] (TIntrinsic IString))
            (PVariable () (Label (TIntrinsic (IList pairType)) "xs") :| [])
            ( EApplication
                ()
                (TIntrinsic IString)
                (EVariable () (Label (TIntrinsic (IList pairType) `TArrow` TIntrinsic IString) "show"))
                (EVariable () (Label (TIntrinsic (IList pairType)) "xs") :| [])
            )
        )
    ]
