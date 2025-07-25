{-# LANGUAGE OverloadedStrings #-}

module Coal.Set2.Test05 where

import Coal.Common.List1 (NonEmpty (..), (<|))
import Coal.Common.Label (Label (..))
import Coal.Language
import Coal.Language.Type.Intrinsic
import Coal.Language.Module

import qualified Coal.Language.Module as Module

-- Normalize top-level definitions
prog2_05 :: [Module () Kind IndexedType]
prog2_05 =
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
        [ DConstant
            "show"
            ( Constant
                ()
                (With [] (TIntrinsic IString `TArrow` TIntrinsic IString))
                ( ELambda
                    ()
                    (PVariable () (Label (TIntrinsic IString) "s") :| [])
                    (EVariable () (Label (TIntrinsic IString) "s"))
                )
            )
        ]
    , -- instance Show(int32)
      DInstance
        "Show"
        (TIntrinsic IInt32)
        [ DConstant
            "show"
            ( Constant
                ()
                (With [] (TIntrinsic IInt32 `TArrow` TIntrinsic IString))
                ( ELambda
                    ()
                    (PVariable () (Label (TIntrinsic IInt32) "n") :| [])
                    (ELiteral () (LString "TODO"))
                )
            )
        ]
    , -- instance Show((a, b)) with Show(a), Show(b)
      DInstance
        "Show"
        (TIntrinsic (ITuple [TVariable (Parameter () "a"), TVariable (Parameter () "b")]))
        [ DConstant
            "show"
            ( Constant
                ()
                (With [] (pairType `TArrow` TIntrinsic IString))
                ( ELambda
                    ()
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
            )
        ]
    , -- instance Show(list(a)) with Show(a)
      DInstance
        "Show"
        (TIntrinsic (IList (TVariable (Parameter () "a"))))
        [ DConstant
            "show"
            ( Constant
                ()
                (With [] (TIntrinsic (IList (TVariable (TypeIndex KType 0))) `TArrow` TIntrinsic IString))
                ( ELambda
                    ()
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
    , DConstant
        "baz"
        ( Constant
            ()
            (With [] (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 1) `TArrow` TIntrinsic IString))
            ( ELambda
                ()
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
        )
    , DConstant
        "bar"
        ( Constant
            ()
            (With [] (TIntrinsic (IList pairType) `TArrow` TIntrinsic IString))
            ( ELambda
                ()
                (PVariable () (Label (TIntrinsic (IList pairType)) "xs") :| [])
                ( EApplication
                    ()
                    (TIntrinsic IString)
                    (EVariable () (Label (TIntrinsic (IList pairType) `TArrow` TIntrinsic IString) "show"))
                    (EVariable () (Label (TIntrinsic (IList pairType)) "xs") :| [])
                )
            )
        )
    ]
