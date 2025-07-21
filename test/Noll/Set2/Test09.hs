{-# LANGUAGE OverloadedStrings #-}

module Noll.Set2.Test09 where

import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Noll.Language
import Noll.Language.Type.Intrinsic
import Noll.Module

import qualified Noll.Module as Module

-- Compile match statements
prog2_09 :: [Module () Kind IndexedType]
prog2_09 =
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
                    ( ECompiledMatch
                        ()
                        (TIntrinsic IString)
                        (EVariable () (Label pairType "p"))
                        ( ECompiledClause
                            ( Label (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 1) `TArrow` pairType) "$Tuple2"
                                <| Label (TVariable (TypeIndex KType 0)) "$match.1.x"
                                <| Label (TVariable (TypeIndex KType 1)) "$match.2.y"
                                :| []
                            )
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
                                    (EVariable () (Label (TVariable (TypeIndex KType 0)) "$match.1.x") :| [])
                                    <| EApplication
                                      ()
                                      (TIntrinsic IString)
                                      (EVariable () (Label (TVariable (TypeIndex KType 1) `TArrow` TIntrinsic IString) "show"))
                                      (EVariable () (Label (TVariable (TypeIndex KType 1)) "$match.2.y") :| [])
                                    :| []
                                )
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
                    ( ECompiledMatch
                        ()
                        (TIntrinsic IString)
                        (EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 0)))) "lst"))
                        ( ECompiledClause
                            ( Label (TVariable (TypeIndex KType 0) `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 0))) `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 0)))) "$Cons"
                                <| Label (TVariable (TypeIndex KType 0)) "$match.4.x"
                                <| Label (TIntrinsic (IList (TVariable (TypeIndex KType 0)))) "$match.5._"
                                :| []
                            )
                            ( EApplication
                                ()
                                (TIntrinsic IString)
                                (EVariable () (Label (TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IString) "show"))
                                (EVariable () (Label (TVariable (TypeIndex KType 0)) "$match.4.x") :| [])
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
