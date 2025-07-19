{-# LANGUAGE OverloadedStrings #-}

module Noll.Set2.Test10 where

import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Noll.Language
import Noll.Language.Type.Intrinsic
import Noll.Module

import qualified Data.Map.Strict as Map
import qualified Noll.Module as Module

-- Dictionary insertion
prog2_10 :: [Module () Kind IndexedType]
prog2_10 =
  [ moduleFoo
  ]

pairType :: IndexedType
pairType = TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)])

traceableTrait t =
  TApplication
    KTrait
    (TConstructor (KType `KArrow` KTrait) "Traceable")
    (t :| [])

moduleFoo :: Module () Kind IndexedType
moduleFoo =
  Module.fromDefinitionList
    (Path ["Foo"])
    -- Exports
    []
    -- Definitions
    [ DTrait
        "Traceable"
        []
        (TVariable (Parameter () "a"))
        [
          ( "trace"
          , TVariable (Parameter () "a") `TArrow` TIntrinsic IString
          )
        ]
    , -- instance Traceable(string)
      DInstance
        "Traceable"
        (TIntrinsic IString)
        [ DConstant
            "trace"
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
    , -- instance Traceable(int32)
      DInstance
        "Traceable"
        (TIntrinsic IInt32)
        [ DConstant
            "trace"
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
    , -- instance Traceable((a, b)) with Traceable(a), Traceable(b)
      DInstance
        "Traceable"
        (TIntrinsic (ITuple [TVariable (Parameter () "a"), TVariable (Parameter () "b")]))
        [ DConstant
            "trace"
            ( Constant
                ()
                ( With
                    [ Trait "Traceable" (TVariable (TypeIndex KType 0))
                    , Trait "Traceable" (TVariable (TypeIndex KType 1))
                    ]
                    (pairType `TArrow` TIntrinsic IString)
                )
                ( ELambda
                    ()
                    ( PPlaceholder () (traceableTrait (TVariable (TypeIndex KType 0))) (Trait "Traceable" (TVariable (TypeIndex KType 0)))
                        <| PPlaceholder () (traceableTrait (TVariable (TypeIndex KType 1))) (Trait "Traceable" (TVariable (TypeIndex KType 1)))
                        :| []
                    )
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
                                        ( EApplication
                                            ()
                                            (TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IString)
                                            (EVariable () (Label (traceableTrait (TVariable (TypeIndex KType 0)) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IString) "trace"))
                                            (EPlaceholder () (traceableTrait (TVariable (TypeIndex KType 0))) (Trait "Traceable" (TVariable (TypeIndex KType 0))) :| [])
                                        )
                                        (EVariable () (Label (TVariable (TypeIndex KType 0)) "$match.1.x") :| [])
                                        <| EApplication
                                          ()
                                          (TIntrinsic IString)
                                          ( EApplication
                                              ()
                                              (TVariable (TypeIndex KType 1) `TArrow` TIntrinsic IString)
                                              (EVariable () (Label (traceableTrait (TVariable (TypeIndex KType 1)) `TArrow` TVariable (TypeIndex KType 1) `TArrow` TIntrinsic IString) "trace"))
                                              (EPlaceholder () (traceableTrait (TVariable (TypeIndex KType 1))) (Trait "Traceable" (TVariable (TypeIndex KType 1))) :| [])
                                          )
                                          (EVariable () (Label (TVariable (TypeIndex KType 1)) "$match.2.y") :| [])
                                        :| []
                                    )
                                )
                                :| []
                            )
                        )
                    )
                )
            )
        ]
    , -- instance Traceable(list(a)) with Traceable(a)
      DInstance
        "Traceable"
        (TIntrinsic (IList (TVariable (Parameter () "a"))))
        [ DConstant
            "trace"
            ( Constant
                ()
                (With [Trait "Traceable" (TVariable (TypeIndex KType 0))] (TIntrinsic (IList (TVariable (TypeIndex KType 0))) `TArrow` TIntrinsic IString))
                ( ELambda
                    ()
                    (PPlaceholder () (traceableTrait (TVariable (TypeIndex KType 0))) (Trait "Traceable" (TVariable (TypeIndex KType 0))) :| [])
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
                                    ( EApplication
                                        ()
                                        (TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IString)
                                        (EVariable () (Label (traceableTrait (TVariable (TypeIndex KType 0)) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IString) "trace"))
                                        (EPlaceholder () (traceableTrait (TVariable (TypeIndex KType 0))) (Trait "Traceable" (TVariable (TypeIndex KType 0))) :| [])
                                    )
                                    (EVariable () (Label (TVariable (TypeIndex KType 0)) "$match.4.x") :| [])
                                )
                                :| []
                            )
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
                    ( EApplication
                        ()
                        (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]) `TArrow` TIntrinsic IString)
                        (EVariable () (Label (traceableTrait (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString])) `TArrow` traceableTrait (TIntrinsic IInt32) `TArrow` traceableTrait (TIntrinsic IString) `TArrow` TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]) `TArrow` TIntrinsic IString) "trace"))
                        ( EPlaceholder () (traceableTrait (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]))) (Trait "Traceable" (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString])))
                            <| EPlaceholder () (traceableTrait (TIntrinsic IInt32)) (Trait "Traceable" (TIntrinsic IInt32))
                            <| EPlaceholder () (traceableTrait (TIntrinsic IString)) (Trait "Traceable" (TIntrinsic IString))
                            :| []
                        )
                    )
                    (EVariable () (Label (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString])) "p") :| [])
                )
            )
        )
    , DConstant
        "baz"
        ( Constant
            ()
            ( With
                [ Trait "Traceable" (TVariable (TypeIndex KType 0))
                , Trait "Traceable" (TVariable (TypeIndex KType 1))
                ]
                (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 1) `TArrow` TIntrinsic IString)
            )
            ( ELambda
                ()
                ( PPlaceholder () (traceableTrait (TVariable (TypeIndex KType 0))) (Trait "Traceable" (TVariable (TypeIndex KType 0)))
                    <| PPlaceholder () (traceableTrait (TVariable (TypeIndex KType 1))) (Trait "Traceable" (TVariable (TypeIndex KType 1)))
                    :| []
                )
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
                            ( EApplication
                                ()
                                (pairType `TArrow` TIntrinsic IString)
                                (EVariable () (Label (traceableTrait pairType `TArrow` traceableTrait (TVariable (TypeIndex KType 0)) `TArrow` traceableTrait (TVariable (TypeIndex KType 1)) `TArrow` pairType `TArrow` TIntrinsic IString) "trace"))
                                ( EPlaceholder () (traceableTrait pairType) (Trait "Traceable" pairType)
                                    <| EPlaceholder () (traceableTrait (TVariable (TypeIndex KType 0))) (Trait "Traceable" (TVariable (TypeIndex KType 0)))
                                    <| EPlaceholder () (traceableTrait (TVariable (TypeIndex KType 1))) (Trait "Traceable" (TVariable (TypeIndex KType 1)))
                                    :| []
                                )
                            )
                            (EVariable () (Label pairType "p") :| [])
                        )
                    )
                )
            )
        )
    , DConstant
        "bar"
        ( Constant
            ()
            ( With
                [ Trait "Traceable" (TVariable (TypeIndex KType 0))
                , Trait "Traceable" (TVariable (TypeIndex KType 1))
                ]
                (TIntrinsic (IList pairType) `TArrow` TIntrinsic IString)
            )
            ( ELambda
                ()
                ( PPlaceholder () (traceableTrait (TVariable (TypeIndex KType 0))) (Trait "Traceable" (TVariable (TypeIndex KType 0)))
                    <| PPlaceholder () (traceableTrait (TVariable (TypeIndex KType 1))) (Trait "Traceable" (TVariable (TypeIndex KType 1)))
                    :| []
                )
                ( ELambda
                    ()
                    (PVariable () (Label (TIntrinsic (IList pairType)) "xs") :| [])
                    ( EApplication
                        ()
                        (TIntrinsic IString)
                        ( EApplication
                            ()
                            (TIntrinsic (IList pairType) `TArrow` TIntrinsic IString)
                            (EVariable () (Label (traceableTrait (TIntrinsic (IList pairType)) `TArrow` TIntrinsic (IList pairType) `TArrow` TIntrinsic IString) "trace"))
                            ( EPlaceholder () (traceableTrait (TIntrinsic (IList pairType))) (Trait "Traceable" (TIntrinsic (IList pairType)))
                                <| ERecord
                                  ()
                                  ( TIntrinsic (IRecord (TRow (RExtend "trace" (pairType `TArrow` TIntrinsic IString) RNil)))
                                  )
                                  ( Map.fromList
                                      [
                                        ( "trace"
                                        , EApplication
                                            ()
                                            (pairType `TArrow` TIntrinsic IString)
                                            (EVariable () (Label (traceableTrait pairType `TArrow` traceableTrait (TVariable (TypeIndex KType 0)) `TArrow` traceableTrait (TVariable (TypeIndex KType 1)) `TArrow` pairType `TArrow` TIntrinsic IString) "trace"))
                                            ( EPlaceholder () (traceableTrait pairType) (Trait "Traceable" pairType)
                                                <| EPlaceholder () (traceableTrait (TVariable (TypeIndex KType 0))) (Trait "Traceable" (TVariable (TypeIndex KType 0)))
                                                <| EPlaceholder () (traceableTrait (TVariable (TypeIndex KType 1))) (Trait "Traceable" (TVariable (TypeIndex KType 1)))
                                                :| []
                                            )
                                        )
                                      ]
                                  )
                                  Nothing
                                :| []
                            )
                        )
                        (EVariable () (Label (TIntrinsic (IList pairType)) "xs") :| [])
                    )
                )
            )
        )
    ]
