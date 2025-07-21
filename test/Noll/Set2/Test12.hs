{-# LANGUAGE OverloadedStrings #-}

module Noll.Set2.Test12 where

import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Noll.Language
import Noll.Language.Type.Intrinsic
import Noll.Module

import qualified Data.Map.Strict as Map
import qualified Noll.Module as Module

-- Denormalization
prog2_12 :: [Module () Kind IndexedType]
prog2_12 =
  [ moduleFoo
  ]

pairType :: IndexedType
pairType = TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)])

traceableType t =
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
        (Parameter KType "a")
        [
          ( "trace"
          , TVariable (Parameter () "a") `TArrow` TIntrinsic IString
          )
        ]
    , -- instance Show(string)
      DInstance
        "Traceable"
        (TIntrinsic IString)
        [ DFunction
            "trace"
            ( Function
                ()
                (With [] (TIntrinsic IString))
                (PVariable () (Label (TIntrinsic IString) "s") :| [])
                (EVariable () (Label (TIntrinsic IString) "s"))
            )
        ]
    , -- instance Show(int32)
      DInstance
        "Traceable"
        (TIntrinsic IInt32)
        [ DFunction
            "trace"
            ( Function
                ()
                (With [] (TIntrinsic IString))
                (PVariable () (Label (TIntrinsic IInt32) "n") :| [])
                (ELiteral () (LString "TODO"))
            )
        ]
    , -- instance Show((a, b)) with Show(a), Show(b)
      DInstance
        "Traceable"
        (TIntrinsic (ITuple [TVariable (Parameter () "a"), TVariable (Parameter () "b")]))
        [ DFunction
            "trace"
            ( Function
                ()
                ( With
                    [ Trait "Traceable" (TVariable (TypeIndex KType 0))
                    , Trait "Traceable" (TVariable (TypeIndex KType 1))
                    ]
                    (TIntrinsic IString)
                )
                ( PPlaceholder () (traceableType (TVariable (TypeIndex KType 0))) (Trait "Traceable" (TVariable (TypeIndex KType 0)))
                    <| PPlaceholder () (traceableType (TVariable (TypeIndex KType 1))) (Trait "Traceable" (TVariable (TypeIndex KType 1)))
                    <| PVariable () (Label pairType "p")
                    :| []
                )
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
                                    (EVariable () (Label (traceableType (TVariable (TypeIndex KType 0)) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IString) "trace"))
                                    (EPlaceholder () (traceableType (TVariable (TypeIndex KType 0))) (Trait "Traceable" (TVariable (TypeIndex KType 0))) :| [])
                                )
                                (EVariable () (Label (TVariable (TypeIndex KType 0)) "$match.1.x") :| [])
                                <| EApplication
                                  ()
                                  (TIntrinsic IString)
                                  ( EApplication
                                      ()
                                      (TVariable (TypeIndex KType 1) `TArrow` TIntrinsic IString)
                                      (EVariable () (Label (traceableType (TVariable (TypeIndex KType 1)) `TArrow` TVariable (TypeIndex KType 1) `TArrow` TIntrinsic IString) "trace"))
                                      (EPlaceholder () (traceableType (TVariable (TypeIndex KType 1))) (Trait "Traceable" (TVariable (TypeIndex KType 1))) :| [])
                                  )
                                  (EVariable () (Label (TVariable (TypeIndex KType 1)) "$match.2.y") :| [])
                                :| []
                            )
                        )
                        :| []
                    )
                )
            )
        ]
    , -- instance Show(list(a)) with Show(a)
      DInstance
        "Traceable"
        (TIntrinsic (IList (TVariable (Parameter () "a"))))
        [ DFunction
            "trace"
            ( Function
                ()
                (With [Trait "Traceable" (TVariable (TypeIndex KType 0))] (TIntrinsic IString))
                ( PPlaceholder () (traceableType (TVariable (TypeIndex KType 0))) (Trait "Traceable" (TVariable (TypeIndex KType 0)))
                    <| PVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 0)))) "lst")
                    :| []
                )
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
                                (EVariable () (Label (traceableType (TVariable (TypeIndex KType 0)) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IString) "trace"))
                                (EPlaceholder () (traceableType (TVariable (TypeIndex KType 0))) (Trait "Traceable" (TVariable (TypeIndex KType 0))) :| [])
                            )
                            (EVariable () (Label (TVariable (TypeIndex KType 0)) "$match.4.x") :| [])
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
                    ( EApplication
                        ()
                        (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]) `TArrow` TIntrinsic IString)
                        (EVariable () (Label (traceableType (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString])) `TArrow` traceableType (TIntrinsic IInt32) `TArrow` traceableType (TIntrinsic IString) `TArrow` TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]) `TArrow` TIntrinsic IString) "trace"))
                        ( EPlaceholder () (traceableType (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]))) (Trait "Traceable" (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString])))
                            <| EPlaceholder () (traceableType (TIntrinsic IInt32)) (Trait "Traceable" (TIntrinsic IInt32))
                            <| EPlaceholder () (traceableType (TIntrinsic IString)) (Trait "Traceable" (TIntrinsic IString))
                            :| []
                        )
                    )
                    (EVariable () (Label (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString])) "p") :| [])
                )
            )
        )
    , DFunction
        "baz"
        ( Function
            ()
            ( With
                [ Trait "Traceable" (TVariable (TypeIndex KType 0))
                , Trait "Traceable" (TVariable (TypeIndex KType 1))
                ]
                (TIntrinsic IString)
            )
            ( PPlaceholder () (traceableType (TVariable (TypeIndex KType 0))) (Trait "Traceable" (TVariable (TypeIndex KType 0)))
                <| PPlaceholder () (traceableType (TVariable (TypeIndex KType 1))) (Trait "Traceable" (TVariable (TypeIndex KType 1)))
                <| PVariable () (Label (TVariable (TypeIndex KType 0)) "x")
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
                        (EVariable () (Label (traceableType pairType `TArrow` traceableType (TVariable (TypeIndex KType 0)) `TArrow` traceableType (TVariable (TypeIndex KType 1)) `TArrow` pairType `TArrow` TIntrinsic IString) "trace"))
                        ( EPlaceholder () (traceableType pairType) (Trait "Traceable" pairType)
                            <| EPlaceholder () (traceableType (TVariable (TypeIndex KType 0))) (Trait "Traceable" (TVariable (TypeIndex KType 0)))
                            <| EPlaceholder () (traceableType (TVariable (TypeIndex KType 1))) (Trait "Traceable" (TVariable (TypeIndex KType 1)))
                            :| []
                        )
                    )
                    (EVariable () (Label pairType "p") :| [])
                )
            )
        )
    , DFunction
        "bar"
        ( Function
            ()
            ( With
                [ Trait "Traceable" (TVariable (TypeIndex KType 0))
                , Trait "Traceable" (TVariable (TypeIndex KType 1))
                ]
                (TIntrinsic IString)
            )
            ( PPlaceholder () (traceableType (TVariable (TypeIndex KType 0))) (Trait "Traceable" (TVariable (TypeIndex KType 0)))
                <| PPlaceholder () (traceableType (TVariable (TypeIndex KType 1))) (Trait "Traceable" (TVariable (TypeIndex KType 1)))
                <| PVariable () (Label (TIntrinsic (IList pairType)) "xs")
                :| []
            )
            ( EApplication
                ()
                (TIntrinsic IString)
                ( EApplication
                    ()
                    (TIntrinsic (IList pairType) `TArrow` TIntrinsic IString)
                    (EVariable () (Label (traceableType (TIntrinsic (IList pairType)) `TArrow` TIntrinsic (IList pairType) `TArrow` TIntrinsic IString) "trace"))
                    ( EPlaceholder () (traceableType (TIntrinsic (IList pairType))) (Trait "Traceable" (TIntrinsic (IList pairType)))
                        <| ERecord
                          ()
                          (TIntrinsic (IRecord (TRow (RExtend "trace" (pairType `TArrow` TIntrinsic IString) RNil))))
                          ( Map.fromList
                              [
                                ( "trace"
                                , EApplication
                                    ()
                                    (pairType `TArrow` TIntrinsic IString)
                                    (EVariable () (Label (traceableType pairType `TArrow` traceableType (TVariable (TypeIndex KType 0)) `TArrow` traceableType (TVariable (TypeIndex KType 1)) `TArrow` pairType `TArrow` TIntrinsic IString) "trace"))
                                    ( EPlaceholder () (traceableType pairType) (Trait "Traceable" pairType)
                                        <| EPlaceholder () (traceableType (TVariable (TypeIndex KType 0))) (Trait "Traceable" (TVariable (TypeIndex KType 0)))
                                        <| EPlaceholder () (traceableType (TVariable (TypeIndex KType 1))) (Trait "Traceable" (TVariable (TypeIndex KType 1)))
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
    ]
