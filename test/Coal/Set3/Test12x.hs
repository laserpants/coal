{-# LANGUAGE OverloadedStrings #-}

module Coal.Set3.Test12x where

import Coal.Common.List1 (NonEmpty (..), (<|))
import Coal.Common.Label (Label (..))
import Coal.Language
import Coal.Language.Type.Intrinsic
import Coal.Language.Module

import qualified Data.Map.Strict as Map
import qualified Coal.Language.Module as Module

prog3_12x :: [Module () Kind IndexedType]
prog3_12x =
  [ moduleMain
  ]

traceableTrait t =
  TApplication
    KTrait
    (TConstructor (KType `KArrow` KTrait) "Traceable")
    (t :| [])

moduleMain :: Module () Kind IndexedType
moduleMain =
  Module.fromDefinitionList
    (Path ["Main"])
    -- Exports
    []
    -- Definitions
    [ DImport
        (Path ["Core$"])
        [ "int32_to_string"
        , "pair_to_string"
        , "list_to_string"
        , "trace"
        , "trace_string"
        ]
    , -- instance Traceable(string)
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
    , -- instance Traceable(int32)
      DInstance
        "Traceable"
        (TIntrinsic IInt32)
        [ DFunction
            "trace"
            ( Function
                ()
                (With [] (TIntrinsic IString))
                (PVariable () (Label (TIntrinsic IInt32) "n") :| [])
                ( EApplication
                    ()
                    (TIntrinsic IString)
                    (EVariable () (Label (TIntrinsic IInt32 `TArrow` TIntrinsic IString) "int32_to_string"))
                    (EVariable () (Label (TIntrinsic IInt32) "n") :| [])
                )
            )
        ]
    , -- instance Traceable((a, b))
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
                ( PPlaceholder () (traceableTrait (TVariable (TypeIndex KType 0))) (Trait "Traceable" (TVariable (TypeIndex KType 0)))
                    <| PPlaceholder () (traceableTrait (TVariable (TypeIndex KType 1))) (Trait "Traceable" (TVariable (TypeIndex KType 1)))
                    <| PVariable () (Label (TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)])) "p")
                    :| []
                )
                ( EApplication
                    ()
                    (TIntrinsic IString)
                    ( EApplication
                        ()
                        (TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)]) `TArrow` TIntrinsic IString)
                        ( EVariable
                            ()
                            ( Label
                                ( traceableTrait (TVariable (TypeIndex KType 0))
                                    `TArrow` traceableTrait (TVariable (TypeIndex KType 1))
                                    `TArrow` TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)])
                                    `TArrow` TIntrinsic IString
                                )
                                "pair_to_string"
                            )
                        )
                        ( EPlaceholder () (traceableTrait (TVariable (TypeIndex KType 0))) (Trait "Traceable" (TVariable (TypeIndex KType 0)))
                            <| EPlaceholder () (traceableTrait (TVariable (TypeIndex KType 1))) (Trait "Traceable" (TVariable (TypeIndex KType 1)))
                            :| []
                        )
                    )
                    ( EVariable () (Label (TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)])) "p")
                        :| []
                    )
                )
            )
        ]
    , -- instance Traceable(list(a))
      DInstance
        "Traceable"
        (TIntrinsic (IList (TVariable (Parameter () "a"))))
        [ DFunction
            "trace"
            ( Function
                ()
                ( With
                    [ Trait "Traceable" (TVariable (TypeIndex KType 0))
                    ]
                    (TIntrinsic IString)
                )
                ( PPlaceholder () (traceableTrait (TVariable (TypeIndex KType 0))) (Trait "Traceable" (TVariable (TypeIndex KType 0)))
                    <| PVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 0)))) "lst")
                    :| []
                )
                ( EApplication
                    ()
                    (TIntrinsic IString)
                    ( EApplication
                        ()
                        (TIntrinsic (IList (TVariable (TypeIndex KType 0))) `TArrow` TIntrinsic IString)
                        ( EVariable
                            ()
                            ( Label
                                ( traceableTrait (TVariable (TypeIndex KType 0))
                                    `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 0)))
                                    `TArrow` TIntrinsic IString
                                )
                                "list_to_string"
                            )
                        )
                        ( EPlaceholder () (traceableTrait (TVariable (TypeIndex KType 0))) (Trait "Traceable" (TVariable (TypeIndex KType 0)))
                            :| []
                        )
                    )
                    (EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 0)))) "lst") :| [])
                )
            )
        ]
    , -- pair1
      DConstant
        "pair1"
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
                        ( EVariable
                            ()
                            ( Label
                                ( traceableTrait (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]))
                                    `TArrow` TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString])
                                    `TArrow` TIntrinsic IString
                                )
                                "trace"
                            )
                        )
                        ( ERecord
                            ()
                            (traceableTrait (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString])))
                            ( Map.fromList
                                [
                                  ( "trace"
                                  , EApplication
                                      ()
                                      (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]) `TArrow` TIntrinsic IString)
                                      ( EVariable
                                          ()
                                          ( Label
                                              ( traceableTrait (TIntrinsic IInt32)
                                                  `TArrow` traceableTrait (TIntrinsic IString)
                                                  `TArrow` TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString])
                                                  `TArrow` TIntrinsic IString
                                              )
                                              "trace__$instance.a2de7bde6bbaafb6"
                                          )
                                      )
                                      ( ERecord
                                          ()
                                          (traceableTrait (TIntrinsic IInt32))
                                          ( Map.fromList
                                              [
                                                ( "trace"
                                                , EVariable () (Label (TIntrinsic IInt32 `TArrow` TIntrinsic IString) "trace__$instance.c847f12006235dc0")
                                                )
                                              ]
                                          )
                                          Nothing
                                          <| ERecord
                                            ()
                                            (traceableTrait (TIntrinsic IString))
                                            ( Map.fromList
                                                [
                                                  ( "trace"
                                                  , EVariable () (Label (TIntrinsic IString `TArrow` TIntrinsic IString) "trace__$instance.c81d5162b7d14248")
                                                  )
                                                ]
                                            )
                                            Nothing
                                          :| []
                                      )
                                  )
                                ]
                            )
                            Nothing
                            :| []
                        )
                    )
                    ( EVariable () (Label (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString])) "p")
                        :| []
                    )
                )
            )
        )
    , -- list1
      DConstant
        "list1"
        ( Constant
            ()
            (With [] (TIntrinsic IString))
            ( EApplication
                ()
                (TIntrinsic IString)
                ( EApplication
                    ()
                    (TIntrinsic (IList (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]))) `TArrow` TIntrinsic IString)
                    (EVariable () (Label (traceableTrait (TIntrinsic (IList (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString])))) `TArrow` TIntrinsic (IList (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]))) `TArrow` TIntrinsic IString) "trace"))
                    ( ERecord
                        ()
                        (traceableTrait (TIntrinsic (IList (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString])))))
                        ( Map.fromList
                            [
                              ( "trace"
                              , -- EApplication
                                --  ()
                                --  (TIntrinsic (IList (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]))) `TArrow` TIntrinsic IString)
                                --  (EVariable () (Label (traceableTrait (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString])) `TArrow` TIntrinsic (IList (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]))) `TArrow` TIntrinsic IString) "trace"))
                                --  ( ERecord
                                --      ()
                                --      (traceableTrait (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString])))
                                --      ( Map.fromList
                                --          [
                                --            ( "trace"
                                --          ,
                                EApplication
                                  ()
                                  (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]) `TArrow` TIntrinsic IString)
                                  ( EVariable
                                      ()
                                      ( Label
                                          ( traceableTrait (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]))
                                              `TArrow` TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString])
                                              `TArrow` TIntrinsic IString
                                          )
                                          "trace__$instance.fcea41ba44fb0cf4"
                                      )
                                  )
                                  ( ERecord
                                      ()
                                      (traceableTrait (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString])))
                                      ( Map.fromList
                                          [
                                            ( "trace"
                                            , EApplication
                                                ()
                                                (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]) `TArrow` TIntrinsic IString)
                                                ( EVariable
                                                    ()
                                                    ( Label
                                                        ( traceableTrait (TIntrinsic IInt32)
                                                            `TArrow` traceableTrait (TIntrinsic IString)
                                                            `TArrow` TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString])
                                                            `TArrow` TIntrinsic IString
                                                        )
                                                        "trace__$instance.a2de7bde6bbaafb6"
                                                    )
                                                )
                                                ( ERecord
                                                    ()
                                                    (traceableTrait (TIntrinsic IInt32))
                                                    ( Map.fromList
                                                        [
                                                          ( "trace"
                                                          , EVariable () (Label (TIntrinsic IInt32 `TArrow` TIntrinsic IString) "trace__$instance.c847f12006235dc0")
                                                          )
                                                        ]
                                                    )
                                                    Nothing
                                                    <| ERecord
                                                      ()
                                                      (traceableTrait (TIntrinsic IString))
                                                      ( Map.fromList
                                                          [
                                                            ( "trace"
                                                            , EVariable () (Label (TIntrinsic IString `TArrow` TIntrinsic IString) "trace__$instance.c81d5162b7d14248")
                                                            )
                                                          ]
                                                      )
                                                      Nothing
                                                    :| []
                                                )
                                            )
                                          ]
                                      )
                                      Nothing
                                      :| []
                                  )
                                  --                                            )
                                  --                                          ]
                                  --                                      )
                                  --                                      Nothing
                                  --                                      :| []
                                  --                                  )
                              )
                            ]
                        )
                        Nothing
                        :| []
                    )
                )
                ( EListLiteral
                    ()
                    (TIntrinsic (IList (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]))))
                    [ ETuple
                        ()
                        (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]))
                        ( ELiteral () (LInt32 1)
                            <| ELiteral () (LString "a")
                            :| []
                        )
                    , ETuple
                        ()
                        (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]))
                        ( ELiteral () (LInt32 2)
                            <| ELiteral () (LString "b")
                            :| []
                        )
                    ]
                    :| []
                )
            )
        )
    , -- main
      DFunction
        "main"
        ( Function
            ()
            (With [] (TVariable (TypeIndex KType 0)))
            (PVariable () (Label (TIntrinsic IUnit) "$v.0") :| [])
            ( EApplication
                ()
                (TVariable (TypeIndex KType 0))
                (EVariable () (Label (TIntrinsic IString `TArrow` TVariable (TypeIndex KType 0)) "trace_string"))
                ( EVariable () (Label (TIntrinsic IString) "list1")
                    :| []
                )
            )
        )
    ]
