{-# LANGUAGE OverloadedStrings #-}

module Noll.Set3.Test10 where

import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Noll.Language
import Noll.Language.Type.Intrinsic
import Noll.Language.Module

import qualified Data.Map.Strict as Map
import qualified Noll.Language.Module as Module

-- Dictionary insertion
prog3_10 :: [Module () Kind IndexedType]
prog3_10 =
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
                    ( EApplication
                        ()
                        (TIntrinsic IString)
                        (EVariable () (Label (TIntrinsic IInt32 `TArrow` TIntrinsic IString) "int32_to_string"))
                        (EVariable () (Label (TIntrinsic IInt32) "n") :| [])
                    )
                )
            )
        ]
    , -- instance Traceable((a, b))
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
                    (TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)]) `TArrow` TIntrinsic IString)
                )
                ( ELambda
                    ()
                    ( PPlaceholder () (traceableTrait (TVariable (TypeIndex KType 0))) (Trait "Traceable" (TVariable (TypeIndex KType 0)))
                        <| PPlaceholder () (traceableTrait (TVariable (TypeIndex KType 1))) (Trait "Traceable" (TVariable (TypeIndex KType 1)))
                        :| []
                    )
                    ( ELambda
                        ()
                        (PVariable () (Label (TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)])) "p") :| [])
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
                            (EVariable () (Label (TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)])) "p") :| [])
                        )
                    )
                )
            )
        ]
    , -- instance Traceable(list(a))
      DInstance
        "Traceable"
        (TIntrinsic (IList (TVariable (Parameter () "a"))))
        [ DConstant
            "trace"
            ( Constant
                ()
                ( With
                    [ Trait "Traceable" (TVariable (TypeIndex KType 0))
                    ]
                    (TIntrinsic (IList (TVariable (TypeIndex KType 0))) `TArrow` TIntrinsic IString)
                )
                ( ELambda
                    ()
                    (PPlaceholder () (traceableTrait (TVariable (TypeIndex KType 0))) (Trait "Traceable" (TVariable (TypeIndex KType 0))) :| [])
                    ( ELambda
                        ()
                        (PVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 0)))) "lst") :| [])
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
                                (EPlaceholder () (traceableTrait (TVariable (TypeIndex KType 0))) (Trait "Traceable" (TVariable (TypeIndex KType 0))) :| [])
                            )
                            (EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 0)))) "lst") :| [])
                        )
                    )
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
                                    `TArrow` traceableTrait (TIntrinsic IInt32)
                                    `TArrow` traceableTrait (TIntrinsic IString)
                                    `TArrow` TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString])
                                    `TArrow` TIntrinsic IString
                                )
                                "trace"
                            )
                        )
                        ( EPlaceholder () (traceableTrait (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]))) (Trait "Traceable" (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString])))
                            <| EPlaceholder () (traceableTrait (TIntrinsic IInt32)) (Trait "Traceable" (TIntrinsic IInt32))
                            <| EPlaceholder () (traceableTrait (TIntrinsic IString)) (Trait "Traceable" (TIntrinsic IString))
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
                    (EVariable () (Label (TIntrinsic (IList (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]))) `TArrow` TIntrinsic IString) "trace"))
                    ( EPlaceholder () (traceableTrait (TIntrinsic (IList (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]))))) (Trait "Traceable" (TIntrinsic (IList (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString])))))
                        <| ERecord
                          ()
                          (TIntrinsic (IRecord (TRow (RExtend "trace" (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]) `TArrow` TIntrinsic IString) RNil))))
                          ( Map.fromList
                              [
                                ( "trace"
                                , EApplication
                                    ()
                                    (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]) `TArrow` TIntrinsic IString)
                                    ( EVariable
                                        ()
                                        ( Label
                                            ( traceableTrait (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]))
                                                `TArrow` traceableTrait (TIntrinsic IInt32)
                                                `TArrow` traceableTrait (TIntrinsic IString)
                                                `TArrow` TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString])
                                                `TArrow` TIntrinsic IString
                                            )
                                            "trace"
                                        )
                                    )
                                    ( EPlaceholder () (traceableTrait (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]))) (Trait "Traceable" (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString])))
                                        <| EPlaceholder () (traceableTrait (TIntrinsic IInt32)) (Trait "Traceable" (TIntrinsic IInt32))
                                        <| EPlaceholder () (traceableTrait (TIntrinsic IString)) (Trait "Traceable" (TIntrinsic IString))
                                        :| []
                                    )
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
    ]
