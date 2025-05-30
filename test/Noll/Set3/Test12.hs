{-# LANGUAGE OverloadedStrings #-}

module Noll.Set3.Test12 where

import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Noll.Language
import Noll.Language.Type.Intrinsic
import Noll.Module

import qualified Data.Map.Strict as Map
import qualified Noll.Module as Module

-- Denormalization
prog3_12 :: [Module () Kind IndexedType]
prog3_12 =
  [ moduleMain
  ]

traceType t =
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
      DInstance2
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
      DInstance2
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
      DInstance2
        "Traceable"
        (TIntrinsic (ITuple [TVariable (Parameter KType "a"), TVariable (Parameter KType "b")]))
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
                ( PDictionary () (traceType (TVariable (TypeIndex KType 0))) (Trait "Traceable" (TVariable (TypeIndex KType 0)))
                    <| PDictionary () (traceType (TVariable (TypeIndex KType 1))) (Trait "Traceable" (TVariable (TypeIndex KType 1)))
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
                                ( traceType (TVariable (TypeIndex KType 0))
                                    `TArrow` traceType (TVariable (TypeIndex KType 1))
                                    `TArrow` TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)])
                                    `TArrow` TIntrinsic IString
                                )
                                "pair_to_string"
                            )
                        )
                        ( EDictionary () (traceType (TVariable (TypeIndex KType 0))) (Trait "Traceable" (TVariable (TypeIndex KType 0)))
                            <| EDictionary () (traceType (TVariable (TypeIndex KType 1))) (Trait "Traceable" (TVariable (TypeIndex KType 1)))
                            :| []
                        )
                    )
                    (EVariable () (Label (TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)])) "p") :| [])
                )
            )
        ]
    , -- instance Traceable(list(a))
      DInstance2
        "Traceable"
        (TIntrinsic (IList (TVariable (Parameter KType "a"))))
        [ DFunction
            "trace"
            ( Function
                ()
                ( With
                    [ Trait "Traceable" (TVariable (TypeIndex KType 0))
                    ]
                    (TIntrinsic IString)
                )
                ( PDictionary () (traceType (TVariable (TypeIndex KType 0))) (Trait "Traceable" (TVariable (TypeIndex KType 0)))
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
                                ( traceType (TVariable (TypeIndex KType 0))
                                    `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 0)))
                                    `TArrow` TIntrinsic IString
                                )
                                "list_to_string"
                            )
                        )
                        (EDictionary () (traceType (TVariable (TypeIndex KType 0))) (Trait "Traceable" (TVariable (TypeIndex KType 0))) :| [])
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
                                ( traceType (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]))
                                    `TArrow` traceType (TIntrinsic IInt32)
                                    `TArrow` traceType (TIntrinsic IString)
                                    `TArrow` TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString])
                                    `TArrow` TIntrinsic IString
                                )
                                "trace"
                            )
                        )
                        ( EDictionary () (traceType (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]))) (Trait "Traceable" (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString])))
                            <| EDictionary () (traceType (TIntrinsic IInt32)) (Trait "Traceable" (TIntrinsic IInt32))
                            <| EDictionary () (traceType (TIntrinsic IString)) (Trait "Traceable" (TIntrinsic IString))
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
                    ( EDictionary () (traceType (TIntrinsic (IList (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]))))) (Trait "Traceable" (TIntrinsic (IList (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString])))))
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
                                            ( traceType (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]))
                                                `TArrow` traceType (TIntrinsic IInt32)
                                                `TArrow` traceType (TIntrinsic IString)
                                                `TArrow` TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString])
                                                `TArrow` TIntrinsic IString
                                            )
                                            "trace"
                                        )
                                    )
                                    ( EDictionary () (traceType (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]))) (Trait "Traceable" (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString])))
                                        <| EDictionary () (traceType (TIntrinsic IInt32)) (Trait "Traceable" (TIntrinsic IInt32))
                                        <| EDictionary () (traceType (TIntrinsic IString)) (Trait "Traceable" (TIntrinsic IString))
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
