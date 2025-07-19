{-# LANGUAGE OverloadedStrings #-}

module Noll.Set3.Test05 where

import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Noll.Language
import Noll.Language.Type.Intrinsic
import Noll.Module

import qualified Noll.Module as Module

-- Normalize top-level definitions
prog3_05 :: [Module () Kind IndexedType]
prog3_05 =
  [ moduleMain
  ]

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
      DInstance2
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
      DInstance2
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
      DInstance2
        "Traceable"
        (TIntrinsic (ITuple [TVariable (Parameter () "a"), TVariable (Parameter () "b")]))
        [ DConstant
            "trace"
            ( Constant
                ()
                (With [] (TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)]) `TArrow` TIntrinsic IString))
                ( ELambda
                    ()
                    (PVariable () (Label (TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)])) "p") :| [])
                    ( EApplication
                        ()
                        (TIntrinsic IString)
                        (EVariable () (Label (TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)]) `TArrow` TIntrinsic IString) "pair_to_string"))
                        (EVariable () (Label (TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)])) "p") :| [])
                    )
                )
            )
        ]
    , -- instance Traceable(list(a))
      DInstance2
        "Traceable"
        (TIntrinsic (IList (TVariable (Parameter () "a"))))
        [ DConstant
            "trace"
            ( Constant
                ()
                (With [] (TIntrinsic (IList (TVariable (TypeIndex KType 0))) `TArrow` TIntrinsic IString))
                ( ELambda
                    ()
                    (PVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 0)))) "lst") :| [])
                    ( EApplication
                        ()
                        (TIntrinsic IString)
                        (EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 0))) `TArrow` TIntrinsic IString) "list_to_string"))
                        (EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 0)))) "lst") :| [])
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
                    (EVariable () (Label (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]) `TArrow` TIntrinsic IString) "trace"))
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
                (EVariable () (Label (TIntrinsic (IList (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]))) `TArrow` TIntrinsic IString) "trace"))
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
