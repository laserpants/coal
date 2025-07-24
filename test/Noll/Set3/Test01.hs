{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Noll.Set3.Test01 where

import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Noll.Language
import Noll.Language.Type.Intrinsic
import Noll.Language.Module

import qualified Noll.Language.Module as Module

-- Untyped source tree
prog3_01 :: [Module () () ()]
prog3_01 =
  [ moduleMain
  ]

moduleMain :: Module () () ()
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
        [ DFunction
            "trace"
            ( Function
                ()
                (With [] ())
                (PVariable () (Label () "s") :| [])
                (EVariable () (Label () "s"))
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
                (With [] ())
                (PVariable () (Label () "n") :| [])
                ( EApplication
                    ()
                    ()
                    (EVariable () (Label () "int32_to_string"))
                    (EVariable () (Label () "n") :| [])
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
                (With [] ())
                (PVariable () (Label () "p") :| [])
                ( EApplication
                    ()
                    ()
                    (EVariable () (Label () "pair_to_string"))
                    (EVariable () (Label () "p") :| [])
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
                (With [] ())
                (PVariable () (Label () "lst") :| [])
                ( EApplication
                    ()
                    ()
                    (EVariable () (Label () "list_to_string"))
                    (EVariable () (Label () "lst") :| [])
                )
            )
        ]
    , -- pair1
      DConstant
        "pair1"
        ( Constant
            ()
            (With [] ())
            ( ELet
                ()
                ( BPattern
                    ()
                    (PVariable () (Label () "p"))
                    ( ETuple
                        ()
                        ()
                        ( ELiteral () (LInt32 1)
                            <| ELiteral () (LString "hello")
                            :| []
                        )
                    )
                    :| []
                )
                ( EApplication
                    ()
                    ()
                    (EVariable () (Label () "trace"))
                    ( EVariable () (Label () "p")
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
            (With [] ())
            ( EApplication
                ()
                ()
                (EVariable () (Label () "trace"))
                ( EListLiteral
                    ()
                    ()
                    [ ETuple
                        ()
                        ()
                        ( ELiteral () (LInt32 1)
                            <| ELiteral () (LString "a")
                            :| []
                        )
                    , ETuple
                        ()
                        ()
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
