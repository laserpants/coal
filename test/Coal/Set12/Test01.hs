{-# LANGUAGE OverloadedStrings #-}

module Coal.Set12.Test01 where

import Coal.Common.List1 (NonEmpty (..), (<|))
import Coal.Common.Label (Label (..))
import Coal.Language
import Coal.Language.Module (Definition (..), Function (..), Module (..), Path (..))

import qualified Coal.Language.Module as Module

prog10_01 :: [Module () o ()]
prog10_01 =
  [ moduleUtilities
  , moduleMain
  ]

moduleUtilities :: Module () o ()
moduleUtilities =
  Module.fromDefinitionList
    (Path ["Utilities"])
    -- Exports
    ["increment"]
    -- Definitions
    [ DImport (Path ["Core$"]) ["pack_nat", "unpack_nat", "trace_int32"]
    , DFunction
        "increment"
        ( Function
            ()
            (With [] ())
            (PVariable () (Label () "a") :| [])
            ( EApplication
                ()
                ()
                (EBinaryOperator () () OAddition)
                ( EVariable () (Label () "a")
                    <| ELiteral () (LInt32 1)
                    :| []
                )
            )
        )
    , DFunction
        "factorial"
        ( Function
            ()
            (With [] ())
            (PVariable () (Label () "n") :| [])
            ( EMatch
                ()
                ()
                ( EApplication
                    ()
                    ()
                    (EConstructor () (Label () "$Succ"))
                    ( ELiteral () (LInt32 4) -- EConstructor () (Label () "Zero")
                        :| []
                    )
                )
                ( EClause
                    ()
                    ( PConstructor
                        ()
                        (Label () "Zero")
                        []
                    )
                    ( CPlain
                        ()
                        []
                        (ELiteral () (LInt32 1))
                        :| []
                    )
                    <| EClause
                      ()
                      (PVariable () (Label () "m"))
                      ( CPlain
                          ()
                          []
                          (ELiteral () (LInt32 401))
                          :| []
                      )
                    :| []
                )
            )
        )
    ]

moduleMain :: Module () o ()
moduleMain =
  Module.fromDefinitionList
    (Path ["Main"])
    -- Exports
    []
    -- Definitions
    [ DImport (Path ["Core$"]) ["trace_int32"]
    , DImport (Path ["Utilities"]) ["factorial", "increment"]
    , DFunction
        "main"
        ( Function
            ()
            (With [] ())
            (PLiteral () LUnit :| [])
            ( EApplication
                ()
                ()
                (EVariable () (Label () "trace_int32"))
                ( EApplication
                    ()
                    ()
                    (EVariable () (Label () "increment"))
                    ( EApplication
                        ()
                        ()
                        (EVariable () (Label () "factorial"))
                        ( ELiteral () (LInt32 4)
                            :| []
                        )
                        :| []
                    )
                    :| []
                )
            )
        )
    ]
