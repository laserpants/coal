{-# LANGUAGE OverloadedStrings #-}

module Noll.Set10.Test01 where

import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Noll.Language
import Noll.Module (Definition (..), Function (..), Module (..), Path (..))

import qualified Noll.Module as Module

-- prog7_01 :: [Module () o ()]
-- prog7_01 =
--  [ moduleMain
--  ]

moduleUtilities :: Module () o ()
moduleUtilities =
  Module.fromDefinitionList
    (Path ["Utilities"])
    -- Exports
    ["increment"]
    -- Definitions
    [ DImport (Path ["Core$"]) ["pack_nat"]
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
            ( EFold
                ()
                ()
                ( EApplication
                    ()
                    ()
                    (EVariable () (Label () "pack_nat"))
                    ( EVariable () (Label () "n")
                        :| []
                    )
                    :| []
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
                        undefined
                        :| []
                    )
                    <| EClause
                      ()
                      ( PConstructor
                          ()
                          (Label () "Succ")
                          [ PAtVariable () (Label () "n")
                          ]
                      )
                      ( CPlain
                          ()
                          []
                          undefined
                          :| []
                      )
                    :| []
                )
                Nothing
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
    , DImport (Path ["Utilities"]) ["increment"]
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
                    (ELiteral () (LInt32 1) :| [])
                    :| []
                )
            )
        )
    ]
