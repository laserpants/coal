{-# LANGUAGE OverloadedStrings #-}

module Noll.Set11.Test01 where

import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Noll.Language
import Noll.Module (Definition (..), Function (..), Module (..), Path (..))

import qualified Noll.Module as Module

-- prog7_01 :: [Module () o ()]
-- prog7_01 =
--  [ moduleMain
--  ]

moduleMain :: Module () o ()
moduleMain =
  Module.fromDefinitionList
    (Path ["Main"])
    -- Exports
    []
    -- Definitions
    [ DImport (Path ["Core$"]) ["trace_int32"]
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
                ( EMatch
                      ()
                      ()
                      ( EApplication
                          ()
                          ()
                          (EConstructor () (Label () "Succ"))
                          ( EConstructor () (Label () "Zero")
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
                            ( PConstructor
                                ()
                                (Label () "Succ")
                                [ PVariable () (Label () "n")
                                ]
                            )
                            ( CPlain
                                ()
                                []
                                (ELiteral () (LInt32 8))
                                :| []
                            )
                          :| []
                      )
                      :| []
                )
            )
        )
    ]
