{-# LANGUAGE OverloadedStrings #-}

module Noll.Set7.Test01 where

import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Noll.Language
import Noll.Module (Constant (..), Definition (..), Function (..), Module (..), Path (..))

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Noll.Module as Module

prog7_01 :: [Module () () ()]
prog7_01 =
  [ moduleMain
  ]

moduleMain :: Module () () ()
moduleMain =
  Module.fromDefinitionList
    (Path ["Main"])
    -- Exports
    []
    -- Definitions
    [ DFunction
        "main"
        ( Function
            ()
            (With [] ())
            (PLiteral () LUnit :| [])
            ( EApplication
                ()
                ()
                (EVariable () (Label () "trace_string"))
                ( ELiteral () (LString "Hello 🤖")
                    :| []
                )
            )
        )
    ]
