{-# LANGUAGE OverloadedStrings #-}

module Noll.Set7.Test01 where

import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Noll.Language
import Noll.Module (Constant (..), Definition (..), Function (..), Module (..), Path (..))

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as TE
import qualified Noll.Module as Module

prog7_01 :: [Module () o ()]
prog7_01 =
  [ moduleMain
  ]

moduleMain :: Module () o ()
moduleMain =
  Module.fromDefinitionList
    (Path ["Main"])
    -- Exports
    []
    -- Definitions
    [ DImport (Path ["Core$"]) ["trace_string"]
    , DFunction
        "main"
        ( Function
            ()
            (With [] ())
            (PLiteral () LUnit :| [])
            ( EApplication
                ()
                ()
                (EVariable () (Label () "trace_string"))
                ( ELiteral () (LString (TE.encodeUtf8 (Text.pack "Hello 🤖, world!")))
                    :| []
                )
            )
        )
    ]
