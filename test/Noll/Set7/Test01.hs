{-# LANGUAGE OverloadedStrings #-}

module Noll.Set7.Test01 where

import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Noll.Language
import Noll.Language.Module (Definition (..), Function (..), Module (..), Path (..))

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Noll.Language.Module as Module

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
                ( ELiteral () (LString (Text.encodeUtf8 (Text.pack "Hello 🤖, world!")))
                    :| []
                )
            )
        )
    ]
