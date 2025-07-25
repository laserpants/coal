{-# LANGUAGE OverloadedStrings #-}

module Coal.Set7.Test01 where

import Coal.Common.List1 (NonEmpty (..), (<|))
import Coal.Common.Label (Label (..))
import Coal.Language
import Coal.Language.Module (Definition (..), Function (..), Module (..), Path (..))

import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import qualified Coal.Language.Module as Module

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
