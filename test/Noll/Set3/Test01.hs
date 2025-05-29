{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Noll.Set3.Test01 where

import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Noll.Language
import Noll.Language.Type.Intrinsic
import Noll.Module

import qualified Noll.Module as Module

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
    [ -- instance Traceable(string)
      DInstance2
        "Traceable"
        (TIntrinsic IString)
        [ DFunction
            "trace"
            ( Function
                ()
                undefined
                undefined
                undefined
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
                undefined
                undefined
                undefined
            )
        ]
    , -- instance Traceable((a, b))
      DInstance2
        "Traceable"
        (TIntrinsic (ITuple [TVariable (Parameter () "a"), TVariable (Parameter () "b")]))
        [ DFunction
            "trace"
            ( Function
                ()
                undefined
                undefined
                undefined
            )
        ]
    , -- instance Traceable(list(a))
      DInstance2
        "Traceable"
        (TIntrinsic (IList (TVariable (Parameter () "a"))))
        [ DFunction
            "trace"
            ( Function
                ()
                undefined
                undefined
                undefined
            )
        ]
    , -- pair1
      DConstant
        "pair1"
        undefined
    , -- list1
      DConstant
        "list1"
        undefined
    ]
