{-# LANGUAGE OverloadedStrings #-}

module Noll.Set2.Test01 where

import Lang.Common.List1 (NonEmpty (..), (<|))
import Noll.Language.Type.Intrinsic
import Lang.Label (Label (..))
import Noll.Language
import Noll.Module 

import qualified Data.Set as Set
import qualified Noll.Module as Module

prog2_01 :: [Module () () ()]
prog2_01 =
  [ moduleFoo
  ]

moduleFoo :: Module () () ()
moduleFoo =
  Module.fromDefinitionList
    (Path ["Foo"])
    -- Exports
    []
    -- Definitions
    [ DType
        "Pair"
        [ Parameter () "a", Parameter () "b" ]
        [ Constructor "Pair" 2 (Forall mempty [] (TIntrinsic (ITuple []))) ]
    , DTrait
        "Show"
        []
        (TVariable (Parameter () "a"))
        [
          ( "show"
          , TVariable (Parameter () "a") `TArrow` TIntrinsic IString
          )
        ]
    , -- instance Show(int32)
      DInstance
        "Show"
        (TIntrinsic IInt32)
        [ DFunction
            "show"
            ( Function
                ()
                (With [] ())
                (PVariable () (Label () "_") :| [])
                (ELiteral () (LString "TODO"))
            )
        ]
    , -- instance Show(Pair(a, b)) with Show(a), Show(b)
      DInstance
        "Show"
        undefined
        undefined
    , -- instance Show(list(a)) with Show(a) 
      DInstance
        "Show"
        undefined
        undefined
    , DConstant
        "foo"
        (
          Constant
            undefined
            undefined
            (
              ELet
                undefined
                undefined
                undefined
            )
        )
    , DFunction
        "baz"
        (
          Function
            undefined
            undefined
            undefined
            (
              ELet
                undefined
                undefined
                undefined
            )
        )
    ]

