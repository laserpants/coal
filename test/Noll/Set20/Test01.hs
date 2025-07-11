{-# LANGUAGE OverloadedStrings #-}

module Noll.Set20.Test01 where

import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Noll.Language
import Noll.Module (Definition (..), Function (..), Module (..), Path (..))

import qualified Noll.Module as Module

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
    ["factorial"]
    -- Definitions
    [ DImport (Path ["Core$"]) ["pack_nat", "unpack_nat", "trace_int32"]
    , DAnnotation
        (With [] (TIntrinsic IInt32))
        ( DFunction
            "factorial"
            ( Function
                ()
                (With [] ())
                ( PAnnotation
                    ()
                    (TIntrinsic IInt32)
                    (PVariable () (Label () "n"))
                    :| []
                )
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
                            (ELiteral () (LInt32 1))
                            :| []
                        )
                        <| EClause
                          ()
                          ( PAs
                              ()
                              (Label () "m")
                              ( PConstructor
                                  ()
                                  (Label () "Succ")
                                  [ PAtVariable () (Label () "f")
                                  ]
                              )
                          )
                          ( CPlain
                              ()
                              []
                              ( EApplication
                                  ()
                                  ()
                                  (EBinaryOperator () () OMultiplication)
                                  ( EApplication
                                      ()
                                      ()
                                      (EVariable () (Label () "unpack_nat"))
                                      ( EVariable () (Label () "m")
                                          :| []
                                      )
                                      <| EVariable () (Label () "f")
                                      :| []
                                  )
                              )
                              :| []
                          )
                        :| []
                    )
                    Nothing
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
    , DImport (Path ["Utilities"]) ["factorial"]
    , DTrait
        "Numeric"
        []
        (TVariable (Parameter () "a"))
        [
          ( "from_int32"
          , TIntrinsic IInt32 `TArrow` TVariable (Parameter () "a")
          )
        ]
    , DInstance
        "Numeric"
        (TIntrinsic INat)
        [ DFunction
            "from_int32"
            ( Function
                ()
                (With [] ())
                (PVariable () (Label () "n") :| [])
                ( EApplication
                    ()
                    ()
                    (EVariable () (Label () "Core$.pack_nat"))
                    (EVariable () (Label () "n") :| [])
                )
            )
        ]
    , DInstance
        "Numeric"
        (TIntrinsic IInt32)
        [ DFunction
            "from_int32"
            ( Function
                ()
                (With [] ())
                (PVariable () (Label () "n") :| [])
                (EVariable () (Label () "n"))
            )
        ]
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
                        (EVariable () (Label () "factorial"))
                        ( EApplication
                            ()
                            ()
                            (EVariable () (Label () "from_int32"))
                            (ELiteral () (LInt32 12) :| [])
                            :| []
                        )
                        :| []
                    )
            )
        )
    ]
