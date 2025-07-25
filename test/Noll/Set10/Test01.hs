{-# LANGUAGE OverloadedStrings #-}

module Noll.Set10.Test01 where

import Noll.Common.List1 (NonEmpty (..), (<|))
import Noll.Common.Label (Label (..))
import Noll.Language
import Noll.Language.Module (Definition (..), Function (..), Module (..), Path (..))

import qualified Noll.Language.Module as Module

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
                          ( ELet
                              ()
                              ( BPattern
                                  ()
                                  (PVariable () (Label () "aa"))
                                  ( EApplication
                                      ()
                                      ()
                                      (EVariable () (Label () "trace_int32"))
                                      ( EApplication
                                          ()
                                          ()
                                          (EVariable () (Label () "unpack_nat"))
                                          ( EVariable () (Label () "m")
                                              :| []
                                          )
                                          :| []
                                      )
                                  )
                                  :| []
                              )
                              -- (ELiteral () (LInt32 401))
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
                          )
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
                        ( ELiteral () (LInt32 12)
                            :| []
                        )
                        :| []
                    )
                    :| []
                )
            )
        )
    ]
