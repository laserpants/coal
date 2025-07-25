{-# LANGUAGE OverloadedStrings #-}

module Noll.Set10.Test03 where

import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Common.Label (Label (..))
import Noll.Language
import Noll.Language.Module (Definition (..), Function (..), Module (..), Path (..))

import qualified Noll.Language.Module as Module

prog10_01 :: [Module () () ()]
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
                                      (EVariable () (Label () "n") :| [])
                                  )
                                  :| []
                              )
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
                ( Just
                    ( ERecursiveLet
                        ()
                        (PVariable () (Label () "$fold.1"))
                        ( ELambda
                            ()
                            (PVariable () (Label () "$fold.1.expr") :| [])
                            ( EMatch
                                ()
                                ()
                                (EVariable () (Label () "$fold.1.expr"))
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
                                              [ PVariable () (Label () "f")
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
                                                      (EVariable () (Label () "n") :| [])
                                                  )
                                                  :| []
                                              )
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
                                                      <| EApplication
                                                        ()
                                                        ()
                                                        (EVariable () (Label () "$fold.1"))
                                                        ( EVariable () (Label () "f")
                                                            :| []
                                                        )
                                                      :| []
                                                  )
                                              )
                                          )
                                          :| []
                                      )
                                    :| []
                                )
                            )
                        )
                        ( EApplication
                            ()
                            ()
                            (EVariable () (Label () "$fold.1"))
                            ( EApplication
                                ()
                                ()
                                (EVariable () (Label () "pack_nat"))
                                ( EVariable () (Label () "n")
                                    :| []
                                )
                                :| []
                            )
                        )
                    )
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
                        ( ELiteral () (LInt32 5)
                            :| []
                        )
                        :| []
                    )
                    :| []
                )
            )
        )
    ]
