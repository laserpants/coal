{-# LANGUAGE OverloadedStrings #-}

module Noll.Set20.Test01 where

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
    ["factorial"]
    -- Definitions
    [ DImport (Path ["Core$"]) ["pack_nat", "unpack_nat", "trace_int32", "from_int32", "from_int32__$instance_Numeric(Intrinsic(Nat))", "from_int32__$instance_Numeric(Intrinsic(Int32))"]
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
                            --                            ( EAnnotation
                            --                                ()
                            --                                (TIntrinsic IInt32)
                            ( EApplication
                                ()
                                ()
                                (EVariable () (Label () "from_int32"))
                                (ELiteral () (LInt32 1) :| [])
                            )
                            --                            )
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
    ["*"]
    -- Definitions
    [ DImport (Path ["Core$"]) ["trace_int32", "from_int32", "from_int32__$instance_Numeric(Intrinsic(Int32))"]
    , DImport (Path ["Utilities"]) ["factorial"]
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
                        (ELiteral () (LInt32 4) :| [])
                        :| []
                    )
                    :| []
                )
            )
        )
    ]
