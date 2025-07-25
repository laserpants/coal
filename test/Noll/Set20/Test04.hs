{-# LANGUAGE OverloadedStrings #-}

module Noll.Set20.Test04 where

import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Common.Label (Label (..))
import Noll.Language
import Noll.Language.Module (Definition (..), Function (..), Module (..), Path (..))

import qualified Noll.Language.Module as Module

prog10_04 :: [Module () Kind IndexedType]
prog10_04 =
  [ moduleUtilities
  , moduleMain
  ]

moduleUtilities :: Module () Kind IndexedType
moduleUtilities =
  Module.fromDefinitionList
    (Path ["Utilities"])
    -- Exports
    ["factorial"]
    -- Definitions
    [ DImport (Path ["Core$"]) ["pack_nat", "unpack_nat", "trace_int32", "from_int32"]
    , DAnnotation
        (With [] (TIntrinsic IInt32))
        ( DFunction
            "factorial"
            ( Function
                ()
                ( With
                    []
                    (TIntrinsic IInt32)
                )
                ( PAnnotation
                    ()
                    (TIntrinsic IInt32)
                    (PVariable () (Label (TIntrinsic IInt32) "n"))
                    :| []
                )
                ( EFold
                    ()
                    (TIntrinsic IInt32)
                    ( EApplication
                        ()
                        (TIntrinsic INat)
                        (EVariable () (Label (TIntrinsic IInt32 `TArrow` TIntrinsic INat) "pack_nat"))
                        ( EVariable () (Label (TIntrinsic IInt32) "n")
                            :| []
                        )
                        :| []
                    )
                    ( EClause
                        ()
                        ( PConstructor
                            ()
                            (Label (TIntrinsic INat) "Zero")
                            []
                        )
                        ( CPlain
                            ()
                            []
                            ( EAnnotation
                                ()
                                (TIntrinsic IInt32)
                                ( EApplication
                                    ()
                                    (TIntrinsic IInt32)
                                    (EVariable () (Label (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) "from_int32"))
                                    (ELiteral () (LInt32 1) :| [])
                                )
                            )
                            :| []
                        )
                        <| EClause
                          ()
                          ( PAs
                              ()
                              (Label (TIntrinsic INat) "m")
                              ( PConstructor
                                  ()
                                  (Label (TIntrinsic INat) "Succ")
                                  [ PAtVariable () (Label (TIntrinsic INat) "f")
                                  ]
                              )
                          )
                          ( CPlain
                              ()
                              []
                              ( EApplication
                                  ()
                                  (TIntrinsic IInt32)
                                  ( EBinaryOperator
                                      ()
                                      (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IInt32)
                                      OMultiplication
                                  )
                                  ( EApplication
                                      ()
                                      (TIntrinsic IInt32)
                                      (EVariable () (Label (TIntrinsic INat `TArrow` TIntrinsic IInt32) "unpack_nat"))
                                      ( EVariable () (Label (TIntrinsic INat) "m")
                                          :| []
                                      )
                                      <| EVariable () (Label (TIntrinsic IInt32) "f")
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

moduleMain :: Module () Kind IndexedType
moduleMain =
  undefined

--  Module.fromDefinitionList
--    (Path ["Main"])
--    -- Exports
--    []
--    -- Definitions
--    [ DImport (Path ["Core$"]) ["trace_int32"]
--    , DImport (Path ["Utilities"]) ["factorial"]
--    , DTrait
--        "Numeric"
--        []
--        (TVariable (Parameter () "a"))
--        [
--          ( "from_int32"
--          , TIntrinsic IInt32 `TArrow` TVariable (Parameter () "a")
--          )
--        ]
--    , DInstance
--        "Numeric"
--        (TIntrinsic INat)
--        [ DFunction
--            "from_int32"
--            ( Function
--                ()
--                (With [] ())
--                (PVariable () (Label undefined "n") :| [])
--                ( EApplication
--                    ()
--                    ()
--                    (EVariable () (Label undefined "Core$.pack_nat"))
--                    (EVariable () (Label undefined "n") :| [])
--                )
--            )
--        ]
--    , DInstance
--        "Numeric"
--        (TIntrinsic IInt32)
--        [ DFunction
--            "from_int32"
--            ( Function
--                ()
--                (With [] ())
--                (PVariable () (Label undefined "n") :| [])
--                (EVariable () (Label undefined "n"))
--            )
--        ]
--    , DFunction
--        "main"
--        ( Function
--            ()
--            (With [] ())
--            (PLiteral () LUnit :| [])
--            ( EApplication
--                ()
--                ()
--                (EVariable () (Label undefined "trace_int32"))
--                ( EApplication
--                    ()
--                    ()
--                    (EVariable () (Label undefined "factorial"))
--                    ( EApplication
--                        ()
--                        ()
--                        (EVariable () (Label undefined "from_int32"))
--                        (ELiteral () (LInt32 4) :| [])
--                        :| []
--                    )
--                    :| []
--                )
--            )
--        )
--    ]
