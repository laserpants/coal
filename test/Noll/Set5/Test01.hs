{-# LANGUAGE OverloadedStrings #-}

module Noll.Set5.Test01 where

import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Noll.Language
import Noll.Module (Constant (..), Definition (..), Function (..), Module (..), Path (..))

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Noll.Module as Module

-- Untyped source tree
prog1_01 :: [Module () () ()]
prog1_01 =
  [ moduleMain
  ]

--
-- let
--   f =
--     fn(n) =>
--       fold(n) {
--         | Zero =>
--             fn(s) => s
--         | Succ(@f) =>
--             fn(s) => f("a" +++ s)
--       }
--   in
--     f(5, "")
--
moduleMain :: Module () () ()
moduleMain =
  Module.fromDefinitionList
    (Path ["Main"])
    -- Exports
    []
    -- Definitions
    [ DFunction
        "main"
        ( Function
            ()
            (With [] ())
            (PLiteral () LUnit :| [])
            ( ELet
                ()
                ( BPattern
                    ()
                    (PVariable () (Label () "f"))
                    ( ELambda
                        ()
                        (PVariable () (Label () "n") :| [])
                        ( EFold
                            ()
                            ()
                            (EVariable () (Label () "n") :| [])
                            ( EClause
                                ()
                                ( PConstructor
                                    ()
                                    (Label () "Succ")
                                    [ PAtVariable () (Label () "f")
                                    ]
                                )
                                ( CPlain
                                    ()
                                    []
                                    ( ELambda
                                        ()
                                        (PVariable () (Label () "s") :| [])
                                        ( EVariable () (Label () "s")
                                        )
                                    )
                                    :| []
                                )
                                <| EClause
                                  ()
                                  ( PConstructor
                                      ()
                                      (Label () "Zero")
                                      []
                                  )
                                  ( CPlain
                                      ()
                                      []
                                      ( ELambda
                                          ()
                                          (PVariable () (Label () "s") :| [])
                                          ( EApplication
                                              ()
                                              ()
                                              (EVariable () (Label () "f"))
                                              ( EApplication
                                                  ()
                                                  ()
                                                  (EBinaryOperator () () OStringConcatenation)
                                                  ( ELiteral () (LString "a")
                                                      <| EVariable () (Label () "s")
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
                            Nothing
                        )
                    )
                    :| []
                )
                ( EApplication
                    ()
                    ()
                    (EVariable () (Label () "f"))
                    ( ELiteral () (LInt32 5)
                        <| ELiteral () (LString "")
                        :| []
                    )
                )
            )
        )
    ]
