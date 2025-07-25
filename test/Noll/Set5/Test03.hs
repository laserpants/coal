{-# LANGUAGE OverloadedStrings #-}

module Noll.Set5.Test03 where

import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Common.Label (Label (..))
import Noll.Language
import Noll.Language.Module (Constant (..), Definition (..), Function (..), Module (..), Path (..))

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Noll.Language.Module as Module

-- Expand folds
prog1_03 :: [Module () k ()]
prog1_03 =
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
moduleMain :: Module () k ()
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
                                    (Label () "Zero")
                                    []
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
                                                      (Label () "Succ")
                                                      [ PVariable () (Label () "f")
                                                      ]
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
                                                              (EVariable () (Label () "$fold.1"))
                                                              ( EVariable () (Label () "f")
                                                                  <| EApplication
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
                                        )
                                    )
                                    ( EApplication
                                        ()
                                        ()
                                        (EVariable () (Label () "$fold.1"))
                                        (EVariable () (Label () "n") :| [])
                                    )
                                )
                            )
                        )
                    )
                    :| []
                )
                ( EApplication
                    ()
                    ()
                    (EVariable () (Label () "trace_string"))
                    ( EApplication
                        ()
                        ()
                        (EVariable () (Label () "f"))
                        ( EApplication
                            ()
                            ()
                            (EVariable () (Label () "from_int32"))
                            ( ELiteral () (LInt32 5)
                                :| []
                            )
                            <| ELiteral () (LString "")
                            :| []
                        )
                        :| []
                    )
                )
            )
        )
        --        ( Function
        --            ()
        --            (With [] ())
        --            (PLiteral () LUnit :| [])
        --            ( ELet
        --                ()
        --                ( BPattern
        --                    ()
        --                    (PVariable () (Label () "f"))
        --                    ( ELambda
        --                        ()
        --                        (PVariable () (Label () "n") :| [])
        --                         ( EFold
        --                             ()
        --                             ()
        --                             (EVariable () (Label () "n") :| [])
        --                             ( EClause
        --                                 ()
        --                                 ( PConstructor
        --                                     ()
        --                                     (Label () "Succ")
        --                                     [ PAtVariable () (Label () "f")
        --                                     ]
        --                                 )
        --                                 ( CPlain
        --                                     ()
        --                                     []
        --                                     ( ELambda
        --                                         ()
        --                                         (PVariable () (Label () "s") :| [])
        --                                         ( EVariable () (Label () "s")
        --                                         )
        --                                     )
        --                                     :| []
        --                                 )
        --                                 <| EClause
        --                                   ()
        --                                   ( PConstructor
        --                                       ()
        --                                       (Label () "Zero")
        --                                       []
        --                                   )
        --                                   ( CPlain
        --                                       ()
        --                                       []
        --                                       ( ELambda
        --                                           ()
        --                                           (PVariable () (Label () "s") :| [])
        --                                           ( EApplication
        --                                               ()
        --                                               ()
        --                                               (EVariable () (Label () "f"))
        --                                               ( EApplication
        --                                                   ()
        --                                                   ()
        --                                                   (EBinaryOperator () () OStringConcatenation)
        --                                                   ( ELiteral () (LString "a")
        --                                                       <| EVariable () (Label () "s")
        --                                                       :| []
        --                                                   )
        --                                                   :| []
        --                                               )
        --                                           )
        --                                       )
        --                                       :| []
        --                                   )
        --                                 :| []
        --                             )
        --                             ( Just
        --                                 ( ERecursiveLet
        --                                     ()
        --                                     (PVariable () (Label () "$fold.1"))
        --                                     ( ELambda
        --                                         ()
        --                                         (PVariable () (Label () "$fold.1.expr") :| [])
        --                                         ( EMatch
        --                                             ()
        --                                             ()
        --                                             (EVariable () (Label () "$fold.1.expr"))
        --                                             ( EClause
        --                                                 ()
        --                                                 ( PConstructor
        --                                                     ()
        --                                                     (Label () "Succ")
        --                                                     [ PVariable () (Label () "f")
        --                                                     ]
        --                                                 )
        --                                                 ( CPlain
        --                                                     ()
        --                                                     []
        --                                                     ( ELambda
        --                                                         ()
        --                                                         (PVariable () (Label () "s") :| [])
        --                                                         ( EVariable () (Label () "s")
        --                                                         )
        --                                                     )
        --                                                     :| []
        --                                                 )
        --                                                 <| EClause
        --                                                   ()
        --                                                   ( PConstructor
        --                                                       ()
        --                                                       (Label () "Zero")
        --                                                       []
        --                                                   )
        --                                                   ( CPlain
        --                                                       ()
        --                                                       []
        --                                                       ( ELambda
        --                                                           ()
        --                                                           (PVariable () (Label () "s") :| [])
        --                                                           ( EApplication
        --                                                               ()
        --                                                               ()
        --                                                               (EVariable () (Label () "f"))
        --                                                               ( EApplication
        --                                                                   ()
        --                                                                   ()
        --                                                                   (EBinaryOperator () () OStringConcatenation)
        --                                                                   ( ELiteral () (LString "a")
        --                                                                       <| EVariable () (Label () "s")
        --                                                                       :| []
        --                                                                   )
        --                                                                   :| []
        --                                                               )
        --                                                           )
        --                                                       )
        --                                                       :| []
        --                                                   )
        --                                                 :| []
        --                                             )
        --                                         )
        --                                     )
        --                                     ( EApplication
        --                                         ()
        --                                         ()
        --                                         (EVariable () (Label () "$fold.1"))
        --                                         (EVariable () (Label () "n") :| [])
        --                                     )
        --                                 )
        --                             )
        --                         )
        --                    )
        --                    :| []
        --                )
        --                ( EApplication
        --                    ()
        --                    ()
        --                    (EVariable () (Label () "f"))
        --                    ( ELiteral () (LInt32 5)
        --                        <| ELiteral () (LString "")
        --                        :| []
        --                    )
        --                )
        --            )
        --        )
    ]
