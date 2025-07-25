{-# LANGUAGE OverloadedStrings #-}

module Noll.Set2.Test01 where

import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Common.Label (Label (..))
import Noll.Language
import Noll.Language.Type.Intrinsic
import Noll.Language.Module

import qualified Noll.Language.Module as Module

-- Untyped source tree
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
    []

--    DType
--        "Pair"
--        [Parameter () "a", Parameter () "b"]
--        [Constructor "Pair" 2 (Forall mempty [] (TIntrinsic (ITuple [])))]
--    , DTrait
--        "Show"
--        []
--        (TVariable (Parameter () "a"))
--        [
--          ( "show"
--          , TVariable (Parameter () "a") `TArrow` TIntrinsic IString
--          )
--        ]
--    , -- instance Show(int32)
--      DInstance
--        "Show"
--        (TIntrinsic IInt32)
--        [ DFunction
--            "show"
--            ( Function
--                ()
--                (With [] ())
--                (PVariable () (Label () "n") :| [])
--                (ELiteral () (LString "TODO"))
--            )
--        ]
--    , -- instance Show(Pair(a, b)) with Show(a), Show(b)
--      DInstance
--        "Show"
--        ( TApplication
--            ()
--            (TConstructor () "Pair")
--            ( TVariable (Parameter () "a")
--                <| TVariable (Parameter () "b")
--                :| []
--            )
--        )
--        [ DFunction
--            "show"
--            ( Function
--                ()
--                (With [] ())
--                (PVariable () (Label () "p") :| [])
--                ( EMatch
--                    ()
--                    ()
--                    (EVariable () (Label () "p"))
--                    ( EClause
--                        ()
--                        ( PConstructor
--                            ()
--                            (Label () "Pair")
--                            [ PVariable () (Label () "x")
--                            , PVariable () (Label () "y")
--                            ]
--                        )
--                        ( CPlain
--                            ()
--                            []
--                            ( EApplication
--                                ()
--                                ()
--                                ( EBinaryOperator
--                                    ()
--                                    ()
--                                    OStringConcatenation
--                                )
--                                ( EApplication
--                                    ()
--                                    ()
--                                    (EVariable () (Label () "show"))
--                                    (EVariable () (Label () "x") :| [])
--                                    <| EApplication
--                                      ()
--                                      ()
--                                      (EVariable () (Label () "show"))
--                                      (EVariable () (Label () "y") :| [])
--                                    :| []
--                                )
--                            )
--                            :| []
--                        )
--                        :| []
--                    )
--                )
--            )
--        ]
--    , -- instance Show(list(a)) with Show(a)
--      DInstance
--        "Show"
--        (TIntrinsic (IList (TVariable (Parameter () "a"))))
--        [ DFunction
--            "show"
--            ( Function
--                ()
--                (With [] ())
--                (PVariable () (Label () "lst") :| [])
--                ( EMatch
--                    ()
--                    ()
--                    (EVariable () (Label () "lst"))
--                    ( EClause
--                        ()
--                        ( PListCons
--                            ()
--                            ()
--                            (PVariable () (Label () "x"))
--                            (PAny () ())
--                        )
--                        ( CPlain
--                            ()
--                            []
--                            ( EApplication
--                                ()
--                                ()
--                                (EVariable () (Label () "show"))
--                                (EVariable () (Label () "x") :| [])
--                            )
--                            :| []
--                        )
--                        :| []
--                    )
--                )
--            )
--        ]
--    , DConstant
--        "foo"
--        ( Constant
--            ()
--            (With [] ())
--            ( ELet
--                ()
--                ( BPattern
--                    ()
--                    (PVariable () (Label () "p"))
--                    ( ETuple
--                        ()
--                        ()
--                        ( ELiteral () (LInt32 1)
--                            <| ELiteral () (LString "hello")
--                            :| []
--                        )
--                    )
--                    :| []
--                )
--                ( EApplication
--                    ()
--                    ()
--                    (EVariable () (Label () "show"))
--                    (EVariable () (Label () "p") :| [])
--                )
--            )
--        )
--    , DFunction
--        "baz"
--        ( Function
--            ()
--            (With [] ())
--            ( PVariable () (Label () "x")
--                <| PVariable () (Label () "y")
--                :| []
--            )
--            ( ELet
--                ()
--                ( BPattern
--                    ()
--                    (PVariable () (Label () "p"))
--                    ( ETuple
--                        ()
--                        ()
--                        ( EVariable () (Label () "x")
--                            <| EVariable () (Label () "y")
--                            :| []
--                        )
--                    )
--                    :| []
--                )
--                ( EApplication
--                    ()
--                    ()
--                    (EVariable () (Label () "show"))
--                    (EVariable () (Label () "p") :| [])
--                )
--            )
--        )
