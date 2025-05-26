{-# LANGUAGE OverloadedStrings #-}

module Noll.Set2.Test10 where

import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Noll.Language
import Noll.Language.Type.Intrinsic
import Noll.Module

import qualified Data.Map.Strict as Map
import qualified Noll.Module as Module

-- Dictionary insertion?
prog2_10 :: [Module () Kind IndexedType]
prog2_10 =
  [ moduleFoo
  ]

pairType :: IndexedType
pairType = TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)])

showType t =
  TApplication
    KTrait
    (TConstructor (KType `KArrow` KTrait) "Show")
    (t :| [])

moduleFoo :: Module () Kind IndexedType
moduleFoo =
  Module.fromDefinitionList
    (Path ["Foo"])
    -- Exports
    []
    -- Definitions
    [ DTrait
        "Show"
        []
        (TVariable (Parameter () "a"))
        [
          ( "show"
          , TVariable (Parameter () "a") `TArrow` TIntrinsic IString
          )
        ]
    , -- instance Show(string)
      DInstance2
        "Show"
        (TIntrinsic IString)
        [ DConstant
            "show"
            ( Constant
                ()
                (With [] (TIntrinsic IString `TArrow` TIntrinsic IString))
                ( ELambda
                    ()
                    (PVariable () (Label (TIntrinsic IString) "s") :| [])
                    (EVariable () (Label (TIntrinsic IString) "s"))
                )
            )
        ]
--    , -- instance Show(int32)
--      DInstance2
--        "Show"
--        (TIntrinsic IInt32)
--        [ DConstant
--            "show"
--            ( Constant
--                ()
--                (With [] (TIntrinsic IInt32 `TArrow` TIntrinsic IString))
--                ( ELambda
--                    ()
--                    (PVariable () (Label (TIntrinsic IInt32) "n") :| [])
--                    (ELiteral () (LString "TODO"))
--                )
--            )
--        ]
--    , -- instance Show((a, b)) with Show(a), Show(b)
--      DInstance2
--        "Show"
--        (TIntrinsic (ITuple [TVariable (Parameter KType "a"), TVariable (Parameter KType "b")]))
--        [ DConstant
--            "show"
--            ( Constant
--                ()
--                ( With
--                    [ Trait "Show" (TVariable (TypeIndex KType 0))
--                    , Trait "Show" (TVariable (TypeIndex KType 1))
--                    ]
--                    (pairType `TArrow` TIntrinsic IString)
--                )
--                ( ELambda
--                    ()
--                    ( PDictionary () (showType (TVariable (TypeIndex KType 0))) (Trait "Show" (TVariable (TypeIndex KType 0)))
--                        <| PDictionary () (showType (TVariable (TypeIndex KType 1))) (Trait "Show" (TVariable (TypeIndex KType 1)))
--                        :| []
--                    )
--                    ( ELambda
--                        ()
--                        (PVariable () (Label pairType "p") :| [])
--                        ( ECompiledMatch
--                            ()
--                            (TIntrinsic IString)
--                            (EVariable () (Label pairType "p"))
--                            ( ECompiledClause
--                                ( Label (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 1) `TArrow` pairType) "$Tuple2"
--                                    <| Label (TVariable (TypeIndex KType 0)) "$match.1.x"
--                                    <| Label (TVariable (TypeIndex KType 1)) "$match.2.y"
--                                    :| []
--                                )
--                                ( EApplication
--                                    ()
--                                    (TIntrinsic IString)
--                                    ( EBinaryOperator
--                                        ()
--                                        (TIntrinsic IString `TArrow` TIntrinsic IString `TArrow` TIntrinsic IString)
--                                        OStringConcatenation
--                                    )
--                                    ( EApplication
--                                        ()
--                                        (TIntrinsic IString)
--                                        ( EApplication
--                                            ()
--                                            (TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IString)
--                                            (EVariable () (Label (showType (TVariable (TypeIndex KType 0)) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IString) "show"))
--                                            (EDictionary () (showType (TVariable (TypeIndex KType 0))) (Trait "Show" (TVariable (TypeIndex KType 0))) :| [])
--                                        )
--                                        (EVariable () (Label (TVariable (TypeIndex KType 0)) "$match.1.x") :| [])
--                                        <| EApplication
--                                          ()
--                                          (TIntrinsic IString)
--                                          ( EApplication
--                                              ()
--                                              (TVariable (TypeIndex KType 1) `TArrow` TIntrinsic IString)
--                                              (EVariable () (Label (showType (TVariable (TypeIndex KType 1)) `TArrow` TVariable (TypeIndex KType 1) `TArrow` TIntrinsic IString) "show"))
--                                              (EDictionary () (showType (TVariable (TypeIndex KType 1))) (Trait "Show" (TVariable (TypeIndex KType 1))) :| [])
--                                          )
--                                          (EVariable () (Label (TVariable (TypeIndex KType 1)) "$match.2.y") :| [])
--                                        :| []
--                                    )
--                                )
--                                :| []
--                            )
--                        )
--                    )
--                )
--            )
--        ]
--    , -- instance Show(list(a)) with Show(a)
--      DInstance2
--        "Show"
--        (TIntrinsic (IList (TVariable (Parameter KType "a"))))
--        [ DConstant
--            "show"
--            ( Constant
--                ()
--                (With [Trait "Show" (TVariable (TypeIndex KType 0))] (TIntrinsic (IList (TVariable (TypeIndex KType 0))) `TArrow` TIntrinsic IString))
--                ( ELambda
--                    ()
--                    (PDictionary () (showType (TVariable (TypeIndex KType 0))) (Trait "Show" (TVariable (TypeIndex KType 0))) :| [])
--                    ( ELambda
--                        ()
--                        (PVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 0)))) "lst") :| [])
--                        ( ECompiledMatch
--                            ()
--                            (TIntrinsic IString)
--                            (EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 0)))) "lst"))
--                            ( ECompiledClause
--                                ( Label (TVariable (TypeIndex KType 0) `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 0))) `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 0)))) "$Cons"
--                                    <| Label (TVariable (TypeIndex KType 0)) "$match.4.x"
--                                    <| Label (TIntrinsic (IList (TVariable (TypeIndex KType 0)))) "$match.5._"
--                                    :| []
--                                )
--                                ( EApplication
--                                    ()
--                                    (TIntrinsic IString)
--                                    ( EApplication
--                                        ()
--                                        (TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IString)
--                                        (EVariable () (Label (showType (TVariable (TypeIndex KType 0)) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IString) "show"))
--                                        (EDictionary () (showType (TVariable (TypeIndex KType 0))) (Trait "Show" (TVariable (TypeIndex KType 0))) :| [])
--                                    )
--                                    (EVariable () (Label (TVariable (TypeIndex KType 0)) "$match.4.x") :| [])
--                                )
--                                :| []
--                            )
--                        )
--                    )
--                )
--            )
--        ]
--    , DConstant
--        "foo"
--        ( Constant
--            ()
--            (With [] (TIntrinsic IString))
--            ( ELet
--                ()
--                ( BPattern
--                    ()
--                    (PVariable () (Label (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString])) "p"))
--                    ( ETuple
--                        ()
--                        (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]))
--                        ( ELiteral () (LInt32 1)
--                            <| ELiteral () (LString "hello")
--                            :| []
--                        )
--                    )
--                    :| []
--                )
--                ( EApplication
--                    ()
--                    (TIntrinsic IString)
--                    ( EApplication
--                        ()
--                        (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]) `TArrow` TIntrinsic IString)
--                        (EVariable () (Label (showType (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString])) `TArrow` showType (TIntrinsic IInt32) `TArrow` showType (TIntrinsic IString) `TArrow` TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]) `TArrow` TIntrinsic IString) "show"))
--                        ( EDictionary () (showType (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]))) (Trait "Show" (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString])))
--                            <| EDictionary () (showType (TIntrinsic IInt32)) (Trait "Show" (TIntrinsic IInt32))
--                            <| EDictionary () (showType (TIntrinsic IString)) (Trait "Show" (TIntrinsic IString))
--                            :| []
--                        )
--                    )
--                    (EVariable () (Label (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString])) "p") :| [])
--                )
--            )
--        )
--    , DConstant
--        "baz"
--        ( Constant
--            ()
--            (With [] (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 1) `TArrow` TIntrinsic IString))
--            ( ELambda
--                ()
--                ( PVariable () (Label (TVariable (TypeIndex KType 0)) "x")
--                    <| PVariable () (Label (TVariable (TypeIndex KType 1)) "y")
--                    :| []
--                )
--                ( ELet
--                    ()
--                    ( BPattern
--                        ()
--                        (PVariable () (Label pairType "p"))
--                        ( ETuple
--                            ()
--                            pairType
--                            ( EVariable () (Label (TVariable (TypeIndex KType 0)) "x")
--                                <| EVariable () (Label (TVariable (TypeIndex KType 1)) "y")
--                                :| []
--                            )
--                        )
--                        :| []
--                    )
--                    ( EApplication
--                        ()
--                        (TIntrinsic IString)
--                        ( EApplication
--                            ()
--                            (pairType `TArrow` TIntrinsic IString)
--                            (EVariable () (Label (showType pairType `TArrow` showType (TVariable (TypeIndex KType 0)) `TArrow` showType (TVariable (TypeIndex KType 1)) `TArrow` pairType `TArrow` TIntrinsic IString) "show"))
--                            ( EDictionary () (showType pairType) (Trait "Show" pairType)
--                                <| EDictionary () (showType (TVariable (TypeIndex KType 0))) (Trait "Show" (TVariable (TypeIndex KType 0)))
--                                <| EDictionary () (showType (TVariable (TypeIndex KType 1))) (Trait "Show" (TVariable (TypeIndex KType 1)))
--                                :| []
--                            )
--                        )
--                        (EVariable () (Label pairType "p") :| [])
--                    )
--                )
--            )
--        )
--    , DConstant
--        "bar"
--        ( Constant
--            ()
--            (With [] (TIntrinsic (IList pairType) `TArrow` TIntrinsic IString))
--            ( ELambda
--                ()
--                (PVariable () (Label (TIntrinsic (IList pairType)) "xs") :| [])
--                ( EApplication
--                    ()
--                    (TIntrinsic IString)
--                    ( EApplication
--                        ()
--                        (TIntrinsic (IList pairType) `TArrow` TIntrinsic IString)
--                        (EVariable () (Label (showType (TIntrinsic (IList pairType)) `TArrow` TIntrinsic (IList pairType) `TArrow` TIntrinsic IString) "show"))
--                        ( EDictionary () (showType (TIntrinsic (IList pairType))) (Trait "Show" (TIntrinsic (IList pairType)))
--                            <| ERecord 
--                              ()
--                              ( TIntrinsic (IRecord (TRow (RExtend "show" (pairType `TArrow` TIntrinsic IString) RNil)))
--                              )
--                              ( Map.fromList
--                                  [
--                                    ( "show"
--                                    , EApplication
--                                        ()
--                                        (pairType `TArrow` TIntrinsic IString)
--                                        (EVariable () (Label (showType pairType `TArrow` showType (TVariable (TypeIndex KType 0)) `TArrow` showType (TVariable (TypeIndex KType 1)) `TArrow` pairType `TArrow` TIntrinsic IString) "show"))
--                                        ( EDictionary () (showType pairType) (Trait "Show" pairType)
--                                            <| EDictionary () (showType (TVariable (TypeIndex KType 0))) (Trait "Show" (TVariable (TypeIndex KType 0)))
--                                            <| EDictionary () (showType (TVariable (TypeIndex KType 1))) (Trait "Show" (TVariable (TypeIndex KType 1)))
--                                            :| []
--                                        )
--                                    )
--                                  ]
--                              )
--                              Nothing
--                            :| []
--                        )
--                    )
--                    (EVariable () (Label (TIntrinsic (IList pairType)) "xs") :| [])
--                )
--            )
--        )
    ]
