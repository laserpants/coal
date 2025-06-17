{-# LANGUAGE OverloadedStrings #-}

module Noll.Set4.Test04 where

import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Noll.Language
import Noll.Module (Constant (..), Definition (..), Function (..), Module (..), Path (..))

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Noll.Module as Module

prog4_04 :: [Module () Kind IndexedType]
prog4_04 =
  [ moduleMain
  ]

-- Add types
moduleMain :: Module () Kind IndexedType
moduleMain =
  Module.fromDefinitionList
    (Path ["Main"])
    []
    [ DImport (Path ["Core$"]) ["trace_int32"]
    , DCodata
        "Stream"
        [Parameter () "a"]
        [
          ( "Head"
          , TVariable (Parameter () "a")
          )
        ,
          ( "Tail"
          , TApplication () (TConstructor () "Stream") (TVariable (Parameter () "a") :| [])
          )
        ]
    , DConstant
        "nats"
        ( Constant
            ()
            (With [] (TIntrinsic IInt32 
                            `TArrow` 
                                          TIntrinsic ( IRecord
                                              ( TRow
                                                  ( RExtend
                                                      "$$Head"
                                                      (TVariable (TypeIndex KType 2) `TArrow` TIntrinsic IInt32)
                                                      (RExtend "$$Tail" (TVariable (TypeIndex KType 2) `TArrow` TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TIntrinsic IInt32 :| [])) RNil)
                                                  )
                                              )
                                          )

            ))
--            ( EApplication
--                ()
--                (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TIntrinsic IInt32 :| []))
--                ( EUnfold
--                    ()
--                    undefined
--                    (Label undefined "Stream")
--                    "f"
--                    ( PAnnotation
--                        ()
--                        (TIntrinsic IInt32)
--                        (PVariable () (Label (TIntrinsic IInt32) "n"))
--                        :| []
--                    )
--                    ( Map.fromList
--                        [
--                          ( "Head"
--                          , EVariable () (Label (TIntrinsic IInt32) "n")
--                          )
--                        ,
--                          ( "Tail"
--                          , EApplication
--                              ()
--                              (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TIntrinsic IInt32 :| []))
--                              (EVariable () (Label (TIntrinsic IInt32 `TArrow` TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TIntrinsic IInt32 :| [])) "f"))
--                              ( EApplication
--                                  ()
--                                  (TIntrinsic IInt32)
--                                  (EBinaryOperator () (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) OAddition)
--                                  ( EVariable () (Label (TIntrinsic IInt32) "n")
--                                      <| ELiteral () (LInt32 1)
--                                      :| []
--                                  )
--                                  :| []
--                              )
--                          )
--                        ]
--                    )
--                    ( Just
                        ( ERecursiveLet
                            ()
                            (PVariable () (Label (
                                  TIntrinsic IInt32 
                                    `TArrow` 
                                          TIntrinsic ( IRecord
                                              ( TRow
                                                  ( RExtend
                                                      "$$Head"
                                                      (TVariable (TypeIndex KType 2) `TArrow` TIntrinsic IInt32)
                                                      (RExtend "$$Tail" (TVariable (TypeIndex KType 2) `TArrow` TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TIntrinsic IInt32 :| [])) RNil)
                                                  )
                                              )
                                          )
                            ) "$unfold.1"))
                            ( ELambda
                                ()
                                (PVariable () (Label (TIntrinsic IInt32) "n") :| [])
                                ( ERecord
                                    ()
                                      ( TIntrinsic
                                          ( IRecord
                                              ( TRow
                                                  ( RExtend
                                                      "Head"
                                                      (TIntrinsic IInt32)
                                                      (RExtend "Tail" (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TIntrinsic IInt32 :| [])) RNil)
                                                  )
                                              )
                                          )
                                      )
                                    ( Map.fromList
                                        [
                                          ( "$$Head"
                                          , ELambda
                                              ()
                                              (PAny () (TIntrinsic IUnit) :| [])
                                              (EVariable () (Label (TIntrinsic IInt32) "n"))
                                          )
                                        ,
                                          ( "$$Tail"
                                          , ELambda
                                              ()
                                              (PAny () (TIntrinsic IUnit) :| [])
                                              ( EApplication
                                                  ()
                                                  (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TIntrinsic IInt32 :| []))
                                                  (EVariable () (Label (TIntrinsic IInt32 `TArrow` TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TIntrinsic IInt32 :| [])) "$unfold.1"))
                                                  ( EApplication
                                                      ()
                                                      (TIntrinsic IInt32)
                                                      (EBinaryOperator () (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) OAddition)
                                                      ( EVariable () (Label (TIntrinsic IInt32) "n")
                                                          <| ELiteral () (LInt32 1)
                                                          :| []
                                                      )
                                                      :| []
                                                  )
                                              )
                                          )
                                        ]
                                    )
                                    Nothing
                                )
                            )
                            (EVariable () (Label (TIntrinsic IInt32 `TArrow` TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TIntrinsic IInt32 :| [])) "$unfold.1"))
                        )
--                    )
--                )
--                (ELiteral () (LInt32 0) :| [])
--            )
--        )
--    , DFunction
--        "nth"
--        ( Function
--            ()
--            (With [] (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TVariable (TypeIndex KType 0) :| []) `TArrow` TVariable (TypeIndex KType 0)))
--            (PVariable () (Label (TIntrinsic INat) "n") :| [])
--            ( EFold
--                ()
--                undefined
--                (EVariable () (Label (TIntrinsic INat) "n") :| [])
--                ( EClause
--                    ()
--                    ( PConstructor
--                        ()
--                        (Label (TIntrinsic INat) "Zero")
--                        []
--                    )
--                    ( CPlain
--                        ()
--                        []
--                        ( ELambda
--                            ()
--                            (PVariable () (Label (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TVariable (TypeIndex KType 0) :| [])) "stream") :| [])
--                            ( ECodataSelect
--                                ()
--                                (Label undefined "Head")
--                                (EVariable () (Label (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TVariable (TypeIndex KType 0) :| [])) "stream"))
--                                ( Just
--                                    ( EApplication
--                                        ()
--                                        undefined
--                                        (EVariable () (Label undefined "$$force_Head"))
--                                        (EVariable () (Label (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TVariable (TypeIndex KType 0) :| [])) "stream") :| [])
--                                    )
--                                )
--                            )
--                        )
--                        :| []
--                    )
--                    <| EClause
--                      ()
--                      ( PConstructor
--                          ()
--                          (Label (TIntrinsic INat `TArrow` TIntrinsic INat) "Succ")
--                          [ PAtVariable () (Label undefined "f")
--                          ]
--                      )
--                      ( CPlain
--                          ()
--                          []
--                          ( ELambda
--                              ()
--                              (PVariable () (Label undefined "stream") :| [])
--                              ( EApplication
--                                  ()
--                                  undefined
--                                  (EVariable () (Label undefined "f"))
--                                  ( ECodataSelect
--                                      ()
--                                      (Label undefined "Tail")
--                                      (EVariable () (Label undefined "stream"))
--                                      ( Just
--                                          ( EApplication
--                                              ()
--                                              (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TIntrinsic IInt32 :| []))
--                                              ( EVariable
--                                                  ()
--                                                  ( Label
--                                                      ( TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TIntrinsic IInt32 :| [])
--                                                          `TArrow` TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TIntrinsic IInt32 :| [])
--                                                      )
--                                                      "$$force_Tail"
--                                                  )
--                                              )
--                                              (EVariable () (Label (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TIntrinsic IInt32 :| [])) "stream") :| [])
--                                          )
--                                      )
--                                      :| []
--                                  )
--                              )
--                          )
--                          :| []
--                      )
--                    :| []
--                )
--                ( Just
--                    ( ERecursiveLet
--                        ()
--                        (PVariable () (Label{labelTag = TIntrinsic INat `TArrow` undefined, labelName = "$fold.1"}))
--                        ( ELambda
--                            ()
--                            (PVariable () (Label{labelTag = undefined, labelName = "$fold.1.expr"}) :| [])
--                            ( EMatch
--                                ()
--                                undefined
--                                (EVariable () (Label{labelTag = undefined, labelName = "$fold.1.expr"}))
--                                ( EClause
--                                    ()
--                                    (PConstructor () (Label{labelTag = undefined, labelName = "Zero"}) [])
--                                    ( CPlain
--                                        ()
--                                        []
--                                        ( ELambda
--                                            ()
--                                            ( PVariable
--                                                ()
--                                                (Label{labelTag = undefined, labelName = "stream"})
--                                                :| []
--                                            )
--                                            ( ECodataSelect
--                                                ()
--                                                (Label{labelTag = undefined, labelName = "Head"})
--                                                (EVariable () (Label{labelTag = TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TIntrinsic IInt32 :| []), labelName = "stream"}))
--                                                ( Just
--                                                    ( EApplication
--                                                        ()
--                                                        (TIntrinsic IUnit `TArrow` TIntrinsic IInt32)
--                                                        ( EVariable
--                                                            ()
--                                                            ( Label
--                                                                { labelTag =
--                                                                    TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TIntrinsic IInt32 :| [])
--                                                                      `TArrow` TIntrinsic IUnit
--                                                                      `TArrow` TIntrinsic IInt32
--                                                                , labelName = "$$force_Head"
--                                                                }
--                                                            )
--                                                        )
--                                                        (EVariable () (Label{labelTag = TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TIntrinsic IInt32 :| []), labelName = "stream"}) :| [])
--                                                    )
--                                                )
--                                            )
--                                        )
--                                        :| []
--                                    )
--                                    :| [ EClause
--                                          ()
--                                          ( PConstructor
--                                              ()
--                                              (Label{labelTag = undefined, labelName = "Succ"})
--                                              [ PVariable
--                                                  ()
--                                                  (Label{labelTag = undefined, labelName = "f"})
--                                              ]
--                                          )
--                                          ( CPlain
--                                              ()
--                                              []
--                                              ( ELambda
--                                                  ()
--                                                  (PVariable () (Label{labelTag = undefined, labelName = "stream"}) :| [])
--                                                  ( EApplication
--                                                      ()
--                                                      undefined
--                                                      (EVariable () (Label{labelTag = undefined, labelName = "$fold.1"}))
--                                                      ( EVariable () (Label{labelTag = undefined, labelName = "f"})
--                                                          :| [ ECodataSelect
--                                                                ()
--                                                                (Label{labelTag = undefined, labelName = "Tail"})
--                                                                (EVariable () (Label{labelTag = undefined, labelName = "stream"}))
--                                                                ( Just
--                                                                    ( EApplication
--                                                                        ()
--                                                                        (TIntrinsic IUnit `TArrow` undefined)
--                                                                        (EVariable () (Label{labelTag = undefined, labelName = "$$force_Tail"}))
--                                                                        (EVariable () (Label{labelTag = undefined, labelName = "stream"}) :| [])
--                                                                    )
--                                                                )
--                                                             ]
--                                                      )
--                                                  )
--                                              )
--                                              :| []
--                                          )
--                                       ]
--                                )
--                            )
--                        )
--                        ( EApplication
--                            ()
--                            undefined
--                            (EVariable () (Label{labelTag = undefined, labelName = "$fold.1"}))
--                            (EVariable () (Label{labelTag = TIntrinsic INat, labelName = "n"}) :| [])
--                        )
--                    )
--                )
--            )
        )
--    , DFunction
--        "main"
--        ( Function
--            ()
--            (With [] (TVariable (TypeIndex KType 0)))
--            (PLiteral () LUnit :| [])
--            ( ELet
--                ()
--                ( BPattern
--                    ()
--                    (PVariable () (Label (TIntrinsic IInt32) "v"))
--                    ( EApplication
--                        ()
--                        (TIntrinsic IInt32)
--                        (EVariable () (Label (TIntrinsic INat `TArrow` TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TIntrinsic IInt32 :| []) `TArrow` TIntrinsic IInt32) "nth"))
--                        ( EApplication () (TIntrinsic INat) (EVariable () (Label (TIntrinsic IInt32 `TArrow` TIntrinsic INat) "from_int32")) (ELiteral () (LInt32 5) :| [])
--                            <| EVariable () (Label (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TIntrinsic IInt32 :| [])) "nats")
--                            :| []
--                        )
--                    )
--                    :| []
--                )
--                ( EApplication
--                    ()
--                    (TVariable (TypeIndex KType 0))
--                    (EVariable () (Label (TIntrinsic IString `TArrow` TVariable (TypeIndex KType 0)) "trace_int32"))
--                    ( EVariable () (Label (TIntrinsic IInt32) "v")
--                        :| []
--                    )
--                )
--            )
--        )
    ]
