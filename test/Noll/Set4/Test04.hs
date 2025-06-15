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
    [ DImport (Path ["Core$"]) ["trace_string"]
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
            (With [] undefined)
            ( EApplication
                ()
                undefined
                ( EUnfold
                    ()
                    undefined
                    (Label undefined "Stream")
                    "f"
                    ( PAnnotation
                        ()
                        (TIntrinsic IInt32)
                        (PVariable () (Label undefined "n"))
                        :| []
                    )
                    ( Map.fromList
                        [
                          ( "Head"
                          , EVariable () (Label undefined "n")
                          )
                        ,
                          ( "Tail"
                          , EApplication
                              ()
                              undefined
                              (EVariable () (Label undefined "f"))
                              ( EApplication
                                  ()
                                  undefined
                                  (EBinaryOperator () undefined OAddition)
                                  ( EVariable () (Label undefined "n")
                                      <| ELiteral () (LInt32 1)
                                      :| []
                                  )
                                  :| []
                              )
                          )
                        ]
                    )
                    ( Just
                        ( ERecursiveLet
                            ()
                            (PVariable () (Label undefined "$unfold.1"))
                            ( ELambda
                                ()
                                (PVariable () (Label undefined "n") :| [])
                                ( ERecord
                                    ()
                                    undefined
                                    ( Map.fromList
                                        [
                                          ( "Head"
                                          , ELambda
                                              ()
                                              (PAny () undefined :| [])
                                              (EVariable () (Label undefined "n"))
                                          )
                                        ,
                                          ( "Tail"
                                          , ELambda
                                              ()
                                              (PAny () undefined :| [])
                                              ( EApplication
                                                  ()
                                                      undefined
                                                  (EVariable () (Label undefined "$unfold.1"))
                                                  ( EApplication
                                                      ()
                                                      undefined
                                                      (EBinaryOperator () undefined OAddition)
                                                      ( EVariable () (Label undefined "n")
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
                            (EVariable () (Label undefined "$unfold.1"))
                        )
                    )
                )
                (ELiteral () (LInt32 0) :| [])
            )
        )
    , DFunction
        "nth"
        ( Function
            ()
            (With [] undefined)
            (PVariable () (Label undefined "n") :| [])
            ( EFold
                ()
                undefined
                (EVariable () (Label undefined "n") :| [])
                ( EClause
                    ()
                    ( PConstructor
                        ()
                        (Label undefined "Zero")
                        []
                    )
                    ( CPlain
                        ()
                        []
                        ( ELambda
                            ()
                            (PVariable () (Label undefined "stream") :| [])
                            ( ECodataSelect
                                ()
                                (Label undefined "Head")
                                (EVariable () (Label undefined "stream"))
                                ( Just
                                    ( EApplication
                                        ()
                                        undefined
                                        (EVariable () (Label undefined "$$force_Head"))
                                        (EVariable () (Label undefined "stream") :| [])
                                    )
                                )
                            )
                        )
                        :| []
                    )
                    <| EClause
                      ()
                      ( PConstructor
                          ()
                          (Label undefined "Succ")
                          [ PAtVariable () (Label undefined "f")
                          ]
                      )
                      ( CPlain
                          ()
                          []
                          ( ELambda
                              ()
                              (PVariable () (Label undefined "stream") :| [])
                              ( EApplication
                                  ()
                                  undefined
                                  (EVariable () (Label undefined "f"))
                                  ( ECodataSelect
                                      ()
                                      (Label undefined "Tail")
                                      (EVariable () (Label undefined "stream"))
                                      ( Just
                                          ( EApplication
                                              ()
                                             undefined
                                              (EVariable () (Label undefined "$$force_Tail"))
                                              (EVariable () (Label undefined "stream") :| [])
                                          )
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
                        (PVariable () (Label{labelTag = undefined, labelName = "$fold.1"}))
                        ( ELambda
                            ()
                            (PVariable () (Label{labelTag = undefined, labelName = "$fold.1.expr"}) :| [])
                            ( EMatch
                                ()
                                undefined
                                (EVariable () (Label{labelTag = undefined, labelName = "$fold.1.expr"}))
                                ( EClause
                                    ()
                                    (PConstructor () (Label{labelTag = undefined, labelName = "Zero"}) [])
                                    ( CPlain
                                        ()
                                        []
                                        ( ELambda
                                            ()
                                            ( PVariable
                                                ()
                                                (Label{labelTag = undefined, labelName = "stream"})
                                                :| []
                                            )
                                            ( ECodataSelect
                                                ()
                                                (Label{labelTag = undefined, labelName = "Head"})
                                                (EVariable () (Label{labelTag = undefined, labelName = "stream"}))
                                                ( Just
                                                    ( EApplication
                                                        ()
                                                        undefined
                                                        (EVariable () (Label{labelTag = undefined, labelName = "$$force_Head"}))
                                                        (EVariable () (Label{labelTag = undefined, labelName = "stream"}) :| [])
                                                    )
                                                )
                                            )
                                        )
                                        :| []
                                    )
                                    :| [ EClause
                                          ()
                                          ( PConstructor
                                              ()
                                              (Label{labelTag = undefined, labelName = "Succ"})
                                              [ PVariable
                                                  ()
                                                  (Label{labelTag = undefined, labelName = "f"})
                                              ]
                                          )
                                          ( CPlain
                                              ()
                                              []
                                              ( ELambda
                                                  ()
                                                  (PVariable () (Label{labelTag = undefined, labelName = "stream"}) :| [])
                                                  ( EApplication
                                                      ()
                                                      undefined
                                                      (EVariable () (Label{labelTag = undefined, labelName = "$fold.1"}))
                                                      ( EVariable () (Label{labelTag = undefined, labelName = "f"})
                                                          :| [ ECodataSelect
                                                                ()
                                                                (Label{labelTag = undefined, labelName = "Tail"})
                                                                (EVariable () (Label{labelTag = undefined, labelName = "stream"}))
                                                                ( Just
                                                                    ( EApplication
                                                                        ()
                                                                        undefined
                                                                        (EVariable () (Label{labelTag = undefined, labelName = "$$force_Tail"}))
                                                                        (EVariable () (Label{labelTag = undefined, labelName = "stream"}) :| [])
                                                                    )
                                                                )
                                                             ]
                                                      )
                                                  )
                                              )
                                              :| []
                                          )
                                       ]
                                )
                            )
                        )
                        ( EApplication
                            ()
                            undefined
                            (EVariable () (Label{labelTag = undefined, labelName = "$fold.1"}))
                            (EVariable () (Label{labelTag = undefined, labelName = "n"}) :| [])
                        )
                    )
                )
            )
        )
    , DFunction
        "main"
        ( Function
            ()
            (With [] (TVariable (TypeIndex KType 0)))
            (PLiteral () LUnit :| [])
            ( ELet
                ()
                ( BPattern
                    ()
                    (PVariable () (Label (TIntrinsic IInt32) "v"))
                    ( EApplication
                        ()
                        (TIntrinsic IInt32)
                        (EVariable () (Label (TIntrinsic INat `TArrow` TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TIntrinsic IInt32 :| []) `TArrow` TIntrinsic IInt32) "nth"))
                        ( EApplication () undefined (EVariable () (Label undefined "from_int32")) (ELiteral () (LInt32 5) :| [])
                            <| EVariable () (Label (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TIntrinsic IInt32 :| [])) "nats")
                            :| []
                        )
                    )
                    :| []
                )
                ( EApplication
                    ()
                    (TVariable (TypeIndex KType 0))
                    (EVariable () (Label (TIntrinsic IString `TArrow` TVariable (TypeIndex KType 0)) "trace_int32"))
                    ( EVariable () (Label (TIntrinsic IInt32) "v")
                        :| []
                    )
                )
            )
        )
    ]
