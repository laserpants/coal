{-# LANGUAGE OverloadedStrings #-}

module Coal.Set4.Test10 where

import Coal.Common.List1 (NonEmpty (..), (<|))
import Coal.Common.Label (Label (..))
import Coal.Language
import Coal.Language.Module (Constant (..), Definition (..), Function (..), Module (..), Path (..))

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Coal.Language.Module as Module

prog4_10 :: [Module () Kind IndexedType]
prog4_10 =
  [ moduleMain
  ]

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
    , --    , DType
      --        "$$Stream"
      --        [Parameter () "a"]
      --        [ Constructor
      --            "$$Stream"
      --            1
      --            ( Forall
      --                (Set.fromList [Parameter () "a"])
      --                []
      --                ( TRow ( RExtend "$$Head" (TArrow (TIntrinsic IUnit) (TVariable (Parameter () "a"))) ( RExtend "$$Tail" (TArrow (TIntrinsic IUnit) (TApplication () (TConstructor () "Stream") (TVariable (Parameter () "a") :| []))) RNil))
      --                    `TArrow` (TApplication () (TConstructor () "$$Stream") (TVariable (Parameter () "a") :| []))
      --                )
      --            )
      --        ]
      DConstant
        "nats"
        ( Constant
            ()
            (With [] (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TIntrinsic IInt32 :| [])))
            ( EApplication
                ()
                (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TIntrinsic IInt32 :| []))
                ( EUnfold
                    ()
                    (TIntrinsic IInt32 `TArrow` TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TIntrinsic IInt32 :| []))
                    (Label (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TIntrinsic IInt32 :| [])) "Stream")
                    "f"
                    ( PAnnotation
                        ()
                        (TIntrinsic IInt32)
                        (PVariable () (Label (TIntrinsic IInt32) "n"))
                        :| []
                    )
                    ( Map.fromList
                        [
                          ( "Head"
                          , EVariable () (Label (TIntrinsic IInt32) "n")
                          )
                        ,
                          ( "Tail"
                          , EApplication
                              ()
                              (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TIntrinsic IInt32 :| []))
                              (EVariable () (Label (TIntrinsic IInt32 `TArrow` TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TIntrinsic IInt32 :| [])) "f"))
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
                        ]
                    )
                    ( Just
                        ( ERecursiveLet
                            ()
                            (PVariable () (Label (TIntrinsic IInt32 `TArrow` TIntrinsic IVoid) "$unfold.1"))
                            ( ELambda
                                ()
                                (PVariable () (Label (TIntrinsic IInt32) "n") :| [])
                                ( ECodataFields
                                    ()
                                    (TIntrinsic IVoid)
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
                                                  (TIntrinsic IVoid)
                                                  (EVariable () (Label (TIntrinsic IInt32 `TArrow` TIntrinsic IVoid) "$unfold.1"))
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
                                )
                            )
                            (EVariable () (Label (TIntrinsic IInt32 `TArrow` TIntrinsic IVoid) "$unfold.1"))
                        )
                    )
                )
                (ELiteral () (LInt32 0) :| [])
            )
        )
    , DConstant
        "nth"
        ( Constant
            ()
            (With [] (TIntrinsic INat `TArrow` TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TVariable (TypeIndex KType 0) :| []) `TArrow` TVariable (TypeIndex KType 0)))
            ( ELambda
                ()
                (PVariable () (Label (TIntrinsic INat) "n") :| [])
                ( EFold
                    ()
                    (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TVariable (TypeIndex KType 0) :| []) `TArrow` TVariable (TypeIndex KType 0))
                    (EVariable () (Label (TIntrinsic INat) "n") :| [])
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
                            ( ELambda
                                ()
                                (PVariable () (Label (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TVariable (TypeIndex KType 0) :| [])) "stream") :| [])
                                ( ECodataSelect
                                    ()
                                    (Label (TVariable (TypeIndex KType 0)) "Head")
                                    (EVariable () (Label (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TVariable (TypeIndex KType 0) :| [])) "stream"))
                                    ( Just
                                        ( EApplication
                                            ()
                                            (TVariable (TypeIndex KType 0))
                                            (EVariable () (Label (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TVariable (TypeIndex KType 0) :| []) `TArrow` TVariable (TypeIndex KType 0)) "$$force_Head"))
                                            (EVariable () (Label (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TVariable (TypeIndex KType 0) :| [])) "stream") :| [])
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
                              (Label (TIntrinsic INat) "Succ")
                              [ PAtVariable () (Label (TIntrinsic INat) "f")
                              ]
                          )
                          ( CPlain
                              ()
                              []
                              ( ELambda
                                  ()
                                  (PVariable () (Label (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TVariable (TypeIndex KType 0) :| [])) "stream") :| [])
                                  ( EApplication
                                      ()
                                      (TVariable (TypeIndex KType 0))
                                      (EVariable () (Label (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TVariable (TypeIndex KType 0) :| []) `TArrow` TVariable (TypeIndex KType 0)) "f"))
                                      ( ECodataSelect
                                          ()
                                          (Label (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TVariable (TypeIndex KType 0) :| [])) "Tail")
                                          (EVariable () (Label (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TVariable (TypeIndex KType 0) :| [])) "stream"))
                                          ( Just
                                              ( EApplication
                                                  ()
                                                  (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TVariable (TypeIndex KType 0) :| []))
                                                  (EVariable () (Label (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TVariable (TypeIndex KType 0) :| []) `TArrow` TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TVariable (TypeIndex KType 0) :| [])) "$$force_Tail"))
                                                  (EVariable () (Label (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TVariable (TypeIndex KType 0) :| [])) "stream") :| [])
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
                            (PVariable () (Label{labelTag = TIntrinsic INat `TArrow` TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TVariable (TypeIndex KType 0) :| []) `TArrow` TVariable (TypeIndex KType 0), labelName = "$fold.1"}))
                            ( ELambda
                                ()
                                (PVariable () (Label{labelTag = TIntrinsic INat, labelName = "$fold.1.expr"}) :| [])
                                ( ECompiledMatch
                                    ()
                                    (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TVariable (TypeIndex KType 0) :| []) `TArrow` TVariable (TypeIndex KType 0))
                                    (EVariable () (Label{labelTag = TIntrinsic INat, labelName = "$fold.1.expr"}))
                                    ( ECompiledClause
                                        (Label (TIntrinsic INat `TArrow` TIntrinsic INat) "Succ" <| Label (TIntrinsic INat) "$match.1.f" :| [])
                                        ( ELambda
                                            ()
                                            (PVariable () (Label (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TVariable (TypeIndex KType 0) :| [])) "stream") :| [])
                                            ( EApplication
                                                ()
                                                (TVariable (TypeIndex KType 0))
                                                (EVariable () (Label (TIntrinsic INat `TArrow` TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TVariable (TypeIndex KType 0) :| []) `TArrow` TVariable (TypeIndex KType 0)) "$fold.1"))
                                                ( EVariable () (Label (TIntrinsic INat) "$match.1.f")
                                                    <| ECodataSelect
                                                      ()
                                                      (Label (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TVariable (TypeIndex KType 0) :| [])) "Tail")
                                                      (EVariable () (Label (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TVariable (TypeIndex KType 0) :| [])) "stream"))
                                                      ( Just
                                                          ( EApplication
                                                              ()
                                                              (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TVariable (TypeIndex KType 0) :| []))
                                                              (EVariable () (Label (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TVariable (TypeIndex KType 0) :| []) `TArrow` TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TVariable (TypeIndex KType 0) :| [])) "$$force_Tail"))
                                                              (EVariable () (Label (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TVariable (TypeIndex KType 0) :| [])) "stream") :| [])
                                                          )
                                                      )
                                                    :| []
                                                )
                                            )
                                        )
                                        <| ECompiledClause
                                          (Label (TIntrinsic INat) "Zero" :| [])
                                          ( ELambda
                                              ()
                                              (PVariable () (Label (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TVariable (TypeIndex KType 0) :| [])) "stream") :| [])
                                              ( ECodataSelect
                                                  ()
                                                  (Label (TVariable (TypeIndex KType 0)) "Head")
                                                  (EVariable () (Label (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TVariable (TypeIndex KType 0) :| [])) "stream"))
                                                  ( Just
                                                      ( EApplication
                                                          ()
                                                          (TVariable (TypeIndex KType 0))
                                                          (EVariable () (Label (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TVariable (TypeIndex KType 0) :| []) `TArrow` TVariable (TypeIndex KType 0)) "$$force_Head"))
                                                          (EVariable () (Label (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TVariable (TypeIndex KType 0) :| [])) "stream") :| [])
                                                      )
                                                  )
                                              )
                                          )
                                        :| []
                                    )
                                    -- (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TVariable (TypeIndex KType 0) :| []) `TArrow` TVariable (TypeIndex KType 0))
                                    -- (EVariable () (Label{labelTag = TIntrinsic INat, labelName = "$fold.1.expr"}))
                                    -- ( EClause
                                    --    ()
                                    --    ( PConstructor
                                    --        ()
                                    --        (Label (TIntrinsic INat) "Zero")
                                    --        []
                                    --    )
                                    --    ( CPlain
                                    --        ()
                                    --        []
                                    --        ( ELambda
                                    --            ()
                                    --            (PVariable () (Label (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TVariable (TypeIndex KType 0) :| [])) "stream") :| [])
                                    --            ( ECodataSelect
                                    --                ()
                                    --                (Label (TVariable (TypeIndex KType 0)) "Head")
                                    --                (EVariable () (Label (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TVariable (TypeIndex KType 0) :| [])) "stream"))
                                    --                ( Just
                                    --                    ( EApplication
                                    --                        ()
                                    --                        (TVariable (TypeIndex KType 0))
                                    --                        (EVariable () (Label (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TVariable (TypeIndex KType 0) :| []) `TArrow` TVariable (TypeIndex KType 0)) "$$force_Head"))
                                    --                        (EVariable () (Label (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TVariable (TypeIndex KType 0) :| [])) "stream") :| [])
                                    --                    )
                                    --                )
                                    --            )
                                    --        )
                                    --        :| []
                                    --    )
                                    --    <| EClause
                                    --      ()
                                    --      ( PConstructor
                                    --          ()
                                    --          (Label (TIntrinsic INat `TArrow` TIntrinsic INat) "Succ")
                                    --          [ PVariable () (Label (TIntrinsic INat) "f")
                                    --          ]
                                    --      )
                                    --      ( CPlain
                                    --          ()
                                    --          []
                                    --          ( ELambda
                                    --              ()
                                    --              (PVariable () (Label (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TVariable (TypeIndex KType 0) :| [])) "stream") :| [])
                                    --              ( EApplication
                                    --                  ()
                                    --                  (TVariable (TypeIndex KType 0))
                                    --                  (EVariable () (Label (TIntrinsic INat `TArrow` TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TVariable (TypeIndex KType 0) :| []) `TArrow` TVariable (TypeIndex KType 0)) "$fold.1"))
                                    --                  ( EVariable () (Label (TIntrinsic INat) "f")
                                    --                      <| ECodataSelect
                                    --                        ()
                                    --                        (Label (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TVariable (TypeIndex KType 0) :| [])) "Tail")
                                    --                        (EVariable () (Label (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TVariable (TypeIndex KType 0) :| [])) "stream"))
                                    --                        ( Just
                                    --                            ( EApplication
                                    --                                ()
                                    --                                (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TVariable (TypeIndex KType 0) :| []))
                                    --                                (EVariable () (Label (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TVariable (TypeIndex KType 0) :| []) `TArrow` TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TVariable (TypeIndex KType 0) :| [])) "$$force_Tail"))
                                    --                                (EVariable () (Label (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TVariable (TypeIndex KType 0) :| [])) "stream") :| [])
                                    --                            )
                                    --                        )
                                    --                      :| []
                                    --                  )
                                    --              )
                                    --          )
                                    --          :| []
                                    --      )
                                    --    :| []
                                    -- )
                                )
                            )
                            ( EApplication
                                ()
                                (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TVariable (TypeIndex KType 0) :| []) `TArrow` TVariable (TypeIndex KType 0))
                                (EVariable () (Label{labelTag = TIntrinsic INat `TArrow` TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TVariable (TypeIndex KType 0) :| []) `TArrow` TVariable (TypeIndex KType 0), labelName = "$fold.1"}))
                                (EVariable () (Label{labelTag = TIntrinsic INat, labelName = "n"}) :| [])
                            )
                        )
                    )
                )
            )
        )
    , DConstant
        "main"
        ( Constant
            ()
            (With [] (TIntrinsic IUnit `TArrow` TVariable (TypeIndex KType 0)))
            ( ELambda
                ()
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
                            ( EApplication
                                ()
                                (TIntrinsic INat)
                                ( EApplication
                                    ()
                                    (TIntrinsic IInt32 `TArrow` TIntrinsic INat)
                                    (EVariable () (Label (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TIntrinsic INat :| []) `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic INat) "from_int32"))
                                    ( ERecord
                                        ()
                                        (TApplication KTrait (TConstructor (KArrow KType KTrait) "Numeric") (TIntrinsic INat :| []))
                                        ( Map.fromList
                                            [
                                              ( "from_int32"
                                              , EVariable () (Label{labelTag = TArrow (TIntrinsic IInt32) (TIntrinsic INat), labelName = "from_int32__$instance.a952655fec712ed8"})
                                              )
                                            ]
                                        )
                                        Nothing
                                        :| []
                                    )
                                )
                                ( ELiteral () (LInt32 5)
                                    :| []
                                )
                                <| EVariable () (Label (TApplication KType (TConstructor (KType `KArrow` KType) "Stream") (TIntrinsic IInt32 :| [])) "nats")
                                :| []
                            )
                        )
                        :| []
                    )
                    ( EApplication
                        ()
                        (TVariable (TypeIndex KType 0))
                        (EVariable () (Label (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 0)) "trace_int32"))
                        ( EVariable () (Label (TIntrinsic IInt32) "v")
                            :| []
                        )
                    )
                )
            )
        )
    ]
