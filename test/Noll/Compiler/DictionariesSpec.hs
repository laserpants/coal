{-# LANGUAGE OverloadedStrings #-}

module Noll.Compiler.DictionariesSpec where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Generics.Uniplate.Data (transformM)
import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Noll.Compiler.Dictionaries
import Noll.Language
import Test.Hspec (Spec, describe, it)

import qualified Data.Map.Strict as Map

spec :: Spec
spec = do
  undefined

fixtured1 =
  undefined

runTraitTransformY v = fst $ runWriter (evalStateT (runReaderT v testEnv) 200)

testEnv = DictionaryEnvironment yy xx

fixtured2 = runTraitTransformY (collectTraitsY (TIntrinsic IInt32) "trace")

fixturee1 =
  ELambda
    ()
    (PVariable () (Label (TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)])) "p") :| [])
    ( EApplication
        ()
        (TIntrinsic IString)
        (EVariable () (Label (TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)]) `TArrow` TIntrinsic IString) "pair_to_string"))
        (EVariable () (Label (TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)])) "p") :| [])
    )

traceableTrait t =
  TApplication
    KTrait
    (TConstructor (KType `KArrow` KTrait) "Traceable")
    (t :| [])

fixturee2 =
  ELambda
    ()
    ( PDictionary () (traceableTrait (TVariable (TypeIndex KType 0))) (Trait "Traceable" (TVariable (TypeIndex KType 0)))
        <| PDictionary () (traceableTrait (TVariable (TypeIndex KType 1))) (Trait "Traceable" (TVariable (TypeIndex KType 1)))
        :| []
    )
    ( ELambda
        ()
        (PVariable () (Label (TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)])) "p") :| [])
        ( EApplication
            ()
            (TIntrinsic IString)
            ( EApplication
                ()
                (TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)]) `TArrow` TIntrinsic IString)
                ( EVariable
                    ()
                    ( Label
                        ( traceableTrait (TVariable (TypeIndex KType 0))
                            `TArrow` traceableTrait (TVariable (TypeIndex KType 1))
                            `TArrow` TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)])
                            `TArrow` TIntrinsic IString
                        )
                        "pair_to_string"
                    )
                )
                ( EDictionary () (traceableTrait (TVariable (TypeIndex KType 0))) (Trait "Traceable" (TVariable (TypeIndex KType 0)))
                    <| EDictionary () (traceableTrait (TVariable (TypeIndex KType 1))) (Trait "Traceable" (TVariable (TypeIndex KType 1)))
                    :| []
                )
            )
            ( EVariable () (Label (TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)])) "p")
                :| []
            )
        )
    )

fixturee3 = fst $ runTraitTransformY (transformScope fixturee1)

fixturee4 =
  ELambda
    ()
    (PVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 0)))) "lst") :| [])
    ( EApplication
        ()
        (TIntrinsic IString)
        (EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 0))) `TArrow` TIntrinsic IString) "list_to_string"))
        (EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 0)))) "lst") :| [])
    )

fixturee5 =
  ELambda
    ()
    ( PDictionary () (traceableTrait (TVariable (TypeIndex KType 0))) (Trait "Traceable" (TVariable (TypeIndex KType 0)))
        :| []
    )
    ( ELambda
        ()
        (PVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 0)))) "lst") :| [])
        ( EApplication
            ()
            (TIntrinsic IString)
            ( EApplication
                ()
                (TIntrinsic (IList (TVariable (TypeIndex KType 0))) `TArrow` TIntrinsic IString)
                ( EVariable
                    ()
                    ( Label
                        ( traceableTrait (TVariable (TypeIndex KType 0))
                            `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 0)))
                            `TArrow` TIntrinsic IString
                        )
                        "list_to_string"
                    )
                )
                ( EDictionary () (traceableTrait (TVariable (TypeIndex KType 0))) (Trait "Traceable" (TVariable (TypeIndex KType 0)))
                    :| []
                )
            )
            (EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 0)))) "lst") :| [])
        )
    )

fixturee6 = fst $ runTraitTransformY (transformScope fixturee4)

fixturee7 =
  ELet
    ()
    ( BPattern
        ()
        (PVariable () (Label (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString])) "p"))
        ( ETuple
            ()
            (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]))
            ( ELiteral () (LInt32 1)
                <| ELiteral () (LString "hello")
                :| []
            )
        )
        :| []
    )
    ( EApplication
        ()
        (TIntrinsic IString)
        (EVariable () (Label (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]) `TArrow` TIntrinsic IString) "trace"))
        ( EVariable () (Label (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString])) "p")
            :| []
        )
    )

fixturee8 :: Expression () (Type TypeIndex Kind)
fixturee8 =
  ELet
    ()
    ( BPattern
        ()
        (PVariable () (Label (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString])) "p"))
        ( ETuple
            ()
            (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]))
            ( ELiteral () (LInt32 1)
                <| ELiteral () (LString "hello")
                :| []
            )
        )
        :| []
    )
    ( EApplication
        ()
        (TIntrinsic IString)
        ( EApplication
            ()
            (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]) `TArrow` TIntrinsic IString)
            ( EVariable
                ()
                ( Label
                    ( traceableTrait (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]))
                        `TArrow` TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString])
                        `TArrow` TIntrinsic IString
                    )
                    "trace"
                )
            )
            ( ERecord
                ()
                (traceableTrait (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString])))
                ( Map.fromList
                    [
                      ( "trace"
                      , EApplication
                          ()
                          (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]) `TArrow` TIntrinsic IString)
                          ( EVariable
                              ()
                              ( Label
                                  ( traceableTrait (TIntrinsic IInt32)
                                      `TArrow` traceableTrait (TIntrinsic IString)
                                      `TArrow` TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString])
                                      `TArrow` TIntrinsic IString
                                  )
                                  "trace__$instance.895a62bb6130678a"
                              )
                          )
                          ( ERecord
                              ()
                              (traceableTrait (TIntrinsic IInt32))
                              ( Map.fromList
                                  [
                                    ( "trace"
                                    , EVariable () (Label (TIntrinsic IInt32 `TArrow` TIntrinsic IString) "trace__$instance.c847f12006235dc0")
                                    )
                                  ]
                              )
                              Nothing
                              <| ERecord
                                ()
                                (traceableTrait (TIntrinsic IString))
                                ( Map.fromList
                                    [
                                      ( "trace"
                                      , EVariable () (Label (TIntrinsic IString `TArrow` TIntrinsic IString) "trace__$instance.c81d5162b7d14248")
                                      )
                                    ]
                                )
                                Nothing
                              :| []
                          )
                      )
                    ]
                )
                Nothing
                :| []
            )
        )
        ( EVariable () (Label (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString])) "p")
            :| []
        )
    )

fixturee9 :: Expression () (Type TypeIndex Kind)
fixturee9 = fst $ runTraitTransformY (transformScope fixturee7)

fixturee10 =
  EApplication
    ()
    (TIntrinsic IString)
    (EVariable () (Label (TIntrinsic (IList (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]))) `TArrow` TIntrinsic IString) "trace"))
    ( EListLiteral
        ()
        (TIntrinsic (IList (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]))))
        [ ETuple
            ()
            (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]))
            ( ELiteral () (LInt32 1)
                <| ELiteral () (LString "a")
                :| []
            )
        , ETuple
            ()
            (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]))
            ( ELiteral () (LInt32 2)
                <| ELiteral () (LString "b")
                :| []
            )
        ]
        :| []
    )

fixturee11 :: Expression () (Type TypeIndex Kind)
fixturee11 =
  EApplication
    ()
    (TIntrinsic IString)
    ( EApplication
        ()
        (TIntrinsic (IList (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]))) `TArrow` TIntrinsic IString)
        (EVariable () (Label (traceableTrait (TIntrinsic (IList (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString])))) `TArrow` TIntrinsic (IList (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]))) `TArrow` TIntrinsic IString) "trace"))
        ( ERecord
            ()
            (traceableTrait (TIntrinsic (IList (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString])))))
            ( Map.fromList
                [
                  ( "trace"
                  , EApplication
                      ()
                      (TIntrinsic (IList (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]))) `TArrow` TIntrinsic IString)
                      ( EVariable
                          ()
                          ( Label
                              ( traceableTrait (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]))
                                  `TArrow` TIntrinsic (IList (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString])))
                                  `TArrow` TIntrinsic IString
                              )
                              "trace__$instance.8582bc20351fc496"
                          )
                      )
                      ( ERecord
                          ()
                          (traceableTrait (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString])))
                          ( Map.fromList
                              [
                                ( "trace"
                                , EApplication
                                    ()
                                    (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]) `TArrow` TIntrinsic IString)
                                    ( EVariable
                                        ()
                                        ( Label
                                            ( traceableTrait (TIntrinsic IInt32)
                                                `TArrow` traceableTrait (TIntrinsic IString)
                                                `TArrow` TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString])
                                                `TArrow` TIntrinsic IString
                                            )
                                            "trace__$instance.895a62bb6130678a"
                                        )
                                    )
                                    ( ERecord
                                        ()
                                        (traceableTrait (TIntrinsic IInt32))
                                        ( Map.fromList
                                            [
                                              ( "trace"
                                              , EVariable () (Label (TIntrinsic IInt32 `TArrow` TIntrinsic IString) "trace__$instance.c847f12006235dc0")
                                              )
                                            ]
                                        )
                                        Nothing
                                        <| ERecord
                                          ()
                                          (traceableTrait (TIntrinsic IString))
                                          ( Map.fromList
                                              [
                                                ( "trace"
                                                , EVariable () (Label (TIntrinsic IString `TArrow` TIntrinsic IString) "trace__$instance.c81d5162b7d14248")
                                                )
                                              ]
                                          )
                                          Nothing
                                        :| []
                                    )
                                )
                              ]
                          )
                          Nothing
                          :| []
                      )
                  )
                ]
            )
            Nothing
            :| []
        )
    )
    ( EListLiteral
        ()
        (TIntrinsic (IList (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]))))
        [ ETuple
            ()
            (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]))
            ( ELiteral () (LInt32 1)
                <| ELiteral () (LString "a")
                :| []
            )
        , ETuple
            ()
            (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]))
            ( ELiteral () (LInt32 2)
                <| ELiteral () (LString "b")
                :| []
            )
        ]
        :| []
    )

fixturee12 :: Expression () (Type TypeIndex Kind)
fixturee12 = fst $ runTraitTransformY (transformScope fixturee10)
