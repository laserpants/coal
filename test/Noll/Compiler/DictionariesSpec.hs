{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Compiler.DictionariesSpec where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Data.Generics.Uniplate.Data (transformM)
import Data.Map.Strict (Map)
import Data.Set (Set)
import qualified Data.Set as Set
import Lang.Common.Environment (Environment (..))
import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Lang.Utils (Dictionary, Name)
import Noll.Compiler.Dictionaries
import Noll.Language
import Noll.Module
import Test.Hspec (Spec, describe, it)

import qualified Data.Map.Strict as Map
import qualified Lang.Common.Environment as Environment

spec :: Spec
spec = do
  undefined

fixtured1 =
  undefined

-- runTraitTransformY :: (Monoid b) => ReaderT DictionaryEnvironment (StateT Int (Writer b)) a -> a
-- runTraitTransformY v = fst $ runWriter (evalStateT (runReaderT v testEnv) 200) -- (freshIdIn v))

-- runTraitTransformY2 :: (Monoid b) => Int -> ReaderT DictionaryEnvironment (StateT Int (Writer b)) a -> a
runTraitTransformY2 n v = fst (runDictionaryStack testEnv n v) -- fst $ runWriter (evalStateT (runReaderT v testEnv) n)

testEnv = DictionaryEnvironment mempty xx

xx :: Environment (Map IndexedType (Dictionary (Scheme TypeIndex Kind IndexedType)))
xx =
  Environment.fromList
    [
      ( "Numeric"
      , Map.fromList
          [
            ( TIntrinsic IInt32
            , Map.fromList
                [
                  ( "from_int32"
                  , Forall mempty [] (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32)
                  )
                ]
            )
          ,
            ( TIntrinsic INat
            , Map.fromList
                [
                  ( "from_int32"
                  , Forall mempty [] (TIntrinsic IInt32 `TArrow` TIntrinsic INat)
                  )
                ]
            )
          ]
      )
    ,
      ( "Ordered"
      , Map.fromList
          [
            ( TIntrinsic IInt32
            , Map.fromList
                [
                  ( "compare"
                  , Forall (Set.fromList [TypeIndex KType 0]) [] (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TConstructor KType "Ordering")
                  )
                ]
            )
          ]
      )
      --    ,
      --      ( "Traceable"
      --      , Map.fromList
      --          [
      --            ( TIntrinsic IString
      --            , Map.fromList
      --                [
      --                  ( "trace"
      --                  , Forall mempty [] (TIntrinsic IString `TArrow` TIntrinsic IString)
      --                  )
      --                ]
      --            )
      --          ,
      --            ( TIntrinsic IInt32
      --            , Map.fromList
      --                [
      --                  ( "trace"
      --                  , Forall mempty [] (TIntrinsic IInt32 `TArrow` TIntrinsic IString)
      --                  )
      --                ]
      --            )
      --          ,
      --            ( TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)])
      --            , Map.fromList
      --                [
      --                  ( "trace"
      --                  , Forall
      --                      (Set.fromList [TypeIndex KType 0, TypeIndex KType 1])
      --                      [ Trait "Traceable" (TVariable (TypeIndex KType 0))
      --                      , Trait "Traceable" (TVariable (TypeIndex KType 1))
      --                      ]
      --                      (TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)]) `TArrow` TIntrinsic IString)
      --                  )
      --                ]
      --            )
      --          ,
      --            ( TIntrinsic (IList (TVariable (TypeIndex KType 0)))
      --            , Map.fromList
      --                [
      --                  ( "trace"
      --                  , Forall
      --                      (Set.fromList [TypeIndex KType 0])
      --                      [ Trait "Traceable" (TVariable (TypeIndex KType 0))
      --                      ]
      --                      (TIntrinsic (IList (TVariable (TypeIndex KType 0))) `TArrow` TIntrinsic IString)
      --                  )
      --                ]
      --            )
      --          ]
      --      )
    ]

-- yy :: Environment (Scheme TypeIndex Kind (Type TypeIndex Kind))
-- yy =
--  Environment.fromList
--    [
--      ( "trace"
--      , Forall
--          (Set.fromList [TypeIndex KType 0] :: Set (TypeIndex Kind))
--          [Trait "Traceable" (TVariable (TypeIndex KType 0))]
--          (TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IString)
--      )
--    ,
--      ( "pair_to_string"
--      , Forall
--          (Set.fromList [TypeIndex KType 0, TypeIndex KType 1] :: Set (TypeIndex Kind))
--          [ Trait "Traceable" (TVariable (TypeIndex KType 0))
--          , Trait "Traceable" (TVariable (TypeIndex KType 1))
--          ]
--          (TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)]) `TArrow` TIntrinsic IString)
--      )
--    ,
--      ( "list_to_string"
--      , Forall
--          (Set.fromList [TypeIndex KType 0] :: Set (TypeIndex Kind))
--          [ Trait "Traceable" (TVariable (TypeIndex KType 0))
--          ]
--          (TIntrinsic (IList (TVariable (TypeIndex KType 0))) `TArrow` TIntrinsic IString)
--      )
--    ,
--      ( "from_int32"
--      , Forall
--          (Set.fromList [TypeIndex KType 0] :: Set (TypeIndex Kind))
--          [Trait "Numeric" (TVariable (TypeIndex KType 0))]
--          (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 0))
--      )
--    ,
--      ( "greater_than"
--      , Forall
--          (Set.fromList [TypeIndex KType 0] :: Set (TypeIndex Kind))
--          [Trait "Ordered" (TVariable (TypeIndex KType 0))]
--          (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool)
--      )
--    ,
--      ( "less_than_or_equal_to"
--      , Forall
--          (Set.fromList [TypeIndex KType 0] :: Set (TypeIndex Kind))
--          [Trait "Ordered" (TVariable (TypeIndex KType 0))]
--          (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool)
--      )
--    ,
--      ( "compare"
--      , Forall
--          (Set.fromList [TypeIndex KType 0] :: Set (TypeIndex Kind))
--          [Trait "Ordered" (TVariable (TypeIndex KType 0))]
--          (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TConstructor KType "Ordering")
--      )
--    ,
--      ( "from_list"
--      , Forall
--          (Set.fromList [TypeIndex KType 0] :: Set (TypeIndex Kind))
--          [Trait "Numeric" (TVariable (TypeIndex KType 0)), Trait "Ordered" (TVariable (TypeIndex KType 0))]
--          (TIntrinsic (IList (TVariable (TypeIndex KType 0))) `TArrow` (TApplication KType (TConstructor (KArrow KType KType) "Tree") (TVariable (TypeIndex KType 0) :| [])))
--      )
--    ,
--      ( "in_range"
--      , Forall
--          (Set.fromList [TypeIndex KType 0] :: Set (TypeIndex Kind))
--          [Trait "Numeric" (TVariable (TypeIndex KType 0)), Trait "Ordered" (TVariable (TypeIndex KType 0))]
--          ( TIntrinsic (IRecord (TRow (RExtend "max" (TVariable (TypeIndex KType 0)) (RExtend "min" (TVariable (TypeIndex KType 0)) RNil))))
--              `TArrow` TVariable (TypeIndex KType 0)
--              `TArrow` TIntrinsic IBool
--          )
--      )
--    ,
--      ( "sort"
--      , Forall
--          (Set.fromList [TypeIndex KType 0] :: Set (TypeIndex Kind))
--          [Trait "Numeric" (TVariable (TypeIndex KType 0)), Trait "Ordered" (TVariable (TypeIndex KType 0))]
--          (TIntrinsic (IList (TVariable (TypeIndex KType 0))) `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 0))))
--      )
--    ]

-- zz :: Map IndexedType (Map String (Function Expression () IndexedType))
-- zz =
--  Map.fromList
--    [
--      ( TIntrinsic IString
--      , Map.fromList
--          [
--            ( "trace"
--            , Function
--                ()
--                (With [] (TIntrinsic IString))
--                (PVariable () (Label (TIntrinsic IString) "s") :| [])
--                (EVariable () (Label (TIntrinsic IString) "s"))
--            )
--          ]
--      )
--    ,
--      ( TIntrinsic IInt32
--      , Map.fromList
--          [
--            ( "trace"
--            , Function
--                ()
--                (With [] (TIntrinsic IString))
--                (PVariable () (Label (TIntrinsic IInt32) "n") :| [])
--                ( EApplication
--                    ()
--                    (TIntrinsic IString)
--                    (EVariable () (Label (TIntrinsic IInt32 `TArrow` TIntrinsic IString) "int32_to_string"))
--                    (EVariable () (Label (TIntrinsic IInt32) "n") :| [])
--                )
--            )
--          ]
--      )
--    ,
--      ( TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)])
--      , Map.fromList
--          [
--            ( "trace"
--            , Function
--                ()
--                ( With
--                    [ Trait "Traceable" (TVariable (TypeIndex KType 0))
--                    , Trait "Traceable" (TVariable (TypeIndex KType 1))
--                    ]
--                    (TIntrinsic IString)
--                )
--                (PVariable () (Label (TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)])) "p") :| [])
--                ( EApplication
--                    ()
--                    (TIntrinsic IString)
--                    (EVariable () (Label (TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)]) `TArrow` TIntrinsic IString) "pair_to_string"))
--                    (EVariable () (Label (TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)])) "p") :| [])
--                )
--            )
--          ]
--      )
--    ,
--      ( TIntrinsic (IList (TVariable (TypeIndex KType 0)))
--      , Map.fromList
--          [
--            ( "trace"
--            , Function
--                ()
--                ( With
--                    [ Trait "Traceable" (TVariable (TypeIndex KType 0))
--                    ]
--                    (TIntrinsic IString)
--                )
--                (PVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 0)))) "lst") :| [])
--                ( EApplication
--                    ()
--                    (TIntrinsic IString)
--                    (EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 0))) `TArrow` TIntrinsic IString) "list_to_string"))
--                    (EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 0)))) "lst") :| [])
--                )
--            )
--          ]
--      )
--    ]

fixtured2 = runTraitTransformY2 0 (collectTraits (TIntrinsic IInt32) "trace")

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
    ( PPlaceholder () (traceableTrait (TVariable (TypeIndex KType 0))) (Trait "Traceable" (TVariable (TypeIndex KType 0)))
        <| PPlaceholder () (traceableTrait (TVariable (TypeIndex KType 1))) (Trait "Traceable" (TVariable (TypeIndex KType 1)))
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
                ( EPlaceholder () (traceableTrait (TVariable (TypeIndex KType 0))) (Trait "Traceable" (TVariable (TypeIndex KType 0)))
                    <| EPlaceholder () (traceableTrait (TVariable (TypeIndex KType 1))) (Trait "Traceable" (TVariable (TypeIndex KType 1)))
                    :| []
                )
            )
            ( EVariable () (Label (TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)])) "p")
                :| []
            )
        )
    )

fixturee3 = fst $ runTraitTransformY2 (freshIdIn fixturee1) (transformScope fixturee1)

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
    ( PPlaceholder () (traceableTrait (TVariable (TypeIndex KType 0))) (Trait "Traceable" (TVariable (TypeIndex KType 0)))
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
                ( EPlaceholder () (traceableTrait (TVariable (TypeIndex KType 0))) (Trait "Traceable" (TVariable (TypeIndex KType 0)))
                    :| []
                )
            )
            (EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 0)))) "lst") :| [])
        )
    )

fixturee6 = fst $ runTraitTransformY2 (freshIdIn fixturee4) (transformScope fixturee4)

fixturee7 :: Expression () (Type TypeIndex Kind)
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
                                  "trace__$instance_Traceable(Intrinsic(Tuple(Variable(TypeIndex(0)),Variable(TypeIndex(1)))))"
                              )
                          )
                          ( ERecord
                              ()
                              (traceableTrait (TIntrinsic IInt32))
                              ( Map.fromList
                                  [
                                    ( "trace"
                                    , EVariable () (Label (TIntrinsic IInt32 `TArrow` TIntrinsic IString) "trace__$instance_Traceable(Intrinsic(Int32))")
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
                                      , EVariable () (Label (TIntrinsic IString `TArrow` TIntrinsic IString) "trace__$instance_Traceable(Intrinsic(String))")
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
fixturee9 = fst $ runTraitTransformY2 (freshIdIn fixturee7) (transformScope fixturee7)

fixturee10 :: Expression () (Type TypeIndex Kind)
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
                              "trace__$instance_Traceable(Intrinsic(List(Variable(TypeIndex(0)))))"
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
                                            "trace__$instance_Traceable(Intrinsic(Tuple(Variable(TypeIndex(0)),Variable(TypeIndex(1)))))"
                                        )
                                    )
                                    ( ERecord
                                        ()
                                        (traceableTrait (TIntrinsic IInt32))
                                        ( Map.fromList
                                            [
                                              ( "trace"
                                              , EVariable () (Label (TIntrinsic IInt32 `TArrow` TIntrinsic IString) "trace__$instance_Traceable(Intrinsic(Int32))")
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
                                                , EVariable () (Label (TIntrinsic IString `TArrow` TIntrinsic IString) "trace__$instance_Traceable(Intrinsic(String))")
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
fixturee12 = fst $ runTraitTransformY2 (freshIdIn fixturee10) (transformScope fixturee10)

fixturee13 =
  Constant
    ()
    (With [] (TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)]) `TArrow` TIntrinsic IString))
    ( ELambda
        ()
        (PVariable () (Label (TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)])) "p") :| [])
        ( EApplication
            ()
            (TIntrinsic IString)
            (EVariable () (Label (TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)]) `TArrow` TIntrinsic IString) "pair_to_string"))
            (EVariable () (Label (TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)])) "p") :| [])
        )
    )

fixturee14 =
  Constant
    ()
    ( With
        [ Trait "Traceable" (TVariable (TypeIndex KType 0))
        , Trait "Traceable" (TVariable (TypeIndex KType 1))
        ]
        (TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)]) `TArrow` TIntrinsic IString)
    )
    ( ELambda
        ()
        ( PPlaceholder () (traceableTrait (TVariable (TypeIndex KType 0))) (Trait "Traceable" (TVariable (TypeIndex KType 0)))
            <| PPlaceholder () (traceableTrait (TVariable (TypeIndex KType 1))) (Trait "Traceable" (TVariable (TypeIndex KType 1)))
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
                    ( EPlaceholder () (traceableTrait (TVariable (TypeIndex KType 0))) (Trait "Traceable" (TVariable (TypeIndex KType 0)))
                        <| EPlaceholder () (traceableTrait (TVariable (TypeIndex KType 1))) (Trait "Traceable" (TVariable (TypeIndex KType 1)))
                        :| []
                    )
                )
                ( EVariable () (Label (TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)])) "p")
                    :| []
                )
            )
        )
    )

fixturee15 :: Constant Expression () (Type TypeIndex Kind)
fixturee15 = runTraitTransformY2 (freshIdIn fixturee13) (expandTraits fixturee13)

fixturee16 =
  Constant
    ()
    (With [] (TIntrinsic (IList (TVariable (TypeIndex KType 0))) `TArrow` TIntrinsic IString))
    ( ELambda
        ()
        (PVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 0)))) "lst") :| [])
        ( EApplication
            ()
            (TIntrinsic IString)
            (EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 0))) `TArrow` TIntrinsic IString) "list_to_string"))
            (EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 0)))) "lst") :| [])
        )
    )

fixturee17 =
  Constant
    ()
    ( With
        [ Trait "Traceable" (TVariable (TypeIndex KType 0))
        ]
        (TIntrinsic (IList (TVariable (TypeIndex KType 0))) `TArrow` TIntrinsic IString)
    )
    ( ELambda
        ()
        ( PPlaceholder () (traceableTrait (TVariable (TypeIndex KType 0))) (Trait "Traceable" (TVariable (TypeIndex KType 0)))
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
                    ( EPlaceholder () (traceableTrait (TVariable (TypeIndex KType 0))) (Trait "Traceable" (TVariable (TypeIndex KType 0)))
                        :| []
                    )
                )
                (EVariable () (Label (TIntrinsic (IList (TVariable (TypeIndex KType 0)))) "lst") :| [])
            )
        )
    )

fixturee18 :: Constant Expression () (Type TypeIndex Kind)
fixturee18 = runTraitTransformY2 (freshIdIn fixturee16) (expandTraits fixturee16)

fixturee19 :: Constant Expression () (Type TypeIndex Kind)
fixturee19 =
  Constant
    ()
    (With [] (TIntrinsic IString))
    ( ELet
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
    )

fixturee20 :: Constant Expression () (Type TypeIndex Kind)
fixturee20 =
  Constant
    ()
    (With [] (TIntrinsic IString))
    ( ELet
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
                                      "trace__$instance_Traceable(Intrinsic(Tuple(Variable(TypeIndex(0)),Variable(TypeIndex(1)))))"
                                  )
                              )
                              ( ERecord
                                  ()
                                  (traceableTrait (TIntrinsic IInt32))
                                  ( Map.fromList
                                      [
                                        ( "trace"
                                        , EVariable () (Label (TIntrinsic IInt32 `TArrow` TIntrinsic IString) "trace__$instance_Traceable(Intrinsic(Int32))")
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
                                          , EVariable () (Label (TIntrinsic IString `TArrow` TIntrinsic IString) "trace__$instance_Traceable(Intrinsic(String))")
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
    )

fixturee21 :: Constant Expression () (Type TypeIndex Kind)
fixturee21 = runTraitTransformY2 (freshIdIn fixturee19) (expandTraits fixturee19)

fixturee22 :: Constant Expression () (Type TypeIndex Kind)
fixturee22 =
  Constant
    ()
    (With [] (TIntrinsic IString))
    ( EApplication
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
    )

fixturee23 :: Constant Expression () (Type TypeIndex Kind)
fixturee23 =
  Constant
    ()
    (With [] (TIntrinsic IString))
    ( EApplication
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
                      , -- EApplication
                        --  ()
                        --  (TIntrinsic (IList (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]))) `TArrow` TIntrinsic IString)
                        --  (EVariable () (Label (traceableTrait (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString])) `TArrow` TIntrinsic (IList (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]))) `TArrow` TIntrinsic IString) "trace"))
                        --  ( ERecord
                        --      ()
                        --      (traceableTrait (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString])))
                        --      ( Map.fromList
                        --          [
                        --            ( "trace"
                        --            ,
                        EApplication
                          ()
                          (TIntrinsic (IList (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]))) `TArrow` TIntrinsic IString)
                          ( EVariable
                              ()
                              ( Label
                                  ( traceableTrait (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString]))
                                      `TArrow` TIntrinsic (IList (TIntrinsic (ITuple [TIntrinsic IInt32, TIntrinsic IString])))
                                      `TArrow` TIntrinsic IString
                                  )
                                  "trace__$instance_Traceable(Intrinsic(List(Variable(TypeIndex(0)))))"
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
                                                "trace__$instance_Traceable(Intrinsic(Tuple(Variable(TypeIndex(0)),Variable(TypeIndex(1)))))"
                                            )
                                        )
                                        ( ERecord
                                            ()
                                            (traceableTrait (TIntrinsic IInt32))
                                            ( Map.fromList
                                                [
                                                  ( "trace"
                                                  , EVariable () (Label (TIntrinsic IInt32 `TArrow` TIntrinsic IString) "trace__$instance_Traceable(Intrinsic(Int32))")
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
                                                    , EVariable () (Label (TIntrinsic IString `TArrow` TIntrinsic IString) "trace__$instance_Traceable(Intrinsic(String))")
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
                          --                                            )
                          --                                          ]
                          --                                      )
                          --                                      Nothing
                          --                                      :| []
                          --                                  )
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
    )

fixturee24 :: Constant Expression () (Type TypeIndex Kind)
fixturee24 = runTraitTransformY2 (freshIdIn fixturee22) (expandTraits fixturee22)

fixturee25 :: Expression () (Type TypeIndex Kind)
fixturee25 =
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

fixturee26 :: Expression () (Type TypeIndex Kind)
fixturee26 =
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
                              "trace__$instance_Traceable(Intrinsic(List(Variable(TypeIndex(0)))))"
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
                                            "trace__$instance_Traceable(Intrinsic(Tuple(Variable(TypeIndex(0)),Variable(TypeIndex(1)))))"
                                        )
                                    )
                                    ( ERecord
                                        ()
                                        (traceableTrait (TIntrinsic IInt32))
                                        ( Map.fromList
                                            [
                                              ( "trace"
                                              , EVariable () (Label (TIntrinsic IInt32 `TArrow` TIntrinsic IString) "trace__$instance_Traceable(Intrinsic(Int32))")
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
                                                , EVariable () (Label (TIntrinsic IString `TArrow` TIntrinsic IString) "trace__$instance_Traceable(Intrinsic(String))")
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

fixturee27 :: Expression () (Type TypeIndex Kind)
fixturee27 = fst $ runTraitTransformY2 (freshIdIn fixturee25) (transformScope fixturee25)

fixturee28 =
  Constant
    ()
    (With [] (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool))
    ( ELambda
        ()
        ( PVariable () (Label (TVariable (TypeIndex KType 0)) "m")
            <| PVariable () (Label (TVariable (TypeIndex KType 0)) "n")
            :| []
        )
        ( ECompiledMatch
            ()
            (TIntrinsic IBool)
            ( EApplication
                ()
                (TConstructor KType "Ordering")
                (EVariable () (Label (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TConstructor KType "Ordering") "compare"))
                ( EVariable () (Label (TVariable (TypeIndex KType 0)) "m")
                    <| EVariable () (Label (TVariable (TypeIndex KType 0)) "n")
                    :| []
                )
            )
            ( ECompiledClause
                (Label (TConstructor KType "Ordering") "EqualTo" :| [])
                (ELiteral () (LBool True))
                <| ECompiledClause
                  (Label (TConstructor KType "Ordering") "GreaterThan" :| [])
                  (ELiteral () (LBool False))
                <| ECompiledClause
                  (Label (TConstructor KType "Ordering") "LessThan" :| [])
                  (ELiteral () (LBool True))
                :| []
            )
        )
    )

fixturee29 =
  Constant
    ()
    ( With
        [ Trait "Ordered" (TVariable (TypeIndex KType 0))
        ]
        (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool)
    )
    ( ELambda
        ()
        ( PPlaceholder () (TApplication KTrait (TConstructor (KType `KArrow` KTrait) "Ordered") (TVariable (TypeIndex KType 0) :| [])) (Trait "Ordered" (TVariable (TypeIndex KType 0)))
            :| []
        )
        ( ELambda
            ()
            ( PVariable () (Label (TVariable (TypeIndex KType 0)) "m")
                <| PVariable () (Label (TVariable (TypeIndex KType 0)) "n")
                :| []
            )
            ( ECompiledMatch
                ()
                (TIntrinsic IBool)
                ( EApplication
                    ()
                    (TConstructor KType "Ordering")
                    ( EApplication
                        ()
                        (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TConstructor KType "Ordering")
                        (EVariable () (Label (TApplication KTrait (TConstructor (KType `KArrow` KTrait) "Ordered") (TVariable (TypeIndex KType 0) :| []) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TConstructor KType "Ordering") "compare"))
                        ( EPlaceholder () (TApplication KTrait (TConstructor (KType `KArrow` KTrait) "Ordered") (TVariable (TypeIndex KType 0) :| [])) (Trait "Ordered" (TVariable (TypeIndex KType 0)))
                            :| []
                        )
                    )
                    ( EVariable () (Label (TVariable (TypeIndex KType 0)) "m")
                        <| EVariable () (Label (TVariable (TypeIndex KType 0)) "n")
                        :| []
                    )
                )
                ( ECompiledClause
                    (Label (TConstructor KType "Ordering") "EqualTo" :| [])
                    (ELiteral () (LBool True))
                    <| ECompiledClause
                      (Label (TConstructor KType "Ordering") "GreaterThan" :| [])
                      (ELiteral () (LBool False))
                    <| ECompiledClause
                      (Label (TConstructor KType "Ordering") "LessThan" :| [])
                      (ELiteral () (LBool True))
                    :| []
                )
            )
        )
    )

fixturee30 :: Constant Expression () (Type TypeIndex Kind)
fixturee30 = runTraitTransformY2 (freshIdIn fixturee28) (expandTraits fixturee28)

fixturee31 =
  Constant
    ()
    (With [] (TVariable (TypeIndex KType 1) `TArrow` TVariable (TypeIndex KType 1) `TArrow` TIntrinsic IBool))
    ( ELambda
        ()
        ( PAnnotation
            ()
            (TVariable (Parameter () "a"))
            (PVariable () (Label (TVariable (TypeIndex KType 1)) "n"))
            :| []
        )
        ( EApplication
            ()
            (TVariable (TypeIndex KType 1) `TArrow` TIntrinsic IBool)
            ( EBinaryOperator
                ()
                ( (TIntrinsic IBool `TArrow` TIntrinsic IBool)
                    `TArrow` (TVariable (TypeIndex KType 1) `TArrow` TIntrinsic IBool)
                    `TArrow` TVariable (TypeIndex KType 1)
                    `TArrow` TIntrinsic IBool
                )
                OReverseComposition
            )
            ( EVariable () (Label (TIntrinsic IBool `TArrow` TIntrinsic IBool) "not")
                <| EApplication
                  ()
                  (TVariable (TypeIndex KType 1) `TArrow` TIntrinsic IBool)
                  (EVariable () (Label (TVariable (TypeIndex KType 1) `TArrow` TVariable (TypeIndex KType 1) `TArrow` TIntrinsic IBool) "less_than_or_equal_to"))
                  (EVariable () (Label (TVariable (TypeIndex KType 1)) "n") :| [])
                :| []
            )
        )
    )

fixturee32 :: Constant Expression () (Type TypeIndex Kind)
fixturee32 =
  Constant
    ()
    ( With
        [ Trait "Ordered" (TVariable (TypeIndex KType 1))
        ]
        (TVariable (TypeIndex KType 1) `TArrow` TVariable (TypeIndex KType 1) `TArrow` TIntrinsic IBool)
    )
    ( ELambda
        ()
        ( PPlaceholder () (TApplication KTrait (TConstructor (KType `KArrow` KTrait) "Ordered") (TVariable (TypeIndex KType 1) :| [])) (Trait "Ordered" (TVariable (TypeIndex KType 1)))
            :| []
        )
        ( ELambda
            ()
            ( PAnnotation
                ()
                (TVariable (Parameter () "a"))
                (PVariable () (Label (TVariable (TypeIndex KType 1)) "n"))
                :| []
            )
            ( EApplication
                ()
                (TVariable (TypeIndex KType 1) `TArrow` TIntrinsic IBool)
                ( EBinaryOperator
                    ()
                    ( (TIntrinsic IBool `TArrow` TIntrinsic IBool)
                        `TArrow` (TVariable (TypeIndex KType 1) `TArrow` TIntrinsic IBool)
                        `TArrow` TVariable (TypeIndex KType 1)
                        `TArrow` TIntrinsic IBool
                    )
                    OReverseComposition
                )
                ( EVariable () (Label (TIntrinsic IBool `TArrow` TIntrinsic IBool) "not")
                    <| EApplication
                      ()
                      (TVariable (TypeIndex KType 1) `TArrow` TIntrinsic IBool)
                      ( EApplication
                          ()
                          (TVariable (TypeIndex KType 1) `TArrow` TVariable (TypeIndex KType 1) `TArrow` TIntrinsic IBool)
                          (EVariable () (Label ((TApplication KTrait (TConstructor (KType `KArrow` KTrait) "Ordered") (TVariable (TypeIndex KType 1) :| [])) `TArrow` TVariable (TypeIndex KType 1) `TArrow` TVariable (TypeIndex KType 1) `TArrow` TIntrinsic IBool) "less_than_or_equal_to"))
                          ( EPlaceholder () (TApplication KTrait (TConstructor (KType `KArrow` KTrait) "Ordered") (TVariable (TypeIndex KType 1) :| [])) (Trait "Ordered" (TVariable (TypeIndex KType 1)))
                              :| []
                          )
                      )
                      (EVariable () (Label (TVariable (TypeIndex KType 1)) "n") :| [])
                    :| []
                )
            )
        )
    )

fixturee33 :: Constant Expression () (Type TypeIndex Kind)
fixturee33 = runTraitTransformY2 (freshIdIn fixturee31) (expandTraits fixturee31)
