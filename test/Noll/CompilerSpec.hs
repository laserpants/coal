{-# LANGUAGE OverloadedStrings #-}

module Noll.CompilerSpec where

import Control.Monad.Identity (runIdentity)
import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Noll.Compiler
import Noll.CompilerExamples.Test02 (bazz)
import Noll.Language
import Noll.Module (Constant (..), Definition (..), Function (..), Module (..))
import Noll.SystemF
import Noll.SystemFSpec.TestRunner
import Test.Hspec (Spec, describe, it)

import qualified Data.Set as Set
import qualified Lang.Common.Environment as Environment

spec :: Spec
spec =
  describe "Noll.Compiler" $ do
    it "" $ do
      1 == 2

foo =
  runIdentity $
    runCompilerT
      ( CompilerEnvironment
          ( Environment.fromList
              [
                ( "LessThan"
                , Constructor
                    "LessThan"
                    0
                    (Forall mempty [] (TConstructor KType "Ordering"))
                )
              ,
                ( "GreaterThan"
                , Constructor
                    "GreaterThan"
                    0
                    (Forall mempty [] (TConstructor KType "Ordering"))
                )
              ,
                ( "EqualTo"
                , Constructor
                    "EqualTo"
                    0
                    (Forall mempty [] (TConstructor KType "Ordering"))
                )
              ]
          )
          ( Environment.fromList
              []
          )
          ( Environment.fromList
              []
          )
      )
      baz

baz :: (Monad m) => CompilerT () m (Function Expression () IndexedType, [CompilerAssumption])
baz = do
  insertNamesC
    [
      ( "compare"
      , Forall
          (Set.fromList [TypeIndex KType 0])
          []
          ( TVariable (TypeIndex KType 0)
              `TArrow` TVariable (TypeIndex KType 0)
              `TArrow` TConstructor KType "Ordering"
          )
      )
    ,
      ( "not"
      , Forall
          mempty
          []
          (TIntrinsic IBool `TArrow` TIntrinsic IBool)
      )
    ,
      ( "less_than_or_equal_to"
      , Forall
          (Set.fromList [TypeIndex KType 0])
          []
          ( TVariable (TypeIndex KType 0)
              `TArrow` TVariable (TypeIndex KType 0)
              `TArrow` TIntrinsic IBool
          )
      )
    ]
  e <-
    indexedC $
      Function
        ()
        (With [] ())
        ( PAnnotation
            ()
            (TVariable (Parameter () "a"))
            (PVariable () (Label () "n"))
            :| []
        )
        ( EApplication
            ()
            ()
            (EBinaryOperator () () OReverseComposition)
            ( EVariable () (Label () "not")
                <| EApplication
                  ()
                  ()
                  (EVariable () (Label () "less_than_or_equal_to"))
                  (EVariable () (Label () "n") :| [])
                :| []
            )
        )
  (xx, yy) <- typeCheckFunctionC e
  pure (normalizeTypeIndexes xx, yy)

bark :: Definition () k ()
bark =
  DInstance
    "Ordered"
    (TIntrinsic IInt32)
    [ DFunction
        "compare"
        ( Function
            ()
            (With [] ())
            ( PVariable () (Label () "x")
                <| PVariable () (Label () "y")
                :| []
            )
            ( EIf
                ()
                ()
                ( EApplication
                    ()
                    ()
                    (EBinaryOperator () () OLessThan)
                    ( EVariable () (Label () "x")
                        <| EVariable () (Label () "y")
                        :| []
                    )
                )
                (EConstructor () (Label () "LessThan"))
                ( EIf
                    ()
                    ()
                    ( EApplication
                        ()
                        ()
                        (EBinaryOperator () () OGreaterThan)
                        ( EVariable () (Label () "x")
                            <| EVariable () (Label () "y")
                            :| []
                        )
                    )
                    (EConstructor () (Label () "GreaterThan"))
                    (EConstructor () (Label () "EqualTo"))
                )
            )
        )
    ]

bark2 :: Definition () k IndexedType
bark2 =
  DInstance
    "Ordered"
    (TIntrinsic IInt32)
    [ DFunction
        "compare"
        ( Function
            ()
            (With [] (TConstructor KType "Ordering"))
            ( PVariable () (Label (TIntrinsic IInt32) "x")
                <| PVariable () (Label (TIntrinsic IInt32) "y")
                :| []
            )
            ( EIf
                ()
                (TConstructor KType "Ordering")
                ( EApplication
                    ()
                    (TIntrinsic IBool)
                    (EBinaryOperator () (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IBool) OLessThan)
                    ( EVariable () (Label (TIntrinsic IInt32) "x")
                        <| EVariable () (Label (TIntrinsic IInt32) "y")
                        :| []
                    )
                )
                (EConstructor () (Label (TConstructor KType "Ordering") "LessThan"))
                ( EIf
                    ()
                    (TConstructor KType "Ordering")
                    ( EApplication
                        ()
                        (TIntrinsic IBool)
                        (EBinaryOperator () (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IBool) OGreaterThan)
                        ( EVariable () (Label (TIntrinsic IInt32) "x")
                            <| EVariable () (Label (TIntrinsic IInt32) "y")
                            :| []
                        )
                    )
                    (EConstructor () (Label (TConstructor KType "Ordering") "GreaterThan"))
                    (EConstructor () (Label (TConstructor KType "Ordering") "EqualTo"))
                )
            )
        )
    ]

bazz2 =
  bazz [bark]
