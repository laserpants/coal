{-# LANGUAGE OverloadedStrings #-}

module Noll.Compiler.Transform.Type.AliasExpansionSpec where

import Control.Monad.Reader (runReader)
import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Common.Label (Label (..))
import Noll.Compiler.Transform.Type.AliasExpansion
import Noll.Examples.Test01 (test01)
import Noll.Examples.Test02 (test02)
import Noll.Language (
  BinaryOperator (..),
  Expression (..),
  Intrinsic (..),
  Parameter (..),
  Pattern (..),
  Row (..),
  Trait (..),
  Type (..),
  With (..),
 )
import Noll.Language.Module (Constant (..), Definition (..), Function (..), Module (..))
import Test.Hspec (Spec, describe, it)

import qualified Lang.Common.Environment as Environment
import qualified Noll.Set.Test01
import qualified Noll.Set.Test02

spec :: Spec
spec =
  describe "Lime.Compiler.ExpandAliases" $ do
    it "" $
      runReader (expandAliases type1) testEnvironment == result1
    it "" $
      runReader (expandAliases object1) testEnvironment == result2
    it "" $
      runReader (expandAliases test01) testEnvironment == test02
    it "" $
      runReader (expandAliases Noll.Set.Test01.prog1_01) testEnvironment2 == Noll.Set.Test02.prog1_02

type1 :: Type Parameter ()
type1 =
  TApplication
    ()
    (TConstructor () "Predicate")
    (TIntrinsic IInt32 :| [])

result1 :: Type Parameter ()
result1 =
  TAlias
    "Predicate"
    [TIntrinsic IInt32]
    (TIntrinsic IInt32 `TArrow` TIntrinsic IBool)

object1 :: Definition () () ()
object1 =
  DAnnotation
    ( With
        [Trait "Ordered" (TVariable (Parameter () "a"))]
        ( TApplication
            ()
            (TConstructor () "Predicate")
            ( TVariable (Parameter () "a")
                :| []
            )
        )
    )
    ( DFunction
        "greater_than"
        ( Function
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
        )
    )

result2 :: Definition () () ()
result2 =
  DAnnotation
    ( With
        [Trait "Ordered" (TVariable (Parameter () "a"))]
        ( TAlias
            "Predicate"
            [TVariable (Parameter () "a")]
            (TVariable (Parameter () "a") `TArrow` TIntrinsic IBool)
        )
    )
    ( DFunction
        "greater_than"
        ( Function
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
        )
    )

testEnvironment :: AliasEnvironment
testEnvironment =
  Environment.fromList
    [
      ( "Predicate"
      ,
        ( ["a"]
        , TVariable (Parameter () "a") `TArrow` TIntrinsic IBool
        )
      )
    ,
      ( "Range"
      ,
        ( ["a"]
        , ( TIntrinsic
              ( IRecord
                  (TRow (RExtend "min" (TVariable (Parameter () "a")) (RExtend "max" (TVariable (Parameter () "a")) RNil)))
              )
          )
        )
      )
    ]

testEnvironment2 :: AliasEnvironment
testEnvironment2 =
  Environment.fromList
    [
      ( "Predicate"
      ,
        ( ["a"]
        , TVariable (Parameter () "a") `TArrow` TIntrinsic IBool
        )
      )
    ,
      ( "Range"
      ,
        ( ["a"]
        , ( TIntrinsic
              ( IRecord
                  ( TRow
                      ( RExtend
                          "max"
                          (TVariable (Parameter () "a"))
                          ( RExtend
                              "min"
                              (TVariable (Parameter () "a"))
                              RNil
                          )
                      )
                  )
              )
          )
        )
      )
    ]
