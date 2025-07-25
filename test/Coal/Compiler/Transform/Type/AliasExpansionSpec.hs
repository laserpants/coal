{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Transform.Type.AliasExpansionSpec where

import Control.Monad.Reader (runReader)
import Coal.Common.List1 (NonEmpty (..), (<|))
import Coal.Common.Label (Label (..))
import Coal.Compiler.Transform.Type.AliasExpansion
import Coal.Examples.Test01 (test01)
import Coal.Examples.Test02 (test02)
import Coal.Language (
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
import Coal.Language.Module (Constant (..), Definition (..), Function (..), Module (..))
import Test.Hspec (Spec, describe, it)

import qualified Coal.Common.Environment as Environment
import qualified Coal.Set.Test01
import qualified Coal.Set.Test02

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
      runReader (expandAliases Coal.Set.Test01.prog1_01) testEnvironment2 == Coal.Set.Test02.prog1_02

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
