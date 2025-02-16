{-# LANGUAGE OverloadedStrings #-}

module Noll.Compiler.Transform.Type.AliasInsertionSpec where

import Control.Monad.Reader (runReader)
import Noll.Common.List1 (NonEmpty (..), (<|))
import Noll.Compiler.Transform.Type.AliasInsertion
import Noll.Examples.Test01 (test01)
import Noll.Examples.Test02 (test02)
import Noll.Label (Label (..))
import Noll.Language (
  BinaryOperator (..),
  Expression (..),
  Intrinsic (..),
  Parameter (..),
  Pattern (..),
  Row (..),
  Trait (..),
  Type (..),
  Uses (..),
 )
import Noll.Module (Constant (..), Definition (..), Function (..), Module (..))
import Test.Hspec (Spec, describe, it)

import qualified Noll.Common.Environment as Environment

spec :: Spec
spec =
  describe "Lime.Compiler.ExpandAliases" $ do
    it "" $
      runReader (insertAliases type1) testEnvironment == result1
    it "" $
      runReader (insertAliases object1) testEnvironment == result2
    it "" $
      runReader (insertAliases test01) testEnvironment == test02

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
    ( Uses
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
            (Uses [] ())
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
    ( Uses
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
            (Uses [] ())
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
