{-# LANGUAGE OverloadedStrings #-}

module Noll.Compiler.Transform.Type.AliasInsertionSpec where

import Control.Monad.Reader (runReader)
import Noll.Common.List1 (NonEmpty (..))
import Noll.Compiler.Transform.Type.AliasInsertion
import Noll.Language (Intrinsic (..), Parameter (..), Row (..), Type (..))
import Test.Hspec (Spec, describe, it)

import qualified Noll.Common.Environment as Environment

spec :: Spec
spec =
  describe "Lime.Compiler.ExpandAliases" $ do
    it "" $
      runReader (insertAliases type1) testEnvironment == result1

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
