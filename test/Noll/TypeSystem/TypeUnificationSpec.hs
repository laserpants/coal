module Noll.TypeSystem.TypeUnificationSpec where

import Control.Monad.Except (runExcept)
import Noll.Language (Intrinsic (..), Kind (..), KindIndex (..), Type (..), TypeIndex (..))
import Noll.TypeSystem.TypeSubstitution (TypeSubstitution (..), mapsToType)
import Noll.TypeSystem.TypeUnification (TypeUnifiable (..))
import Noll.TypeSystem.TypeUnification.Error (UnificationError (..))
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "Noll.TypeSystem.TypeUnification" $ do
    it "'0 ~ '0" $ do
      validateResult fixture1 fixture1 == Right mempty
    it "'0 ~ int32" $ do
      validateResult fixture1 (TIntrinsic IInt32) == Right (0 `mapsToType` TIntrinsic IInt32)
    it "int32 ~ '0" $ do
      validateResult (TIntrinsic IInt32) fixture1 == Right (0 `mapsToType` TIntrinsic IInt32)
    it "int32 ~ int32" $ do
      validateResult (TIntrinsic IInt32 :: Type TypeIndex (Kind KindIndex)) (TIntrinsic IInt32) == Right mempty

fixture1 :: Type TypeIndex (Kind KindIndex)
fixture1 = TVariable (TypeIndex KType 0)

validateResult :: (TypeUnifiable a) => a -> a -> Either UnificationError TypeSubstitution
validateResult t1 t2 = runExcept (unify t1 t2)
