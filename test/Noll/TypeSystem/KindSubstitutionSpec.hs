{-# LANGUAGE OverloadedStrings #-}

module Noll.TypeSystem.KindSubstitutionSpec (spec) where

import Data.List.NonEmpty (NonEmpty (..))
import Noll.Label (Label (..))
import Noll.Language (Binding (..), Expression (..), Intrinsic (..), Kind (..), KindIndex (..), Pattern (..), Primitive (..), Type (..), TypeIndex (..))
import Noll.TypeSystem.KindSubstitution (applyKindSub, mapsToKind)
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "Noll.TypeSystem.KindSubstitution" $ do
    describe "applyKindSub" $ do
      it "" $ do
        let t = TVariable (TypeIndex (KVariable (KindIndex 1)) 1) :: Type TypeIndex (Kind KindIndex)
        applyKindSub (1 `mapsToKind` KType) t == TVariable (TypeIndex KType 1)
      it "" $ do
        let t = TVariable (TypeIndex (KVariable (KindIndex 1)) 1) :: Type TypeIndex (Kind KindIndex)
        applyKindSub (2 `mapsToKind` KType) t == t
      it "" $
        applyKindSub (3 `mapsToKind` KType) fixture1 == fixture1Result

fixture1 :: Expression () (Type TypeIndex (Kind KindIndex))
fixture1 =
  ELet
    ()
    ( BPattern
        ()
        (PVariable () (Label (TVariable (TypeIndex (KVariable (KindIndex 3)) 3) `TArrow` TVariable (TypeIndex (KVariable (KindIndex 3)) 3)) "f"))
        ( ELambda
            ()
            (PVariable () (Label (TVariable (TypeIndex (KVariable (KindIndex 3)) 3)) "x") :| [])
            (EVariable () (Label (TVariable (TypeIndex (KVariable (KindIndex 3)) 3)) "x"))
        )
        :| []
    )
    ( EApplication
        ()
        (TIntrinsic IInt32)
        ( EApplication
            ()
            (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32)
            (EVariable () (Label ((TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) "f"))
            (EVariable () (Label (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) "f") :| [])
        )
        ( EApplication
            ()
            (TIntrinsic IInt32)
            (EVariable () (Label (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) "f"))
            (ELiteral () (LInt32 1) :| [])
            :| []
        )
    )

fixture1Result :: Expression () (Type TypeIndex (Kind KindIndex))
fixture1Result =
  ( ELet
      ()
      ( BPattern
          ()
          (PVariable () (Label (TVariable (TypeIndex KType 3) `TArrow` TVariable (TypeIndex KType 3)) "f"))
          ( ELambda
              ()
              (PVariable () (Label (TVariable (TypeIndex KType 3)) "x") :| [])
              (EVariable () (Label (TVariable (TypeIndex KType 3)) "x"))
          )
          :| []
      )
      ( EApplication
          ()
          (TIntrinsic IInt32)
          ( EApplication
              ()
              (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32)
              (EVariable () (Label ((TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) "f"))
              (EVariable () (Label (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) "f") :| [])
          )
          ( EApplication
              ()
              (TIntrinsic IInt32)
              (EVariable () (Label (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) "f"))
              (ELiteral () (LInt32 1) :| [])
              :| []
          )
      )
  )
