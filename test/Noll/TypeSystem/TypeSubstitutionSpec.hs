{-# LANGUAGE OverloadedStrings #-}

module Noll.TypeSystem.TypeSubstitutionSpec where

import Data.List.NonEmpty (NonEmpty (..))
import Noll.Label (Label (..))
import Noll.Language (
  Binding (..),
  Expression (..),
  Intrinsic (..),
  Kind (..),
  KindIndex (..),
  Pattern (..),
  Primitive (..),
  Type (..),
  TypeIndex (..),
 )
import Noll.TypeSystem.TypeSubstitution (
  TypeSubstitution (..),
  apply,
  mapsToType,
  normalizeTypeIndexes,
  typeSubstitutionFromList,
 )
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "Noll.TypeSystem.TypeSubstitution" $ do
    describe "apply" $ do
      it "" $ do
        let t = TVariable (TypeIndex KType 1) :: Type TypeIndex (Kind KindIndex)
        apply (1 `mapsToType` TIntrinsic IBool) t == TIntrinsic IBool
      it "" $ do
        let t = TVariable (TypeIndex KType 0) :: Type TypeIndex (Kind KindIndex)
        apply (1 `mapsToType` TIntrinsic IBool) t == t
      it "" $ validateResult fixture1 fixture1Result
      it "" $ validateResult fixture2 fixture2Result
    describe "normalizeTypeIndexes" $ do
      it "" $
        normalizeTypeIndexes fixture3
          == ( ELambda
                ()
                (PVariable () (Label (typeBool `TArrow` typeVariableKind 3 0) "m") :| [])
                ( ELet
                    ()
                    ( BPattern
                        (PVariable () (Label (typeBool `TArrow` typeVariableKind 3 0) "y"))
                        (EVariable () (Label (typeBool `TArrow` typeVariableKind 3 0) "m"))
                        :| []
                    )
                    ( ELet
                        ()
                        ( BPattern
                            (PVariable () (Label (typeVariableKind 3 0) "x"))
                            ( EApplication
                                ()
                                (typeVariableKind 3 0)
                                (EVariable () (Label (typeBool `TArrow` typeVariableKind 3 0) "y"))
                                (ELiteral () (LBool True) :| [])
                            )
                            :| []
                        )
                        ( EVariable () (Label (typeVariableKind 3 0) "x")
                        )
                    )
                )
             )

validateResult ::
  (Eq a) =>
  (Expression a (Type TypeIndex (Kind KindIndex)), TypeSubstitution) ->
  Expression a (Type TypeIndex (Kind KindIndex)) ->
  Bool
validateResult (e, sub) res = apply sub e == res

fixture1 :: (Expression () (Type TypeIndex (Kind KindIndex)), TypeSubstitution)
fixture1 =
  ( ELambda
      ()
      (PVariable () (Label (typeVariable 5) "m") :| [])
      ( ELet
          ()
          ( BPattern
              (PVariable () (Label (typeVariable 6) "y"))
              (EVariable () (Label (typeVariable 1) "m"))
              :| []
          )
          ( ELet
              ()
              ( BPattern
                  (PVariable () (Label (typeVariable 7) "x"))
                  ( EApplication
                      ()
                      (typeVariable 3)
                      (EVariable () (Label (typeVariable 2) "y"))
                      (ELiteral () (LBool True) :| [])
                  )
                  :| []
              )
              ( EVariable () (Label (typeVariable 4) "x")
              )
          )
      )
  , typeSubstitutionFromList
      [ (1, typeBool `TArrow` typeVariable 3)
      , (2, typeBool `TArrow` typeVariable 3)
      , (4, typeVariable 3)
      , (5, typeBool `TArrow` typeVariable 3)
      , (6, typeBool `TArrow` typeVariable 3)
      , (7, typeVariable 3)
      ]
  )

fixture1Result :: Expression () (Type TypeIndex (Kind KindIndex))
fixture1Result =
  ELambda
    ()
    (PVariable () (Label (typeBool `TArrow` typeVariable 3) "m") :| [])
    ( ELet
        ()
        ( BPattern
            (PVariable () (Label (typeBool `TArrow` typeVariable 3) "y"))
            (EVariable () (Label (typeBool `TArrow` typeVariable 3) "m"))
            :| []
        )
        ( ELet
            ()
            ( BPattern
                (PVariable () (Label (typeVariable 3) "x"))
                ( EApplication
                    ()
                    (typeVariable 3)
                    (EVariable () (Label (typeBool `TArrow` typeVariable 3) "y"))
                    (ELiteral () (LBool True) :| [])
                )
                :| []
            )
            ( EVariable () (Label (typeVariable 3) "x")
            )
        )
    )

fixture2 :: (Expression () (Type TypeIndex (Kind KindIndex)), TypeSubstitution)
fixture2 =
  ( ELet
      ()
      ( BPattern
          (PVariable () (Label (typeVariable 1) "f"))
          ( ELambda
              ()
              (PVariable () (Label (typeVariable 2) "x") :| [])
              (EVariable () (Label (typeVariable 3) "x"))
          )
          :| []
      )
      ( EApplication
          ()
          (typeVariable 4)
          ( EApplication
              ()
              (typeVariable 5)
              (EVariable () (Label (typeVariable 6) "f"))
              (EVariable () (Label (typeVariable 7) "f") :| [])
          )
          ( EApplication
              ()
              (typeVariable 8)
              (EVariable () (Label (typeVariable 9) "f"))
              (ELiteral () (LInt32 1) :| [])
              :| []
          )
      )
  , typeSubstitutionFromList
      [ (1, typeVariable 3 `TArrow` typeVariable 3)
      , (2, typeVariable 3)
      , (4, typeInt32)
      , (5, typeInt32 `TArrow` typeInt32)
      , (6, (typeInt32 `TArrow` typeInt32) `TArrow` typeInt32 `TArrow` typeInt32)
      , (7, typeInt32 `TArrow` typeInt32)
      , (8, typeInt32)
      , (9, typeInt32 `TArrow` typeInt32)
      ]
  )

fixture2Result :: Expression () (Type TypeIndex (Kind KindIndex))
fixture2Result =
  ELet
    ()
    ( BPattern
        (PVariable () (Label (typeVariable 3 `TArrow` typeVariable 3) "f"))
        ( ELambda
            ()
            (PVariable () (Label (typeVariable 3) "x") :| [])
            (EVariable () (Label (typeVariable 3) "x"))
        )
        :| []
    )
    ( EApplication
        ()
        typeInt32
        ( EApplication
            ()
            (typeInt32 `TArrow` typeInt32)
            (EVariable () (Label ((typeInt32 `TArrow` typeInt32) `TArrow` typeInt32 `TArrow` typeInt32) "f"))
            (EVariable () (Label (typeInt32 `TArrow` typeInt32) "f") :| [])
        )
        ( EApplication
            ()
            typeInt32
            (EVariable () (Label (typeInt32 `TArrow` typeInt32) "f"))
            (ELiteral () (LInt32 1) :| [])
            :| []
        )
    )

fixture3 :: Expression () (Type TypeIndex (Kind KindIndex))
fixture3 =
  ELambda
    ()
    (PVariable () (Label (typeBool `TArrow` typeVariable 3) "m") :| [])
    ( ELet
        ()
        ( BPattern
            (PVariable () (Label (typeBool `TArrow` typeVariable 3) "y"))
            (EVariable () (Label (typeBool `TArrow` typeVariable 3) "m"))
            :| []
        )
        ( ELet
            ()
            ( BPattern
                (PVariable () (Label (typeVariable 3) "x"))
                ( EApplication
                    ()
                    (typeVariable 3)
                    (EVariable () (Label (typeBool `TArrow` typeVariable 3) "y"))
                    (ELiteral () (LBool True) :| [])
                )
                :| []
            )
            ( EVariable () (Label (typeVariable 3) "x")
            )
        )
    )

typeVariable :: Int -> Type TypeIndex (Kind KindIndex)
typeVariable n = TVariable (TypeIndex (KVariable (KindIndex n)) n)

typeVariableKind :: Int -> Int -> Type TypeIndex (Kind KindIndex)
typeVariableKind m n = TVariable (TypeIndex (KVariable (KindIndex m)) n)

typeBool :: Type TypeIndex k
typeBool = TIntrinsic IBool

typeInt32 :: Type TypeIndex k
typeInt32 = TIntrinsic IInt32
