{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.TypeSystem.KindConstraint.CollectSpec where

import Data.List.NonEmpty (NonEmpty (..))
import Noll.Label (Label (..))
import Noll.Language (Binding (..), Expression (..), Intrinsic (..), Kind (..), KindIndex (..), Pattern (..), Primitive (..), Type (..), TypeIndex (..))
import Noll.TypeSystem.KindConstraint (KindConstraint (..))
import Noll.TypeSystem.KindConstraint.Collect (collectKindConstraints, runCollectKindConstraints)
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "Noll.TypeSystem.KindConstraint.Collect" $ do
    it "" $
      hasConstraints
        fixture1
        [ KindEquality (KVariable (KindIndex 3)) KType
        ]
    it "" $
      hasConstraints
        fixture2
        [ KindEquality (KVariable (KindIndex 3)) KType
        ]

hasConstraints :: Expression (Type TypeIndex (Kind KindIndex)) -> [KindConstraint (Kind KindIndex)] -> Bool
hasConstraints = all . hasConstraint

hasConstraint :: Expression (Type TypeIndex (Kind KindIndex)) -> KindConstraint (Kind KindIndex) -> Bool
hasConstraint e =
  \case
    KindEquality k1 k2 ->
      elem (KindEquality k1 k2) constraints || elem (KindEquality k2 k1) constraints
 where
  constraints = runCollectKindConstraints mempty (collectKindConstraints e)

-- fn(m) => let y = m in let x = y(true) in x
fixture1 :: Expression (Type TypeIndex (Kind KindIndex))
fixture1 =
  ELambda
    (PVariable (Label (TIntrinsic IBool `TArrow` TVariable (TypeIndex (KVariable (KindIndex 3)) 3)) "m") :| [])
    ( ELet
        ( BPattern
            (PVariable (Label (TIntrinsic IBool `TArrow` TVariable (TypeIndex (KVariable (KindIndex 3)) 3)) "y"))
            (EVariable (Label (TIntrinsic IBool `TArrow` TVariable (TypeIndex (KVariable (KindIndex 3)) 3)) "m"))
            :| []
        )
        ( ELet
            ( BPattern
                (PVariable (Label (TVariable (TypeIndex (KVariable (KindIndex 3)) 3)) "x"))
                ( EApplication
                    (TVariable (TypeIndex (KVariable (KindIndex 3)) 3))
                    (EVariable (Label (TIntrinsic IBool `TArrow` TVariable (TypeIndex (KVariable (KindIndex 3)) 3)) "y"))
                    (ELiteral (LBool True) :| [])
                )
                :| []
            )
            ( EVariable (Label (TVariable (TypeIndex (KVariable (KindIndex 3)) 3)) "x")
            )
        )
    )

-- let f = fn(x) => x in (f f)(f 1)
fixture2 :: Expression (Type TypeIndex (Kind KindIndex))
fixture2 =
  ELet
    ( BPattern
        (PVariable (Label (TVariable (TypeIndex (KVariable (KindIndex 3)) 3) `TArrow` TVariable (TypeIndex (KVariable (KindIndex 3)) 3)) "f"))
        ( ELambda
            (PVariable (Label (TVariable (TypeIndex (KVariable (KindIndex 3)) 3)) "x") :| [])
            (EVariable (Label (TVariable (TypeIndex (KVariable (KindIndex 3)) 3)) "x"))
        )
        :| []
    )
    ( EApplication
        (TIntrinsic IInt32)
        ( EApplication
            (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32)
            (EVariable (Label ((TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) "f"))
            (EVariable (Label (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) "f") :| [])
        )
        ( EApplication
            (TIntrinsic IInt32)
            (EVariable (Label (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32) "f"))
            (ELiteral (LInt32 1) :| [])
            :| []
        )
    )
