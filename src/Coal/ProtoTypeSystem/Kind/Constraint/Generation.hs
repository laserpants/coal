{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Coal.ProtoTypeSystem.Kind.Constraint.Generation (
  ProtoKindInferenceError (..),
  ProtoKindConstraintsGen (..),
) where

import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Language.HasKind (HasKind (..))
import Coal.Language.Type (Type (..))
import Coal.Language.Type.Kind (Kind (..))
import Coal.Language.Type.Row (Row (..))
import Coal.ProtoTypeSystem.Kind.Constraint (ProtoKindConstraint (..))
import Control.Monad.RWS
import Data.Data (Data, Typeable)
import Extras (Name)
import Extras.Control.Monad.Writer (tellLeft, tellRight)

data ProtoKindInferenceError
  = ProtoENoTypeConstructor Name
  | ProtoECannotUnifyKinds
  | ProtoEInfiniteKind
  deriving (Show, Eq, Ord, Read)

type ProtoKindConstraintsGenOutput = Either ProtoKindInferenceError ProtoKindConstraint

newtype ProtoKindConstraintsGen a = ProtoKindConstraintsGen {protoOkindConstraintsGenMonad :: RWS (Environment Kind) [ProtoKindConstraintsGenOutput] () a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (Environment Kind)
    , MonadWriter [ProtoKindConstraintsGenOutput]
    , MonadState ()
    , MonadRWS (Environment Kind) [ProtoKindConstraintsGenOutput] ()
    )

class ProtoEmitKinds k where
  protoOemitKindConstraints :: k -> ProtoKindConstraintsGen ()

instance (ProtoEmitKinds (o Kind), Typeable o, Data (o Kind)) => ProtoEmitKinds (Type o Kind) where
  protoOemitKindConstraints =
    \case
      TApplication k t1 t2 -> do
        protoOemitKindConstraints t1
        protoOemitKindConstraints t2
        tellRight [ProtoKEquality (kindOf t1) (KArrow (kindOf t2) k)]
      TArrow t1 t2 -> do
        protoOemitKindConstraints t1
        protoOemitKindConstraints t2
        pure ()
      TConstructor k name -> do
        env <- ask
        case Environment.lookup name env of
          Nothing ->
            tellLeft [ProtoENoTypeConstructor name]
          Just k1 ->
            tellRight [ProtoKEquality k k1]
      TIntrinsic{} ->
        pure ()
      TRecord t ->
        protoOemitKindConstraints t
      TRow row ->
        protoOemitKindConstraints row
      TVariable param ->
        protoOemitKindConstraints param
      TAlias _ _ _ ->
        error "TODO"

instance (ProtoEmitKinds (o Kind), Typeable o, Data (o Kind)) => ProtoEmitKinds (Row o Kind (Type o Kind)) where
  protoOemitKindConstraints =
    undefined
