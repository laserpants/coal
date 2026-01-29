{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

module Coal.ProtoTypeSystem.Kind.Constraint.Generation (
  ProtoKindInferenceError (..),
  KindConstraintsGen (..),
) where

import Coal.Common.Environment (Environment (..))
import Coal.Language.Type.Kind (Kind (..))
import Coal.ProtoTypeSystem.Kind.Constraint (ProtoKindConstraint (..))
import Control.Monad.RWS
import Extras (Name)

data ProtoKindInferenceError
  = ProtoENoTypeConstructor Name
  | ProtoECannotUnifyKinds
  | ProtoEInfiniteKind
  deriving (Show, Eq, Ord, Read)

type ProtoKindConstraintsGenOutput = Either ProtoKindInferenceError ProtoKindConstraint

newtype KindConstraintsGen a = KindConstraintsGen {protoOkindConstraintsGenMonad :: RWS (Environment Kind) [ProtoKindConstraintsGenOutput] (Environment Kind) a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (Environment Kind)
    , MonadWriter [ProtoKindConstraintsGenOutput]
    , MonadState (Environment Kind)
    , MonadRWS (Environment Kind) [ProtoKindConstraintsGenOutput] (Environment Kind)
    )
