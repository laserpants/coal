{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.ProtoTypeSystem.Kind.Constraint.Generation (
  ProtoKindInferenceError (..),
  ProtoKindConstraintsGen (..),
) where

import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Language.Data.Constructor (DataConstructor (..))
import Coal.Language.HasKind (HasKind (..))
import Coal.Language.Type (Parameter (..), Type (..))
import Coal.Language.Type.Kind (Kind (..))
import Coal.Language.Type.Row (Row (..))
import Coal.Language.Type.Scheme (Scheme (..))
import Coal.ProtoLanguage.ProtoDefinition
import Coal.ProtoLanguage.ProtoModule
import Coal.ProtoTypeSystem.Kind.Constraint (ProtoKindConstraint (..))
import Control.Monad.RWS
import Extras (Name, traverse_)
import Extras.Control.Monad.Writer (tellLeft, tellRight)

data ProtoKindInferenceError
  = ProtoENoTypeConstructor Name
  | ProtoECannotUnifyKinds
  | ProtoEInfiniteKind
  deriving (Show, Eq, Ord, Read)

type ProtoKindConstraintsGenOutput = Either ProtoKindInferenceError ProtoKindConstraint

newtype ProtoKindConstraintsGen a = ProtoKindConstraintsGen
  { protoOkindConstraintsGenMonad :: RWS (Environment Kind) [ProtoKindConstraintsGenOutput] () a
  }
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

instance (ProtoEmitKinds k) => ProtoEmitKinds [k] where
  protoOemitKindConstraints = traverse_ protoOemitKindConstraints

instance ProtoEmitKinds (DataConstructor o Kind t) where
  protoOemitKindConstraints =
    \case
      DataConstructor{..} ->
        undefined

instance ProtoEmitKinds (Type Parameter Kind) where
  protoOemitKindConstraints =
    \case
      TApplication k t1 t2 -> do
        protoOemitKindConstraints t1
        protoOemitKindConstraints t2
        tellRight [ProtoKEquality (kindOf t1) (KArrow (kindOf t2) k)]
      TArrow t1 t2 -> do
        protoOemitKindConstraints t1
        protoOemitKindConstraints t2
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
      TVariable Parameter{..} ->
        undefined
      TAlias _ ts t -> do
        protoOemitKindConstraints ts
        protoOemitKindConstraints t

instance ProtoEmitKinds (Row Parameter Kind (Type Parameter Kind)) where
  protoOemitKindConstraints =
    \case
      RExtend _ t row -> do
        protoOemitKindConstraints t
        protoOemitKindConstraints row
      RVariable Parameter{..} ->
        undefined
      RNil ->
        pure ()

instance ProtoEmitKinds (Scheme Parameter Kind (Type Parameter Kind)) where
  protoOemitKindConstraints =
    \case
      Forall{..} -> do
        forM_ schemeTypeVariables $
          \Parameter{..} ->
            undefined

instance ProtoEmitKinds (ProtoModule a Kind t) where
  protoOemitKindConstraints =
    \case
      ProtoModule{..} ->
        protoOemitKindConstraints protoOmoduleDefinitions

instance ProtoEmitKinds (ProtoDefinition a Kind t) where
  protoOemitKindConstraints =
    \case
      ProtoDType _ _ def ->
        protoOemitKindConstraints def
      ProtoDTypeAlias _ _ ->
        pure ()
      ProtoDFunction _ _ def ->
        protoOemitKindConstraints def
      ProtoDFunctionGroup _ _ ->
        pure ()
      ProtoDFold _ _ ->
        pure ()
      ProtoDLet _ _ def ->
        protoOemitKindConstraints def
      ProtoDImport{} ->
        pure ()
      ProtoDQualifiedImport{} ->
        pure ()
      ProtoDTrait _ _ def ->
        protoOemitKindConstraints def
      ProtoDInstance _ def ->
        protoOemitKindConstraints def

instance ProtoEmitKinds (ProtoTypeDefinition a Kind t) where
  protoOemitKindConstraints =
    \case
      ProtoTypeDefinition{..} -> do
        undefined

instance ProtoEmitKinds (ProtoFunctionDefinition a Kind t) where
  protoOemitKindConstraints =
    \case
      ProtoFunctionDefinition{..} -> do
        undefined

instance ProtoEmitKinds (ProtoLetDefinition a Kind t) where
  protoOemitKindConstraints =
    \case
      ProtoLetDefinition{..} ->
        undefined

--
--  trait Applicative<f> with Functor<f> {
--    pure : a -> f<a>
--    ap : f<a -> b> -> f<a> -> f<b>
--  }
--
--
instance ProtoEmitKinds (ProtoTraitDefinition a Kind) where
  protoOemitKindConstraints =
    \case
      ProtoTraitDefinition{..} ->
        undefined

instance ProtoEmitKinds (ProtoInstanceDefinition a Kind t) where
  protoOemitKindConstraints =
    \case
      ProtoInstanceDefinition{..} ->
        undefined
