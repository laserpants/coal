{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Coal.ProtoTypeSystem.Kind.Unification (
  ProtoKindUnifier (..),
  unifyKinds,
)
where

import Coal.Language.Type.Kind (Kind (..))
import Coal.ProtoTypeSystem.Kind.Error (ProtoKindError (..))
import Coal.ProtoTypeSystem.Kind.Substitution
import Control.Monad.Except (MonadError, throwError)
import qualified Data.Map.Strict as Map
import Data.Set (Set, member)
import qualified Data.Set as Set

newtype ProtoKindUnifier a = ProtoKindUnifier
  {protoOKindUnifierMonad :: Either ProtoKindError a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadError ProtoKindError
    )

unifyKinds :: Kind -> Kind -> ProtoKindUnifier ProtoKindSubstitution
unifyKinds (KArrow k1 k2) (KArrow k3 k4) = do
  sub1 <- unifyKinds k1 k3
  sub2 <- unifyKinds (protoOapplyKinds sub1 k2) (protoOapplyKinds sub1 k4)
  pure (sub2 <> sub1)
unifyKinds (KVar k1) k2 =
  bindKind k1 k2
unifyKinds k1 (KVar k2) =
  bindKind k2 k1
unifyKinds k1 k2
  | k1 == k2 = pure mempty
  | otherwise = throwError ProtoEInfiniteKind

bindKind :: Int -> Kind -> ProtoKindUnifier ProtoKindSubstitution
bindKind n =
  \case
    KVar k
      | k == n ->
          pure mempty
    k
      | n `member` kindIdsIn k ->
          throwError ProtoEInfiniteKind
      | otherwise ->
          pure (ProtoKindSubstitution (Map.singleton n k))

kindIdsIn :: Kind -> Set Int
kindIdsIn =
  \case
    KArrow k1 k2 ->
      kindIdsIn k1 <> kindIdsIn k2
    KVar n ->
      Set.singleton n
    _ ->
      mempty
