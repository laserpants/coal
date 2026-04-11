{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Coal.TypeSystem.Kind.Unification (
  KindUnifier (..),
  unifyKinds,
)
where

import Coal.Language.Type.Kind (Kind (..))
import Coal.TypeSystem.Kind.Error (KindError (..))
import Coal.TypeSystem.Kind.Substitution
import Control.Monad.Except (MonadError, throwError)
import qualified Data.Map.Strict as Map
import Data.Set (Set, member)
import qualified Data.Set as Set

newtype KindUnifier a = KindUnifier {kindUnifierMonad :: Either KindError a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadError KindError
    )

unifyKinds :: Kind -> Kind -> KindUnifier KindSubstitution
unifyKinds (KArrow k1 k2) (KArrow k3 k4) = do
  sub1 <- unifyKinds k1 k3
  sub2 <- unifyKinds (applyKinds sub1 k2) (applyKinds sub1 k4)
  pure (sub2 <> sub1)
unifyKinds (KVariable k1) k2 =
  bindKind k1 k2
unifyKinds k1 (KVariable k2) =
  bindKind k2 k1
unifyKinds k1 k2
  | k1 == k2 = pure mempty
  | otherwise =
      throwError ECannotUnifyKinds

bindKind :: Int -> Kind -> KindUnifier KindSubstitution
bindKind n =
  \case
    KVariable k
      | k == n ->
          pure mempty
    k
      | n `member` kindIdsIn k ->
          throwError EInfiniteKind
      | otherwise ->
          pure (KindSubstitution (Map.singleton n k))

kindIdsIn :: Kind -> Set Int
kindIdsIn =
  \case
    KArrow k1 k2 ->
      kindIdsIn k1 <> kindIdsIn k2
    KVariable n ->
      Set.singleton n
    _ ->
      mempty
