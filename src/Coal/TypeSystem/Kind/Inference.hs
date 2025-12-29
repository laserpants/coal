{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}

module Coal.TypeSystem.Kind.Inference (
  KindInferenceError (..),
  inferTraitKinds,
  inferTypeKinds,
) where

import Coal.Common.Environment (Environment (..), mapEnvironment)
import qualified Coal.Common.Environment as Environment
import Coal.Language.Module.Definition.Trait (TraitDefinition (..))
import Coal.Language.Trait (Trait (..))
import Coal.Language.Type (Parameter (Parameter), Type (..))
import Coal.Language.Type.Kind (Kind (..))
import Coal.Language.Type.Row (Row (..))
import Coal.Language.Type.Scheme (Scheme (..))
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.RWS
import Control.Monad.State (State, evalState, runStateT)
import Data.Bifunctor (bimap)
import Data.Data (Data, Typeable)
import Data.Either (partitionEithers)
import Data.Generics.Uniplate.Data (universeBi)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set, member)
import qualified Data.Set as Set
import Extras (Name, traverse_)
import Extras.Control.Monad.Writer (tellLeft, tellRight)
import Extras.Data.Set (unionMap)

data KindNode
  = IsKType
  | IsKRow
  | IsKTrait
  | IsKArrow KindNode KindNode
  | IsKVar Int
  deriving (Show, Eq, Ord, Read, Data, Typeable)

liftKind :: Kind -> KindNode
liftKind =
  \case
    KType ->
      IsKType
    KRow ->
      IsKRow
    KArrow k1 k2 ->
      IsKArrow (liftKind k1) (liftKind k2)
    KTrait ->
      IsKTrait

class LowerKinds a b where
  lowerKinds :: a -> b

instance LowerKinds KindNode Kind where
  lowerKinds = \case
    IsKType -> KType
    IsKRow -> KRow
    IsKTrait -> KTrait
    IsKArrow a b -> KArrow (lowerKinds a) (lowerKinds b)
    IsKVar _ -> KType

instance LowerKinds (Parameter KindNode) (Parameter Kind) where
  lowerKinds (Parameter k name) = Parameter (lowerKinds k) name

instance
  LowerKinds
    (Row Parameter KindNode (Type Parameter KindNode))
    (Row Parameter Kind (Type Parameter Kind))
  where
  lowerKinds = \case
    RExtend name t row ->
      RExtend name (lowerKinds t) (lowerKinds row)
    RVariable (Parameter _ name) ->
      RVariable (Parameter KRow name)
    RNil -> RNil

instance LowerKinds (Type Parameter KindNode) (Type Parameter Kind) where
  lowerKinds = \case
    TApplication k t1 t2 ->
      TApplication (lowerKinds k) (lowerKinds t1) (lowerKinds t2)
    TArrow t1 t2 ->
      TArrow (lowerKinds t1) (lowerKinds t2)
    TConstructor k name ->
      TConstructor (lowerKinds k) name
    TIntrinsic i ->
      TIntrinsic i
    TRecord t ->
      TRecord (lowerKinds t)
    TRow row ->
      TRow (lowerKinds row)
    TVariable param ->
      TVariable (lowerKinds param)
    TAlias name ts t ->
      TAlias name (fmap lowerKinds ts) (lowerKinds t)

instance LowerKinds (Trait (Parameter KindNode)) (Trait (Parameter Kind)) where
  lowerKinds (Trait n p) = Trait n (lowerKinds p)

instance
  LowerKinds
    (Trait (Type Parameter KindNode))
    (Trait (Type Parameter Kind))
  where
  lowerKinds (Trait n t) = Trait n (lowerKinds t)

instance LowerKinds (Set (Parameter KindNode)) (Set (Parameter Kind)) where
  lowerKinds = Set.map lowerKinds

instance
  LowerKinds
    (Scheme Parameter KindNode (Type Parameter KindNode))
    (Scheme Parameter Kind (Type Parameter Kind))
  where
  lowerKinds (Forall vs ts t) =
    Forall (lowerKinds vs) (fmap lowerKinds ts) (lowerKinds t)

nodeKind :: Type Parameter KindNode -> KindNode
nodeKind =
  \case
    TRow{} ->
      IsKRow
    TArrow{} ->
      IsKType
    TIntrinsic{} ->
      IsKType
    TRecord{} ->
      IsKType
    k ->
      head (universeBi k)

data KindConstraint = KEquality KindNode KindNode
  deriving (Show, Eq, Ord, Read)

data KindInferenceError
  = ENoTypeConstructor Name
  | ECannotUnifyKinds
  | EInfiniteKind
  deriving (Show, Eq, Ord, Read)

type KindConstraintsGenOutput = Either KindInferenceError KindConstraint

newtype KindConstraintsGen a = KindConstraintsGen {kindConstraintsGenMonad :: RWS (Environment KindNode) [KindConstraintsGenOutput] (Environment KindNode) a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (Environment KindNode)
    , MonadWriter [KindConstraintsGenOutput]
    , MonadState (Environment KindNode)
    , MonadRWS (Environment KindNode) [KindConstraintsGenOutput] (Environment KindNode)
    )

runKindConstraintsGen :: Environment KindNode -> KindConstraintsGen a -> (a, [KindConstraintsGenOutput])
runKindConstraintsGen env gen = (s, w)
 where
  (s, _, w) = runRWS (kindConstraintsGenMonad gen) env mempty

class EmitKinds k where
  emitKindConstraints :: k -> KindConstraintsGen ()

instance (EmitKinds t) => EmitKinds [t] where
  emitKindConstraints = traverse_ emitKindConstraints

instance (EmitKinds t) => EmitKinds (Set t) where
  emitKindConstraints = traverse_ emitKindConstraints . Set.toList

instance EmitKinds (Type Parameter KindNode) where
  emitKindConstraints =
    \case
      TApplication k t1 t2 -> do
        emitKindConstraints t1
        emitKindConstraints t2
        tellRight [KEquality (nodeKind t1) (IsKArrow (nodeKind t2) k)]
      TArrow t1 t2 -> do
        emitKindConstraints t1
        emitKindConstraints t2
        pure ()
      TConstructor k name -> do
        env <- ask
        case Environment.lookup name env of
          Nothing ->
            tellLeft [ENoTypeConstructor name]
          Just k1 ->
            tellRight [KEquality k k1]
      TIntrinsic{} ->
        pure ()
      TRecord t ->
        emitKindConstraints t
      TRow row ->
        emitKindConstraints row
      TVariable param ->
        emitKindConstraints param
      TAlias _ _ t ->
        emitKindConstraints t

instance EmitKinds (Parameter KindNode) where
  emitKindConstraints =
    \case
      Parameter k name -> do
        env <- get
        case Environment.lookup name env of
          Just k1 ->
            tellRight [KEquality k k1]
          Nothing ->
            modify (Environment.insert name k)

instance (EmitKinds k) => EmitKinds (Row Parameter KindNode k) where
  emitKindConstraints =
    \case
      RExtend _ t row -> do
        emitKindConstraints t
        emitKindConstraints row
      RVariable (Parameter k _) ->
        tellRight [KEquality k IsKRow]
      RNil ->
        pure ()

instance (EmitKinds k) => EmitKinds (Trait k) where
  emitKindConstraints =
    \case
      Trait _ t ->
        emitKindConstraints t

instance (EmitKinds k) => EmitKinds (Scheme Parameter KindNode k) where
  emitKindConstraints =
    \case
      Forall vs ts t -> do
        emitKindConstraints vs
        emitKindConstraints ts
        emitKindConstraints t

next :: State Int KindNode
next = do
  modify (+ 1)
  gets IsKVar

class IndexKinds a where
  type Indexed a
  indexKinds :: a -> State Int (Indexed a)

instance IndexKinds (Parameter ()) where
  type Indexed (Parameter ()) = Parameter KindNode
  indexKinds = \case
    Parameter () name -> do
      k <- next
      pure (Parameter k name)

instance IndexKinds (Type Parameter ()) where
  type Indexed (Type Parameter ()) = Type Parameter KindNode
  indexKinds = \case
    TApplication () t1 t2 -> do
      k <- next
      t1' <- indexKinds t1
      t2' <- indexKinds t2
      pure $ TApplication k t1' t2'
    TArrow t1 t2 -> do
      t1' <- indexKinds t1
      t2' <- indexKinds t2
      pure $ TArrow t1' t2'
    TConstructor () name -> do
      k <- next
      pure $ TConstructor k name
    TIntrinsic i ->
      pure $ TIntrinsic i
    TRecord t -> do
      t' <- indexKinds t
      pure $ TRecord t'
    TRow row -> do
      row' <- indexKinds row
      pure $ TRow row'
    TVariable p -> do
      p' <- indexKinds p
      pure $ TVariable p'
    TAlias name ts t -> do
      ts' <- traverse indexKinds ts
      t' <- indexKinds t
      pure $ TAlias name ts' t'

instance IndexKinds (Row Parameter () (Type Parameter ())) where
  type
    Indexed (Row Parameter () (Type Parameter ())) =
      Row Parameter KindNode (Type Parameter KindNode)
  indexKinds = \case
    RExtend name t row -> do
      t' <- indexKinds t
      row' <- indexKinds row
      pure $ RExtend name t' row'
    RVariable (Parameter () name) ->
      pure $ RVariable (Parameter IsKRow name)
    RNil -> pure RNil

instance IndexKinds (Trait (Type Parameter ())) where
  type Indexed (Trait (Type Parameter ())) = Trait (Type Parameter KindNode)
  indexKinds (Trait name t) = do
    t' <- indexKinds t
    pure $ Trait name t'

instance IndexKinds (Trait (Parameter ())) where
  type Indexed (Trait (Parameter ())) = Trait (Parameter KindNode)
  indexKinds (Trait name p) = do
    p' <- indexKinds p
    pure $ Trait name p'

instance IndexKinds (Set (Parameter ())) where
  type Indexed (Set (Parameter ())) = Set (Parameter KindNode)
  indexKinds s = do
    let xs = Set.toList s
    ys <- traverse indexKinds xs
    pure $ Set.fromList ys

instance IndexKinds (Scheme Parameter () (Type Parameter ())) where
  type
    Indexed (Scheme Parameter () (Type Parameter ())) =
      Scheme Parameter KindNode (Type Parameter KindNode)
  indexKinds = \case
    Forall vs ts t -> do
      vs' <- indexKinds vs
      ts' <- traverse indexKinds ts
      t' <- indexKinds t
      pure $ Forall vs' ts' t'

newtype KindSubstitution = KindSubstitution {kindSubstitutionMap :: Map Int KindNode}
  deriving (Show, Eq, Ord)

instance Semigroup KindSubstitution where
  s1 <> s2 = KindSubstitution (s3 <> kindSubstitutionMap s1)
   where
    s3 = applyKinds s1 (kindSubstitutionMap s2)

instance Monoid KindSubstitution where
  mempty = KindSubstitution mempty

class KindSubstitutable k where
  applyKinds :: KindSubstitution -> k -> k

instance (KindSubstitutable k) => KindSubstitutable [k] where
  applyKinds = fmap . applyKinds

instance (Ord k, KindSubstitutable k) => KindSubstitutable (Set k) where
  applyKinds = Set.map . applyKinds

instance (KindSubstitutable n, KindSubstitutable k) => KindSubstitutable (n, k) where
  applyKinds sub (a, b) = (applyKinds sub a, applyKinds sub b)

instance (Ord k, KindSubstitutable k, KindSubstitutable t) => KindSubstitutable (Scheme Parameter k t) where
  applyKinds sub =
    \case
      Forall vs ts t ->
        Forall (applyKinds sub vs) (applyKinds sub ts) (applyKinds sub t)

instance (KindSubstitutable k) => KindSubstitutable (Trait k) where
  applyKinds = fmap . applyKinds

instance KindSubstitutable (Map Int KindNode) where
  applyKinds = fmap . applyKinds

instance KindSubstitutable KindConstraint where
  applyKinds sub =
    \case
      KEquality k1 k2 ->
        KEquality (applyKinds sub k1) (applyKinds sub k2)

instance KindSubstitutable KindNode where
  applyKinds sub =
    \case
      IsKArrow k1 k2 ->
        IsKArrow (applyKinds sub k1) (applyKinds sub k2)
      IsKVar n ->
        fromMaybe (IsKVar n) (Map.lookup n (kindSubstitutionMap sub))
      k ->
        k

instance (KindSubstitutable k) => KindSubstitutable (Parameter k) where
  applyKinds sub =
    \case
      Parameter k name ->
        Parameter (applyKinds sub k) name

instance KindSubstitutable (Type Parameter KindNode) where
  applyKinds sub =
    \case
      TApplication k t1 t2 ->
        TApplication (applyKinds sub k) (applyKinds sub t1) (applyKinds sub t2)
      TArrow t1 t2 ->
        TArrow (applyKinds sub t1) (applyKinds sub t2)
      TConstructor k name ->
        TConstructor (applyKinds sub k) name
      TIntrinsic i ->
        TIntrinsic i
      TRecord t ->
        TRecord (applyKinds sub t)
      TRow row ->
        TRow (applyKinds sub row)
      TVariable param ->
        TVariable (applyKinds sub param)
      TAlias name ts t ->
        TAlias name (fmap (applyKinds sub) ts) (applyKinds sub t)

instance (KindSubstitutable n, KindSubstitutable k) => KindSubstitutable (Row Parameter n k) where
  applyKinds sub =
    \case
      RExtend name t row ->
        RExtend name (applyKinds sub t) (applyKinds sub row)
      RVariable (Parameter k name) -> do
        RVariable (Parameter (applyKinds sub k) name)
      RNil ->
        RNil

newtype KindUnifier a = KindUnifier (Either KindInferenceError a)
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadError KindInferenceError
    )

unifyKinds :: KindNode -> KindNode -> KindUnifier KindSubstitution
unifyKinds (IsKArrow k1 k2) (IsKArrow k3 k4) = do
  sub1 <- unifyKinds k1 k3
  sub2 <- unifyKinds (applyKinds sub1 k2) (applyKinds sub1 k4)
  pure (sub2 <> sub1)
unifyKinds (IsKVar k1) k2 =
  bindKind k1 k2
unifyKinds k1 (IsKVar k2) =
  bindKind k2 k1
unifyKinds k1 k2
  | k1 == k2 = pure mempty
  | otherwise = throwError ECannotUnifyKinds

bindKind :: Int -> KindNode -> KindUnifier KindSubstitution
bindKind n =
  \case
    IsKVar k
      | k == n ->
          pure mempty
    k
      | n `member` kindIdsIn k ->
          throwError EInfiniteKind
      | otherwise ->
          pure (KindSubstitution (Map.singleton n k))

kindIdsIn :: KindNode -> Set Int
kindIdsIn =
  \case
    IsKArrow k1 k2 ->
      kindIdsIn k1 <> kindIdsIn k2
    IsKVar n ->
      Set.singleton n
    _ ->
      mempty

class Parameterized p where
  paramsIn :: p -> Set Name

instance (Parameterized p) => Parameterized [p] where
  paramsIn = unionMap paramsIn

instance (Parameterized p) => Parameterized (Set p) where
  paramsIn = Set.unions . Set.map paramsIn

instance Parameterized (Parameter p) where
  paramsIn (Parameter _ name) = Set.singleton name

instance (Parameterized p) => Parameterized (Trait p) where
  paramsIn (Trait _ t) = paramsIn t

instance Parameterized (TraitDefinition k) where
  paramsIn (TraitDefinition ts p _) = paramsIn ts <> paramsIn p

solveKindConstraints :: [KindConstraint] -> KindUnifier KindSubstitution
solveKindConstraints [] =
  pure mempty
solveKindConstraints (KEquality k1 k2 : cs) = do
  sub1 <- unifyKinds k1 k2
  sub2 <- solveKindConstraints (applyKinds sub1 cs)
  pure (sub2 <> sub1)

insertKind :: [(Name, k)] -> Parameter () -> Parameter k
insertKind params (Parameter () name) =
  case lookup name params of
    Just kind ->
      Parameter kind name
    Nothing ->
      error "Implementation error"

insertTraitKind :: [(Name, k)] -> Trait (Parameter ()) -> Trait (Parameter k)
insertTraitKind params (Trait trait (Parameter () name)) =
  case lookup name params of
    Just kind ->
      Trait trait (Parameter kind name)
    Nothing ->
      error "Implementation error"

inferTraitKinds :: Environment Kind -> TraitDefinition () -> Either [KindInferenceError] (TraitDefinition Kind)
inferTraitKinds env def@(TraitDefinition ts p defs) =
  case runStateT go (fmap (insertTraitKind qs) ts, insertKind qs p) of
    Left errs ->
      Left errs
    Right (defs0, (traits0, param0)) ->
      pure $
        TraitDefinition
          (lowerKinds <$> traits0)
          (lowerKinds param0)
          defs0
 where
  qs = zip (Set.toList (paramsIn def)) [IsKVar n | n <- [1 ..]]
  go =
    forM defs $
      \(n, s) -> do
        let (r, outs) = runKindConstraintsGen (mapEnvironment liftKind env) $ do
              forM_ qs (modify . uncurry Environment.insert)
              let indexed = evalState (indexKinds s) (length qs)
              emitKindConstraints indexed
              pure indexed
        let (errs, cs) = partitionEithers outs
            KindUnifier res = solveKindConstraints cs
        unless (null errs) $
          throwError errs
        case res of
          Left err ->
            throwError [err]
          Right sub -> do
            modify (bimap (applyKinds sub) (applyKinds sub))
            pure (n, lowerKinds (applyKinds sub r))

inferTypeKinds :: Type Parameter () -> Either [KindInferenceError] (Type Parameter Kind)
inferTypeKinds t = do
  -- TODO: DRY
  let (r, outs) = runKindConstraintsGen mempty $ do
        let indexed = evalState (indexKinds t) 0
        emitKindConstraints indexed
        pure indexed
  let (errs, cs) = partitionEithers outs
      KindUnifier res = solveKindConstraints cs
  unless (null errs) $
    Left errs
  case res of
    Left err ->
      Left [err]
    Right sub -> do
      Right (lowerKinds (applyKinds sub r))
