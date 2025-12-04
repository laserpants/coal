{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Coal.TypeSystem.Kind.Inference (inferTraitKinds) where

import Coal.Common.Environment (Environment (..), mapEnvironment)
import qualified Coal.Common.Environment as Environment
import Coal.Language.Module.Definition.Trait (TraitDef (..))
import Coal.Language.Trait (Trait (..))
import Coal.Language.Type
import Coal.Language.Type.Kind
import Coal.Language.Type.Row
import Coal.Language.Type.Scheme (Scheme (..))
import Control.Monad.RWS
import Control.Monad.State (State, evalState, runState)
import Data.Bifunctor (bimap)
import Data.Data (Data, Typeable)
import Data.Generics.Uniplate.Data (universeBi)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set, member)
import qualified Data.Set as Set
import Extras (Name, traverse_)
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

lowerKindNode :: KindNode -> Kind
lowerKindNode =
  \case
    IsKType ->
      KType
    IsKRow ->
      KRow
    IsKTrait ->
      KTrait
    IsKArrow k1 k2 ->
      KArrow (lowerKindNode k1) (lowerKindNode k2)
    IsKVar{} ->
      KType

lowerKinds :: Type Parameter KindNode -> Type Parameter Kind
lowerKinds =
  \case
    TApplication k t1 t2 -> do
      TApplication (lowerKindNode k) (lowerKinds t1) (lowerKinds t2)
    TArrow t1 t2 ->
      TArrow (lowerKinds t1) (lowerKinds t2)
    TConstructor k name ->
      TConstructor (lowerKindNode k) name
    TIntrinsic i ->
      TIntrinsic i
    TRecord t ->
      TRecord (lowerKinds t)
    TRow row ->
      TRow (lowerKindsInRow row)
    TVariable param ->
      TVariable (lowerKindsInParameter param)
    TAlias name ts t ->
      TAlias name (fmap lowerKinds ts) (lowerKinds t)

lowerKindsInParameter :: Parameter KindNode -> Parameter Kind
lowerKindsInParameter =
  \case
    (Parameter k name) ->
      Parameter (lowerKindNode k) name

lowerKindsInRow :: Row Parameter KindNode (Type Parameter KindNode) -> Row Parameter Kind (Type Parameter Kind)
lowerKindsInRow =
  \case
    RExtend name t row ->
      RExtend name (lowerKinds t) (lowerKindsInRow row)
    RVariable (Parameter _ name) ->
      RVariable (Parameter KRow name)
    RNil ->
      RNil

lowerKindsInTrait :: Trait (Parameter KindNode) -> Trait (Parameter Kind)
lowerKindsInTrait =
  \case
    Trait n p ->
      Trait n (lowerKindsInParameter p)

lowerKindsInTrait1 :: Trait (Type Parameter KindNode) -> Trait (Type Parameter Kind)
lowerKindsInTrait1 =
  \case
    Trait n t ->
      Trait n (lowerKinds t)

lowerKindsInSet :: Set (Parameter KindNode) -> Set (Parameter Kind)
lowerKindsInSet = Set.map lowerKindsInParameter

lowerKindsInScheme1 :: Scheme Parameter KindNode (Type Parameter KindNode) -> Scheme Parameter Kind (Type Parameter Kind)
lowerKindsInScheme1 =
  \case
    Forall vs ts t ->
      Forall (lowerKindsInSet vs) (lowerKindsInTrait1 <$> ts) (lowerKinds t)

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

newtype KindConstraintsGen a = KindConstraintsGen {kindConstraintsGenMonad :: RWS (Environment KindNode) [KindConstraint] (Environment KindNode) a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (Environment KindNode)
    , MonadWriter [KindConstraint]
    , MonadState (Environment KindNode)
    , MonadRWS (Environment KindNode) [KindConstraint] (Environment KindNode)
    )

runKindConstraintsGen :: Environment KindNode -> KindConstraintsGen a -> (a, [KindConstraint])
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
        tell [KEquality (nodeKind t1) (IsKArrow (nodeKind t2) k)]
      TArrow t1 t2 -> do
        emitKindConstraints t1
        emitKindConstraints t2
        pure ()
      TConstructor k name -> do
        env <- ask
        case Environment.lookup name env of
          Nothing ->
            error (show name)
          Just k1 ->
            tell [KEquality k k1]
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
            tell [KEquality k k1]
          Nothing ->
            modify (Environment.insert name k)

instance (EmitKinds k) => EmitKinds (Row Parameter KindNode k) where
  emitKindConstraints =
    \case
      RExtend _ t row -> do
        emitKindConstraints t
        emitKindConstraints row
      RVariable (Parameter k _) ->
        tell [KEquality k IsKRow]
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

indexKinds :: Type Parameter () -> State Int (Type Parameter KindNode)
indexKinds =
  \case
    TApplication () t1 t2 -> do
      TApplication <$> next <*> indexKinds t1 <*> indexKinds t2
    TArrow t1 t2 ->
      TArrow <$> indexKinds t1 <*> indexKinds t2
    TConstructor () name -> do
      k <- next
      pure (TConstructor k name)
    TIntrinsic i ->
      pure (TIntrinsic i)
    TRecord t ->
      TRecord <$> indexKinds t
    TRow row ->
      TRow <$> indexKindsInRow row
    TVariable p -> do
      TVariable <$> indexKindsInParam p
    TAlias name ts t ->
      TAlias name <$> traverse indexKinds ts <*> indexKinds t

indexKindsInParam :: Parameter () -> State Int (Parameter KindNode)
indexKindsInParam =
  \case
    Parameter () name -> do
      k <- next
      pure (Parameter k name)

indexKindsInRow :: Row Parameter () (Type Parameter ()) -> State Int (Row Parameter KindNode (Type Parameter KindNode))
indexKindsInRow =
  \case
    RExtend name t row ->
      RExtend name <$> indexKinds t <*> indexKindsInRow row
    RVariable (Parameter () name) -> do
      pure (RVariable (Parameter IsKRow name))
    RNil ->
      pure RNil

indexKindsInTrait :: Trait (Type Parameter ()) -> State Int (Trait (Type Parameter KindNode))
indexKindsInTrait =
  \case
    Trait name t ->
      Trait name <$> indexKinds t

indexKindsInBound :: Set (Parameter ()) -> State Int (Set (Parameter KindNode))
indexKindsInBound s = Set.fromList <$> traverse indexKindsInParam (Set.toList s)

indexKindsInScheme :: Scheme Parameter () (Type Parameter ()) -> State Int (Scheme Parameter KindNode (Type Parameter KindNode))
indexKindsInScheme =
  \case
    Forall vs ts t ->
      Forall <$> indexKindsInBound vs <*> traverse indexKindsInTrait ts <*> indexKinds t

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

unifyKinds :: (Monad m) => KindNode -> KindNode -> m KindSubstitution
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
  | otherwise = error "Cannot unify"

bindKind :: (Monad m) => Int -> KindNode -> m KindSubstitution
bindKind n =
  \case
    IsKVar k
      | k == n ->
          pure mempty
    k
      | n `member` kindIdsIn k ->
          error "Infinite kind"
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

instance Parameterized (TraitDef k) where
  paramsIn (TraitDef ts p _) = paramsIn ts <> paramsIn p

solveKindConstraints :: (Monad m) => [KindConstraint] -> m KindSubstitution
solveKindConstraints [] =
  pure mempty
solveKindConstraints (KEquality k1 k2 : cs) = do
  sub1 <- unifyKinds k1 k2
  sub2 <- solveKindConstraints (applyKinds sub1 cs)
  pure (sub2 <> sub1)

insertKind :: [(Name, k)] -> Parameter () -> Parameter k
insertKind params (Parameter () name) =
  case lookup name params of
    Just k1 ->
      Parameter k1 name
    Nothing ->
      error "Implementation error"

insertTraitKind :: [(Name, k)] -> Trait (Parameter ()) -> Trait (Parameter k)
insertTraitKind params (Trait trait (Parameter () name)) =
  case lookup name params of
    Just k1 ->
      Trait trait (Parameter k1 name)
    Nothing ->
      error "Implementation error"

inferTraitKinds :: Environment Kind -> TraitDef () -> TraitDef Kind
inferTraitKinds env def@(TraitDef ts p defs) = do
  let (defs0, (traits0, param0)) =
        flip runState (fmap (insertTraitKind qs) ts, insertKind qs p) $
          forM defs $
            \(n, s) -> do
              let (r, cs) = runKindConstraintsGen (mapEnvironment liftKind env) $ do
                    forM_ qs (modify . uncurry Environment.insert)
                    let aaa = evalState (indexKindsInScheme s) (length qs)
                    emitKindConstraints aaa
                    pure aaa
              sub <- solveKindConstraints cs
              modify (bimap (applyKinds sub) (applyKinds sub))
              pure (n, lowerKindsInScheme1 (applyKinds sub r))
   in TraitDef (lowerKindsInTrait <$> traits0) (lowerKindsInParameter param0) defs0
 where
  ps = paramsIn def
  qs = zip (Set.toList ps) [IsKVar n | n <- [1 ..]]
