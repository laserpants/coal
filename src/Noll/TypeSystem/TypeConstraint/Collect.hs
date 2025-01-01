{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.TypeConstraint.Collect (
  TypeConstraintsContext (..),
  TypeConstraints (..),
  CollectConstraints,
  TypeCollectError (..),
  collectTypeConstraints,
  runCollectTypeConstraints,
  evalCollectTypeConstraints,
  annotationScheme,
  withMonomorphic,
)
where

import Control.Monad (join)
import Control.Monad.Except (MonadError, runExceptT, throwError)
import Control.Monad.RWS (MonadRWS, MonadReader, MonadState, MonadWriter, RWS, ask, asks, evalRWS, local, tell)
import Control.Monad.State (StateT, evalStateT, get, modify, put)
import Control.Monad.Trans (lift)
import Data.Either.Extra (lefts, rights)
import Data.List (partition, transpose)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import Data.Tuple.Extra (second, third3)
import Noll.Label (Label (..))
import Noll.Language (
  Binding (..),
  Choice (..),
  Clause (..),
  Constructor (..),
  Expression (..),
  Guard (..),
  HasType (..),
  Intrinsic (..),
  Kind (..),
  KindIndex (..),
  OpaqueRow,
  OpaqueType,
  Pattern (..),
  Row (..),
  Type (..),
  TypeIndex (..),
  TypeIndexed (..),
  TypeVariable (..),
  foldType,
 )
import Noll.Language.Type.Scheme (Scheme (..))
import Noll.Library.Environment (Environment (..))
import qualified Noll.Library.Environment as Environment
import Noll.Library.List1 (list1ToList)
import Noll.Library.Supply (supply, supplyN)
import Noll.TypeSystem.TypeConstraint (MonomorphicSet (..), TypeConstraint (..), overMonomorphicSet)
import Noll.TypeSystem.TypeConstraint.Assumption (Assumption (..), assumptionNameIs)
import Noll.TypeSystem.TypeConstraint.Rule (TypeRule (..))
import Noll.Utils (Dictionary, Name, concatMapM, forM, forM_, tellLeft, tellRight, (<$$>))

data TypeConstraintsContext o k t = TypeConstraintsContext
  { contextMonomorphicSet :: MonomorphicSet (o k)
  , contextConstructorEnv :: Environment (Constructor o k t)
  }
  deriving (Show, Eq, Ord, Read)

{-# INLINE overContextMonomorphicSet #-}
overContextMonomorphicSet :: (MonomorphicSet (o k) -> MonomorphicSet (o k)) -> TypeConstraintsContext o k t -> TypeConstraintsContext o k t
overContextMonomorphicSet fn TypeConstraintsContext{..} = TypeConstraintsContext{contextMonomorphicSet = fn contextMonomorphicSet, ..}

data TypeCollectError a
  = MissingDataConstructor a Name
  | ConstructorArityMismatch a Name Int Int
  | IllFormedTypeAnnotation a
  deriving (Show, Eq, Ord, Read)

type TypeCollectOutput a o k t = Either (TypeCollectError a) (TypeConstraint (TypeRule k a) o k t)

type TypeConstraintsMonad a o k t = RWS (TypeConstraintsContext o k t) [TypeCollectOutput a o k t] ()

newtype TypeConstraints a o k t e = TypeConstraints {constraintsMonad :: TypeConstraintsMonad a o k t e}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (TypeConstraintsContext o k t)
    , MonadWriter [TypeCollectOutput a o k t]
    , MonadState ()
    , MonadRWS (TypeConstraintsContext o k t) [TypeCollectOutput a o k t] ()
    )

{-# INLINE monosetInsert #-}
monosetInsert :: (Ord k) => TypeIndex k -> MonomorphicSet (TypeIndex k) -> MonomorphicSet (TypeIndex k)
monosetInsert = overMonomorphicSet . Set.insert

{-# INLINE monosetInsertMany #-}
monosetInsertMany :: (Ord k, Foldable f) => f (TypeIndex k) -> MonomorphicSet (TypeIndex k) -> MonomorphicSet (TypeIndex k)
monosetInsertMany = flip (foldr monosetInsert)

{-# INLINE localMonoset #-}
localMonoset :: (MonomorphicSet (o k) -> MonomorphicSet (o k)) -> TypeConstraints a o k t e -> TypeConstraints a o k t e
localMonoset = local . overContextMonomorphicSet

{-# INLINE lookupContextConstructor #-}
lookupContextConstructor :: Name -> TypeConstraints a o k t (Maybe (Constructor o k t))
lookupContextConstructor name = Environment.lookup name <$> asks contextConstructorEnv

type CollectConstraints a = TypeConstraints a TypeIndex () OpaqueType

{-# INLINE runCollectTypeConstraints #-}
runCollectTypeConstraints :: TypeConstraintsContext o k t -> TypeConstraints a o k t e -> (e, [TypeCollectError a], [TypeConstraint (TypeRule k a) o k t])
runCollectTypeConstraints ctx v = (a, lefts outp, rights outp)
 where
  (a, outp) = evalRWS (constraintsMonad v) ctx ()

{-# INLINE evalCollectTypeConstraints #-}
evalCollectTypeConstraints :: TypeConstraintsContext o k t -> TypeConstraints a o k t e -> ([TypeCollectError a], [TypeConstraint (TypeRule k a) o k t])
evalCollectTypeConstraints ctx v = let (_, es, cs) = runCollectTypeConstraints ctx v in (es, cs)

{-# INLINE assertEquality #-}
assertEquality :: TypeRule () a -> [OpaqueType] -> CollectConstraints a ()
assertEquality meta ts = tellRight [Equality meta ts]

assertEqualityAssumptions :: OpaqueType -> [Assumption OpaqueType] -> CollectConstraints a ()
assertEqualityAssumptions t ms =
  tellRight $ do
    Assumption{..} <- ms
    -- TODO
    pure (Equality TypeRule [assumptionType, t])

assertImplicitAssumptions :: OpaqueType -> [Assumption OpaqueType] -> CollectConstraints a ()
assertImplicitAssumptions t ms = do
  set <- asks contextMonomorphicSet
  tellRight $ do
    Assumption{..} <- ms
    -- TODO
    pure (Implicit TypeRule assumptionType t set)

type AssertFn a = OpaqueType -> [Assumption OpaqueType] -> CollectConstraints a ()

patternAssumptions :: AssertFn a -> [Assumption OpaqueType] -> Pattern a OpaqueType -> CollectConstraints a [Assumption OpaqueType]
patternAssumptions assertFn ms =
  \case
    PVariable _ (Label t name) -> do
      let (ls, rs) = partition (assumptionNameIs name) ms
      assertFn t ls
      pure rs
    PConstructor loc (Label t name) ps -> do
      r <- lookupContextConstructor name
      case r of
        Nothing ->
          tellLeft [MissingDataConstructor loc name]
        Just Constructor{..}
          | constructorArity /= length ps ->
              tellLeft [ConstructorArityMismatch loc name constructorArity (length ps)]
        Just Constructor{..} ->
          tellRight [Explicit TypeRule (foldType t (typeOf <$> ps)) constructorScheme]
      concat <$> traverse (patternAssumptions assertFn ms) ps

withMonomorphic :: (TypeIndexed () s) => s -> CollectConstraints a v -> CollectConstraints a v
withMonomorphic s = localMonoset (monosetInsertMany (typeIndexesIn s))

collectTypeConstraints :: Expression a OpaqueType -> CollectConstraints a [Assumption OpaqueType]
collectTypeConstraints =
  \case
    EAnnotation loc t e -> do
      r <- annotationScheme t
      case r of
        Nothing ->
          tellLeft [IllFormedTypeAnnotation loc]
        Just s -> do
          let t1 = typeOf e
          tellRight [Explicit (RuleAnnotation loc t1 s) t1 s]
      collectTypeConstraints e
    EConstructor loc (Label t name) -> do
      r <- lookupContextConstructor name
      case r of
        Nothing ->
          tellLeft [MissingDataConstructor loc name]
        Just Constructor{..} ->
          tellRight [Explicit TypeRule t constructorScheme]
      pure []
    EVariable _ (Label t name) ->
      pure [Assumption name t]
    ELambda loc ps e -> do
      ms1 <- withMonomorphic ps (collectTypeConstraints e)
      concat <$> forM ps (patternAssumptions assertEqualityAssumptions ms1)
    ELet _ gs e1 -> do
      ms1 <- collectTypeConstraints e1
      ms2 <- flip concatMapM gs $
        \case
          BPattern _ p e -> do
            ms <- collectTypeConstraints e
            assertEquality TypeRule [typeOf p, typeOf e]
            pure ms
      ms3 <- flip concatMapM gs $
        \case
          BPattern _ p _ ->
            patternAssumptions assertImplicitAssumptions ms1 p
      pure (ms1 <> ms2 <> ms3)
    EIf loc t e1 e2 e3 -> do
      ms1 <- collectTypeConstraints e1
      ms2 <- collectTypeConstraints e2
      ms3 <- collectTypeConstraints e3
      let t1 = typeOf e1
          t2 = typeOf e2
          t3 = typeOf e3
      assertEquality (RuleIfCondition loc) [t1, TIntrinsic IBool]
      assertEquality (RuleIfBranches loc t2 t3) [t, t2, t3]
      pure (ms1 <> ms2 <> ms3)
    EApplication loc t e1 es -> do
      ms1 <- collectTypeConstraints e1
      ms2 <- concat <$> traverse collectTypeConstraints es
      let t1 = typeOf e1
          t2 = foldType t (typeOf <$> es)
      assertEquality (RuleApplication loc t1 (list1ToList (typeOf <$> es))) [t1, t2]
      pure (ms1 <> ms2)
    ELiteral{} ->
      pure []
    EMatch loc t e cs -> do
      ms1 <- collectTypeConstraints e
      (ts1, ts2, ms2) <- collectClauseTypeConstraints (list1ToList cs)
      -- Pattern types
      assertEquality (RuleMatchClausePatterns loc) (typeOf e : ts1)
      -- Expression types
      assertEquality (RuleMatchClauseExpressions loc) (t : concat ts2)
      pure (ms1 <> ms2)

collectClauseTypeConstraints :: [Clause Expression a OpaqueType] -> CollectConstraints a ([OpaqueType], [[OpaqueType]], [Assumption OpaqueType])
collectClauseTypeConstraints = third3 concat . unzip3 <$$> traverse go
 where
  go (EClause _ p cs) = do
    (ts1, ms1) <- second concat . unzip <$$> withMonomorphic p $
      forM (list1ToList cs) $
        \case
          CPlain _ gs e -> do
            ns1 <- concat <$$> forM gs $ \(CGuard g) -> do
              assertEquality RuleMatchClauseGuard [typeOf g, TIntrinsic IBool]
              collectTypeConstraints g
            ns2 <- collectTypeConstraints e
            pure (typeOf e, ns1 <> ns2)
    ms2 <- patternAssumptions assertEqualityAssumptions ms1 p
    pure (typeOf p, ts1, ms2)

annotationScheme :: (Monad m) => Type TypeVariable () -> m (Maybe (Scheme TypeIndex () OpaqueType))
annotationScheme t = do
  r <- runExceptT (evalStateT (instantiateType t) (0, mempty))
  case r of
    Left{} ->
      pure Nothing
    Right s ->
      pure (Just (Forall (typeIndexesIn s) [] s))

instantiateType :: (MonadError () m, MonadState (Int, Dictionary OpaqueType) m) => Type TypeVariable () -> m OpaqueType
instantiateType =
  \case
    TApplication _ t ts ->
      TApplication () <$> instantiateType t <*> traverse instantiateType ts
    TArrow t1 t2 ->
      TArrow <$> instantiateType t1 <*> instantiateType t2
    TConstructor _ name ->
      pure (TConstructor () name)
    TIntrinsic t ->
      TIntrinsic <$> traverse instantiateType t
    TRow row ->
      TRow <$> instantiateRow row
    TVariable (TypeVariable _ name) -> do
      (n, dict) <- get
      case Map.lookup name dict of
        Nothing -> do
          let t = TVariable (TypeIndex () n)
          put (n + 1, Map.insert name t dict)
          pure t
        Just t@TVariable{} ->
          pure t
        Just _ ->
          throwError ()
    TAlias name ts t ->
      TAlias name <$> traverse instantiateType ts <*> instantiateType t

instantiateRow :: (MonadError () m, MonadState (Int, Dictionary OpaqueType) m) => Row TypeVariable () (Type TypeVariable ()) -> m OpaqueRow
instantiateRow =
  \case
    RExtend name t row ->
      RExtend name <$> instantiateType t <*> instantiateRow row
    RVariable (TypeVariable _ name) -> do
      (n, dict) <- get
      case Map.lookup name dict of
        Nothing -> do
          let r = RVariable (TypeIndex () n)
          put (n + 1, Map.insert name (TRow r) dict)
          pure r
        Just (TRow row) ->
          pure row
        Just _ ->
          throwError ()
    RNil ->
      pure RNil
