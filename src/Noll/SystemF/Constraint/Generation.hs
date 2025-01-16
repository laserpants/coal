{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.SystemF.Constraint.Generation (
  ConstraintsGenerationContext (..),
  ConstraintsGenerationError (..),
  collectConstraints,
  runConstraintsGenerationStack,
) where

import Control.Monad.Reader (asks)
import Data.List (partition)
import Data.Maybe (maybeToList)
import Data.Tuple.Extra (second, third3)
import Debug.Trace
import Noll.Common.List1 (NonEmpty ((:|)), fromList1)
import Noll.Common.Supply (supplied)
import Noll.Label (Label (..))
import Noll.Language (
  BinaryOperator (..),
  Binding (..),
  Choice (..),
  Clause (..),
  Constructor (..),
  Expression (..),
  Guard (..),
  IndexedType,
  Intrinsic (..),
  Kind (..),
  Pattern (..),
  Row (..),
  Scheme (..),
  Type (..),
  TypeIndex (..),
  TypeIndexed (..),
  foldType,
  fromDictionary,
  typeOf,
 )
import Noll.SystemF.Constraint (Constraint (..))
import Noll.SystemF.Constraint.Assumption (
  Assumption (..),
  assumptionNameIs,
  assumptionNameIsNotOneOf,
 )
import Noll.SystemF.Constraint.Generation.Internal (
  ConstraintsGenerationContext (..),
  ConstraintsGenerationError (..),
  ConstraintsGenerationStack (..),
  InferenceRule (..),
  localMonoset,
  monosetInsertMany,
  runConstraintsGenerationStack,
 )
import Noll.SystemF.Constraint.Generation.TypeAnnotation (
  instantiateAnnotation,
 )
import Noll.Utils (
  Map,
  Name,
  concatForM,
  concatMapM,
  forM,
  forM_,
  fromMaybe,
  tellLeft,
  tellRight,
  (<$$>),
 )

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Noll.Common.Environment as Environment

type ConstraintsGeneration a = ConstraintsGenerationStack a TypeIndex Kind IndexedType

{-# INLINE lookupDataConstructor #-}
lookupDataConstructor :: Name -> ConstraintsGenerationStack c o k t (Maybe (Constructor o k t))
lookupDataConstructor name = asks (Environment.lookup name . constraintsGenerationContextDataConstructorEnv)

assertEqualityAssumptions :: a -> IndexedType -> [Assumption IndexedType] -> ConstraintsGeneration a ()
assertEqualityAssumptions loc t ms =
  tellRight $ do
    Assumption{..} <- ms
    -- TODO
    pure (Equality (InferenceRule 1) [assumptionType, t])

assertImplicitAssumptions :: a -> IndexedType -> [Assumption IndexedType] -> ConstraintsGeneration a ()
assertImplicitAssumptions loc t ms = do
  set <- asks constraintsGenerationContextMonomorphicSet
  tellRight $ do
    Assumption{..} <- ms
    -- TODO
    pure (Implicit (InferLetImplicit loc assumptionName assumptionType t) assumptionType t set)

withMonomorphic :: (TypeIndexed Kind t) => t -> ConstraintsGeneration a c -> ConstraintsGeneration a c
withMonomorphic a = localMonoset (monosetInsertMany (typeIndexesIn a))

type Assertion a = IndexedType -> [Assumption IndexedType] -> ConstraintsGeneration a ()

patternConstraints :: Assertion a -> [Assumption IndexedType] -> Pattern a IndexedType -> ConstraintsGeneration a [Name]
patternConstraints assert ms =
  \case
    PAnnotation loc t p -> do
      r <- instantiateAnnotation loc t
      case r of
        Left err ->
          tellLeft [IllFormedTypeAnnotation err]
        Right t1 ->
          tellRight [Equality (InferAnnotation loc t1) [typeOf p, t1]]
      patternConstraints assert ms p
    PVariable _ (Label t name) -> do
      assert t (filter (assumptionNameIs name) ms)
      pure [name]
    PConstructor loc (Label t name) ps -> do
      r <- lookupDataConstructor name
      case r of
        Nothing ->
          tellLeft [NoDataConstructor loc name]
        Just Constructor{..}
          | constructorArity /= length ps ->
              tellLeft [DataConstructorArityMismatch loc name constructorArity (length ps)]
        Just Constructor{..} ->
          tellRight [Explicit (InferenceRule 3) (foldType t (typeOf <$> ps)) constructorScheme]
      concatForM ps (patternConstraints assert ms)
    POr _ t p1 p2 -> do
      tellRight [Equality (InferenceRule 11) [t, typeOf p1, typeOf p2]]
      ps1 <- patternConstraints assert ms p1
      ps2 <- patternConstraints assert ms p2
      pure (ps1 <> ps2)
    PShorthand _ (Label t name) -> do
      assert t (filter (assumptionNameIs name) ms)
      pure [name]
    PRecord _ t d p -> do
      let d1 = pure . typeOf <$> d
          p1 = extractRow . typeOf <$> p
          t1 = TIntrinsic (IRecord (TRow (fromDictionary d1 (fromMaybe RNil p1))))
      forM_ (Map.toList d) $ \(name, e) ->
        assert (typeOf e) (filter (assumptionNameIs name) ms)
      tellRight [Equality (InferenceRule 300) [t, t1]]
      ps1 <- concatForM (Map.elems d <> maybeToList p) (patternConstraints assert ms)
      pure (ps1 <> Map.keys d)
    PShorthand _ (Label t name) -> do
      assert t (filter (assumptionNameIs name) ms)
      pure [name]
    PAny{} ->
      pure []
    PListCons _ t p1 p2 -> do
      ms1 <- patternConstraints assert ms p1
      ms2 <- patternConstraints assert ms p2
      tellRight [Explicit (InferenceRule 3) (foldType t [typeOf p1, typeOf p2]) listScheme]
      pure (ms1 <> ms2)
    PListLiteral _ t ps -> do
      tellRight [Equality (InferenceRule 3) (t : (typeOf <$> ps))]
      concatForM ps (patternConstraints assert ms)
    PAtVariable _ (Label t name) -> do
      pure [name]
    PLiteral{} ->
      pure []

listScheme :: Scheme TypeIndex Kind (Type TypeIndex Kind)
listScheme =
  Forall
    (Set.fromList [TypeIndex KType 0])
    []
    (t0 `TArrow` TIntrinsic (IList t0) `TArrow` TIntrinsic (IList t0))
 where
  t0 = TVariable (TypeIndex KType 0)

-- TODO: move
extractRow :: Type TypeIndex Kind -> Row TypeIndex Kind (Type TypeIndex Kind)
extractRow =
  \case
    TIntrinsic (IRecord (TRow r)) ->
      r
    _ ->
      error "TODO"

clauseAssumptions :: Clause Expression a IndexedType -> ConstraintsGeneration a (IndexedType, [IndexedType], [Assumption IndexedType])
clauseAssumptions (EClause loc p cs) = do
  (ts1, ms) <- second concat . unzip <$$> withMonomorphic p $
    forM (fromList1 cs) $
      \case
        CPlain _ gs e -> do
          ms1 <- concatForM gs $ \(CGuard g) -> do
            tellRight [Equality (InferMatchClauseGuard loc) [typeOf g, TIntrinsic IBool]]
            collectConstraints g
          ms2 <- collectConstraints e
          pure (typeOf e, ms1 <> ms2)
  names <- patternConstraints (assertEqualityAssumptions loc) ms p
  pure (typeOf p, ts1, filter (assumptionNameIsNotOneOf names) ms)

collectConstraints :: Expression a IndexedType -> ConstraintsGeneration a [Assumption IndexedType]
collectConstraints =
  \case
    EAnnotation loc t e -> do
      r <- instantiateAnnotation loc t
      case r of
        Left err ->
          tellLeft [IllFormedTypeAnnotation err]
        Right t1 ->
          tellRight [Equality (InferAnnotation loc t1) [typeOf e, t1]]
      collectConstraints e
    EConstructor loc (Label t name) -> do
      r <- lookupDataConstructor name
      case r of
        Nothing ->
          tellLeft [NoDataConstructor loc name]
        Just Constructor{..} ->
          tellRight [Explicit (InferenceRule 4) t constructorScheme]
      pure []
    EVariable loc (Label t name) ->
      pure [Assumption name t]
    ELambda loc ps e -> do
      ms <- withMonomorphic ps (collectConstraints e)
      names <- concatForM ps (patternConstraints (assertEqualityAssumptions loc) ms)
      pure (filter (assumptionNameIsNotOneOf names) ms)
    ERecursiveLet loc p e1 e2 -> do
      ms1 <- collectConstraints e2
      let t1 = typeOf p
          t2 = typeOf e1
      tellRight [Equality (InferLetBindingPattern loc t1 t2) [t1, t2]]
      ms2 <- collectConstraints e1
      names <- patternConstraints (assertImplicitAssumptions loc) ms1 p
      pure (filter (assumptionNameIsNotOneOf names) (ms1 <> ms2))
    ELet loc gs e1 -> do
      ms1 <- collectConstraints e1
      ms2 <- concatForM gs $
        \case
          BPattern _ p e -> do
            let t1 = typeOf p
                t2 = typeOf e
            tellRight [Equality (InferLetBindingPattern loc t1 t2) [t1, t2]]
            collectConstraints e
          BFunction _ _ ps e -> do
            ms <- withMonomorphic ps (collectConstraints e)
            concatForM ps (patternConstraints (assertEqualityAssumptions loc) ms)
            pure ms
      names <- concatForM gs $
        \case
          BPattern _ p _ ->
            patternConstraints (assertImplicitAssumptions loc) ms1 p
          BFunction _ _ ps e ->
            concatMapM (patternConstraints (assertImplicitAssumptions loc) ms1) ps
      pure (filter (assumptionNameIsNotOneOf names) ms1 <> ms2)
    EIf loc t e1 e2 e3 -> do
      ms1 <- collectConstraints e1
      ms2 <- collectConstraints e2
      ms3 <- collectConstraints e3
      let t1 = typeOf e1
          t2 = typeOf e2
          t3 = typeOf e3
      tellRight [Equality (InferIfCondition loc t1) [t1, TIntrinsic IBool]]
      tellRight [Equality (InferIfBranches loc t2 t3) [t, t2, t3]]
      pure (ms1 <> ms2 <> ms3)
    EApplication loc t e1 es -> do
      ms1 <- collectConstraints e1
      ms2 <- concatMapM collectConstraints es
      let t1 = typeOf e1
          t2 = foldType t ts
          ts = typeOf <$> es
      tellRight [Equality (InferApplication loc t1 (fromList1 ts)) [t1, t2]]
      pure (ms1 <> ms2)
    ELiteral{} ->
      pure []
    EListCons loc t e1 e2 -> do
      ms1 <- collectConstraints e1
      ms2 <- collectConstraints e2
      let t1 = typeOf e1 `TArrow` typeOf e2 `TArrow` t
      tellRight [Explicit (InferenceRule 402) t1 listConstructor]
      pure (ms1 <> ms2)
    EListLiteral loc t es -> do
      ms1 <- concatMapM collectConstraints es
      tellRight [Equality (InferenceRule 555) (t : (typeOf <$> es))]
      pure ms1
    EMatch loc t e cs -> do
      ms1 <- collectConstraints e
      (ts1, ts2, ms2) <- (third3 concat . unzip3 <$$> traverse clauseAssumptions) (fromList1 cs)
      -- Pattern types
      tellRight [Equality (InferMatchClausePatterns loc) (typeOf e : ts1)]
      -- Expression types
      tellRight [Equality (InferMatchClauseExpressions loc) (t : concat ts2)]
      pure (ms1 <> ms2)
    EBinaryOperator loc (t, op) -> do
      tellRight [Explicit (InferBinaryOperator loc) t (binaryOperatorType op)]
      pure []
    ESelect loc (Label t name) e -> do
      rvar <- supplied (RVariable . TypeIndex KRow)
      let t1 = TIntrinsic (IRecord (TRow (RExtend name t rvar)))
      tellRight [Equality (InferenceRule 302) [t1, typeOf e]]
      collectConstraints e
    EFold loc t (e :| es) cs e1 -> do
      ms1 <- collectConstraints e
      ms2 <- concatMapM collectConstraints es
      (ts1, ts2, ms3) <- (third3 concat . unzip3 <$$> traverse clauseAssumptions) (fromList1 cs)
      -- Pattern types
      tellRight [Equality (InferenceRule 401) (typeOf e : ts1)]
      -- Expression types
      tellRight [Equality (InferenceRule 402) (foldType t (typeOf <$> es) : concat ts2)]
      ms4 <- concatMapM collectConstraints e1
      pure (ms1 <> ms2 <> ms3 <> ms4)
    ERecord loc t d e -> do
      ms1 <- concatMapM collectConstraints e
      ms2 <- concatMapM collectConstraints d
      let d1 = pure . typeOf <$> d
          e1 = extractRow . typeOf <$> e
          t1 = TIntrinsic (IRecord (TRow (fromDictionary d1 (fromMaybe RNil e1))))
      tellRight [Equality (InferenceRule 301) [t, t1]]
      pure (ms1 <> ms2)

listConstructor :: Scheme TypeIndex Kind IndexedType
listConstructor =
  Forall
    (Set.fromList [TypeIndex KType 0])
    []
    ( TVariable (TypeIndex KType 0)
        `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 0)))
        `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 0)))
    )

binaryOperatorType :: BinaryOperator -> Scheme TypeIndex Kind IndexedType
binaryOperatorType =
  \case
    OReverseApplication ->
      Forall
        (Set.fromList [TypeIndex KType 0, TypeIndex KType 1])
        []
        ( TVariable (TypeIndex KType 0)
            `TArrow` (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 1))
            `TArrow` TVariable (TypeIndex KType 1)
        )
    OForwardApplication ->
      Forall
        (Set.fromList [TypeIndex KType 0, TypeIndex KType 1])
        []
        ( (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 1))
            `TArrow` TVariable (TypeIndex KType 0)
            `TArrow` TVariable (TypeIndex KType 1)
        )
    OReverseComposition ->
      Forall
        (Set.fromList [TypeIndex KType 0, TypeIndex KType 1, TypeIndex KType 2])
        []
        ( (TVariable (TypeIndex KType 1) `TArrow` TVariable (TypeIndex KType 2))
            `TArrow` (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 1))
            `TArrow` TVariable (TypeIndex KType 0)
            `TArrow` TVariable (TypeIndex KType 2)
        )
    OForwardComposition ->
      Forall
        (Set.fromList [TypeIndex KType 0, TypeIndex KType 1, TypeIndex KType 2])
        []
        ( (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 1))
            `TArrow` (TVariable (TypeIndex KType 1) `TArrow` TVariable (TypeIndex KType 2))
            `TArrow` TVariable (TypeIndex KType 0)
            `TArrow` TVariable (TypeIndex KType 2)
        )
    OLogicalOr ->
      Forall
        mempty
        []
        (TIntrinsic IBool `TArrow` TIntrinsic IBool `TArrow` TIntrinsic IBool)
    OLogicalAnd ->
      Forall
        mempty
        []
        (TIntrinsic IBool `TArrow` TIntrinsic IBool `TArrow` TIntrinsic IBool)
    OListConcatenation ->
      Forall
        (Set.fromList [TypeIndex KType 0])
        []
        ( TIntrinsic (IList (TVariable (TypeIndex KType 0)))
            `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 0)))
            `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 0)))
        )
    OAddition ->
      Forall
        (Set.fromList [TypeIndex KType 0])
        []
        ( TVariable (TypeIndex KType 0)
            `TArrow` TVariable (TypeIndex KType 0)
            `TArrow` TVariable (TypeIndex KType 0)
        )
