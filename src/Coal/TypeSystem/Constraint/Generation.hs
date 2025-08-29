{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.TypeSystem.Constraint.Generation (
  ConstraintsGenContext (..),
  ConstraintsGenError (..),
  collectConstraints,
  runConstraintsGenStack,
  evalConstraintsGenStack,
) where

import Coal.Common.Label (Label (..))
import Coal.Common.List1 (List1, NonEmpty (..), fromList1, (<|))
import Coal.Common.Supply (supplied)
import Coal.Language
import Coal.TypeSystem.Constraint (Constraint (..))
import Coal.TypeSystem.Constraint.Assumption
import Coal.TypeSystem.Constraint.Generation.Internal
import Coal.TypeSystem.Constraint.Generation.TypeAnnotation (instantiateAnnotation)
import Control.Monad.Reader (asks)
import Data.Data (Data)
import Data.Maybe (maybeToList)
import Data.Tuple.Extra (third3)
import Extra

import qualified Coal.Common.Environment as Environment
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

type ConstraintsGen a = ConstraintsGenStack a TypeIndex Kind IndexedType

{-# INLINE lookupDataConstructor #-}
lookupDataConstructor :: Name -> ConstraintsGenStack c o a t (Maybe (DataConstructor o a t))
lookupDataConstructor name = asks (Environment.lookup name . constraintsGenContextDataConstructorEnv)

{-# INLINE lookupCodataAccessor #-}
lookupCodataAccessor :: Name -> ConstraintsGenStack c o a t (Maybe (CodataAccessor o a t))
lookupCodataAccessor name = asks (Environment.lookup name . constraintsGenContextCodataAccessorEnv)

assertEqualityAssumptions :: a -> IndexedType -> [Assumption a IndexedType] -> ConstraintsGen a ()
assertEqualityAssumptions _ t ms =
  tellRight $ do
    Assumption{..} <- ms
    pure (Equality InferenceRulePlaceholder [assumptionType, t])

assertImplicitAssumptions :: a -> IndexedType -> [Assumption a IndexedType] -> ConstraintsGen a ()
assertImplicitAssumptions loc t ms = do
  set <- asks constraintsGenContextMonomorphicSet
  tellRight $ do
    Assumption{..} <- ms
    pure (Implicit (RuleLetImplicit loc assumptionName assumptionType t) assumptionType t set)

withMonomorphic :: (TypeIndexed Kind t) => t -> ConstraintsGen a c -> ConstraintsGen a c
withMonomorphic = localMonoset . monosetInsertMultiple . typeIndexesIn

type Assertion a = IndexedType -> [Assumption a IndexedType] -> ConstraintsGen a ()

emitPAnnotationConstraints :: (Data a) => a -> Type Parameter () -> Pattern a IndexedType -> ConstraintsGen a ()
emitPAnnotationConstraints loc t p = do
  r <- instantiateAnnotation loc t
  case r of
    Left err ->
      tellLeft [EIllFormedTypeAnnotation err]
    Right t1 ->
      tellRight [Equality (RuleAnnotation loc (typeOf p) t1) [typeOf p, t1]]

patternConstraints :: (Data a) => Assertion a -> [Assumption a IndexedType] -> Pattern a IndexedType -> ConstraintsGen a [Name]
patternConstraints assert ms =
  \case
    PAnnotation loc t p -> do
      emitPAnnotationConstraints loc t p
      patternConstraints assert ms p
    PVariable _ (Label t name) -> do
      assert t (filter (assumptionNameIs name) ms)
      pure [name]
    PConstructor loc (Label t name) ps -> do
      r <- lookupDataConstructor name
      case r of
        Nothing ->
          tellLeft [ENoDataConstructor loc name]
        Just DataConstructor{..}
          | constructorArity /= length ps ->
              tellLeft [EDataConstructorArityMismatch loc name constructorArity (length ps)]
        Just DataConstructor{..} ->
          tellRight [Explicit InferenceRulePlaceholder (foldTypeOf t ps) constructorScheme]
      concatForM ps (patternConstraints assert ms)
    POr _ t p1 p2 -> do
      tellRight [Equality InferenceRulePlaceholder [t, typeOf p1, typeOf p2]]
      ps1 <- patternConstraints assert ms p1
      ps2 <- patternConstraints assert ms p2
      pure (ps1 <> ps2)
    PShorthand _ (Label t name) -> do
      assert t (filter (assumptionNameIs name) ms)
      pure [name]
    PRecord _ t fields p -> do
      r1 <- tailRow p
      let t1 = TIntrinsic (IRecord (TRow (fromDictionary (typeOf <$> fields) r1)))
      tellRight [Equality InferenceRulePlaceholder [t, t1]]
      forM_ (Map.toList fields) $
        \(name, p1) ->
          assert (typeOf p1) (filter (assumptionNameIs name) ms)
      case r1 of
        r@RVariable{} ->
          forM_ (Map.keys fields) $
            \field ->
              tellRight [Lacks InferenceRulePlaceholder (TRow r) field]
        _ ->
          pure ()
      ps1 <- concatForM (Map.elems fields <> maybeToList p) (patternConstraints assert ms)
      pure (ps1 <> Map.keys fields)
    PAny{} ->
      pure []
    PListCons _ t p1 p2 -> do
      ms1 <- patternConstraints assert ms p1
      ms2 <- patternConstraints assert ms p2
      tellRight [Explicit InferenceRulePlaceholder (foldTypeOf t [p1, p2]) listConstructorTypeScheme]
      pure (ms1 <> ms2)
    PListLiteral _ t ps -> do
      tellRight
        [ Equality InferenceRulePlaceholder (t : (typeOf <$> ps))
        , Explicit InferenceRulePlaceholder t (forall1 listType)
        ]
      concatForM ps (patternConstraints assert ms)
    PAtVariable _ n (Label _ name) -> do
      pure [name]
    PAs _ (Label t name) p -> do
      ps <- patternConstraints assert ms p
      tellRight [Equality InferenceRulePlaceholder [t, typeOf p]]
      assert t (filter (assumptionNameIs name) ms)
      pure (name : ps)
    PLiteral{} ->
      pure []
    PTuple _ t ps -> do
      tellRight
        [ Equality InferenceRulePlaceholder [t, tupleType (typeOf <$> ps)]
        , Explicit InferenceRulePlaceholder t (tupleScheme (length ps))
        ]
      concatForM ps (patternConstraints assert ms)
    _ ->
      error "TODO"

clauseAssumptions :: (Show a, Data a) => Clause a IndexedType -> ConstraintsGen a (IndexedType, [IndexedType], [Assumption a IndexedType])
clauseAssumptions (EClause loc p cs) = do
  (ts1, ms) <- second concat . unzip <$$> withMonomorphic p $
    forM (fromList1 cs) $
      \case
        CPlain _ gs e -> do
          ms1 <- concatForM gs $ \(CGuard g) -> do
            tellRight [Equality (RuleMatchClauseGuard loc) [typeOf g, TIntrinsic IBool]]
            collectConstraints g
          ms2 <- collectConstraints e
          pure (typeOf e, ms1 <> ms2)
        CLambda{} ->
          error "TODO"
  names <- patternConstraints (assertEqualityAssumptions loc) ms p
  pure (typeOf p, ts1, filter (assumptionNameIsNotOneOf names) ms)

emitEAnnotationConstraints :: (Data a) => a -> Type Parameter () -> Expression a IndexedType -> ConstraintsGen a ()
emitEAnnotationConstraints loc t e = do
  r <- instantiateAnnotation loc t
  case r of
    Left err ->
      tellLeft [EIllFormedTypeAnnotation err]
    Right t1 ->
      tellRight [Equality (RuleAnnotation loc (typeOf e) t1) [typeOf e, t1]]

emitEConstructorConstraints :: a -> Label IndexedType -> ConstraintsGen a [Assumption a IndexedType]
emitEConstructorConstraints loc (Label t name) = do
  r <- lookupDataConstructor name
  case r of
    Nothing ->
      tellLeft [ENoDataConstructor loc name]
    Just DataConstructor{..} ->
      tellRight [Explicit (RuleDataConstructor loc name t constructorScheme) t constructorScheme]
  pure []

emitELambdaConstraints :: (Show a, Data a) => a -> List1 (Pattern a IndexedType) -> Expression a IndexedType -> ConstraintsGen a [Assumption a IndexedType]
emitELambdaConstraints loc ps e = do
  ms <- withMonomorphic ps (collectConstraints e)
  names <- concatForM ps (patternConstraints (assertEqualityAssumptions loc) ms)
  pure (filter (assumptionNameIsNotOneOf names) ms)

emitERecordConstraints :: (Show a, Data a) => a -> IndexedType -> Dictionary (Expression a IndexedType) -> Maybe (Expression a IndexedType) -> ConstraintsGen a [Assumption a IndexedType]
emitERecordConstraints loc t fields expr = do
  ms1 <- concatMapM collectConstraints expr
  ms2 <- concatMapM collectConstraints fields
  r1 <- tailRow expr
  let t1 = TIntrinsic (IRecord (TRow (fromDictionary (typeOf <$> fields) r1)))
  tellRight [Equality InferenceRulePlaceholder [t, t1]]
  case r1 of
    r@RVariable{} ->
      forM_ (Map.keys fields) $
        \field ->
          tellRight [Lacks InferenceRulePlaceholder (TRow r) field]
    _ ->
      pure ()
  pure (ms1 <> ms2)

tailRow :: (HasType TypeIndex Kind t) => Maybe t -> ConstraintsGen a (Row TypeIndex Kind IndexedType)
tailRow =
  \case
    Nothing ->
      pure RNil
    Just t -> do
      r <- supplied (RVariable . TypeIndex KRow)
      tellRight [Equality InferenceRulePlaceholder [TIntrinsic (IRecord (TRow r)), typeOf t]]
      pure r

emitIfConstraints :: (Show a, Data a) => a -> IndexedType -> Expression a IndexedType -> Expression a IndexedType -> Expression a IndexedType -> ConstraintsGen a [Assumption a IndexedType]
emitIfConstraints loc t e1 e2 e3 = do
  ms1 <- collectConstraints e1
  ms2 <- collectConstraints e2
  ms3 <- collectConstraints e3
  let t1 = typeOf e1
      t2 = typeOf e2
      t3 = typeOf e3
  tellRight [Equality (RuleIfCondition loc t1) [t1, TIntrinsic IBool]]
  tellRight [Equality (RuleIfBranches loc t2 t3) [t, t2, t3]]
  pure (ms1 <> ms2 <> ms3)

emitTupleConstraints :: (Show a, Data a) => a -> IndexedType -> List1 (Expression a IndexedType) -> ConstraintsGen a [Assumption a IndexedType]
emitTupleConstraints loc t es = do
  ms1 <- concatMapM collectConstraints es
  tellRight
    [ Equality InferenceRulePlaceholder [t, tupleType (typeOf <$> es)]
    , Explicit InferenceRulePlaceholder t (tupleScheme (length es))
    ]
  pure ms1

tupleScheme :: Int -> IndexedScheme
tupleScheme n = Forall (Set.fromList (fromList1 ixs)) [] (tupleType (TVariable <$> ixs))
 where
  ixs = TypeIndex KType 0 :| [TypeIndex KType ti | ti <- [1 .. n - 1]]

-- TODO: emit
collectConstraints :: (Show a, Data a) => Expression a IndexedType -> ConstraintsGen a [Assumption a IndexedType]
collectConstraints =
  \case
    EAnnotation loc t e -> do
      emitEAnnotationConstraints loc t e
      collectConstraints e
    EConstructor loc ll ->
      emitEConstructorConstraints loc ll
    EVariable loc (Label t name) ->
      pure [Assumption loc name t]
    ELambda loc ps e ->
      emitELambdaConstraints loc ps e
    ERecursiveLet loc p e1 e2 -> do
      ms1 <- collectConstraints e2
      let t1 = typeOf p
          t2 = typeOf e1
      tellRight [Equality (RuleLetBindingPattern loc t1 t2) [t1, t2]]
      ms2 <- collectConstraints e1
      names <- patternConstraints (assertEqualityAssumptions loc) (ms1 <> ms2) p
      pure (filter (assumptionNameIsNotOneOf names) (ms1 <> ms2))
    ELet loc gs e1 -> do
      ms1 <- collectConstraints e1
      ms2 <- concatForM gs $
        \case
          BPattern _ p e -> do
            let t1 = typeOf p
                t2 = typeOf e
            tellRight [Equality (RuleLetBindingPattern loc t1 t2) [t1, t2]]
            collectConstraints e
          BFunction _ _ ps e -> do
            ms <- withMonomorphic ps (collectConstraints e)
            names <- concatForM ps (patternConstraints (assertEqualityAssumptions loc) ms)
            pure (filter (assumptionNameIsNotOneOf names) ms)
      names <- concatForM gs $
        \case
          BPattern _ p _ ->
            patternConstraints (assertImplicitAssumptions loc) ms1 p
          BFunction _ name ps e -> do
            let t1 = foldTypeOf e ps
            assertImplicitAssumptions loc t1 (filter (assumptionNameIs name) ms1)
            names <- concatMapM (patternConstraints (assertEqualityAssumptions loc) ms1) ps
            pure (name : names)
      pure (filter (assumptionNameIsNotOneOf names) ms1 <> ms2)
    EIf loc t e1 e2 e3 ->
      emitIfConstraints loc t e1 e2 e3
    EApplication loc t e1 es -> do
      ms1 <- collectConstraints e1
      ms2 <- concatMapM collectConstraints es
      let t1 = typeOf e1
          t2 = foldType t ts
          ts = typeOf <$> es
      tellRight [Equality (RuleApplication loc t1 (fromList1 ts)) [t1, t2]]
      pure (ms1 <> ms2)
    ELiteral{} ->
      pure []
    EListCons _ t e1 e2 -> do
      ms1 <- collectConstraints e1
      ms2 <- collectConstraints e2
      let t1 = typeOf e1 `TArrow` typeOf e2 `TArrow` t
      tellRight [Explicit InferenceRulePlaceholder t1 listConstructorTypeScheme]
      pure (ms1 <> ms2)
    EListLiteral _ t es -> do
      ms1 <- concatMapM collectConstraints es
      tellRight
        [ Equality InferenceRulePlaceholder (t : (listType . typeOf <$> es))
        , Explicit InferenceRulePlaceholder t (forall1 listType)
        ]
      pure ms1
    EMatch loc t e cs -> do
      ms1 <- collectConstraints e
      (ts1, ts2, ms2) <- (third3 concat . unzip3 <$$> traverse clauseAssumptions) (fromList1 cs)
      -- Pattern types
      tellRight [Equality (RuleMatchClausePatterns loc) (typeOf e : ts1)]
      -- Expression types
      tellRight [Equality (RuleMatchClauseExpressions loc) (t : concat ts2)]
      pure (ms1 <> ms2)
    ECompiledMatch{} ->
      error "TODO"
    EUnaryOperator{} ->
      error "TODO"
    EBinaryOperator loc t op -> do
      tellRight [Explicit (RuleBinaryOperator loc) t (binaryOperatorTypeScheme op)]
      pure []
    ESelect _ (Label t name) e -> do
      rvar <- supplied (RVariable . TypeIndex KRow)
      let t1 = TIntrinsic (IRecord (TRow (RExtend name t rvar)))
      tellRight [Equality InferenceRulePlaceholder [t1, typeOf e]]
      collectConstraints e
    EFold _ t name (e :| es) cs e1 -> do
      ms1 <- collectConstraints e
      ms2 <- concatMapM collectConstraints es
      (ts1, ts2, ms3) <- (third3 concat . unzip3 <$$> traverse clauseAssumptions) (fromList1 cs)
      -- Pattern types
      tellRight [Equality InferenceRulePlaceholder (typeOf e : ts1)]
      -- Expression types
      tellRight [Equality InferenceRulePlaceholder (foldTypeOf t es : concat ts2)]
      ms4 <- concatMapM collectConstraints e1
      case e1 of
        Just (ERecursiveLet _ (PVariable _ (Label t1 _)) _ _) ->
          tellRight [Equality InferenceRulePlaceholder [foldTypeOf t (e :| es), t1]]
        _ ->
          pure ()
      pure (filter (assumptionNameIsNotOneOf [name]) (ms1 <> ms2 <> ms3 <> ms4))
    EUnfold loc t name ps d e1 -> do
      t0 <- supplied (TVariable . TypeIndex KType)
      t1 <- supplied (TVariable . TypeIndex KType)
      let qs = PVariable loc (Label t name) <| ps
      tellRight [Equality InferenceRulePlaceholder [t, foldTypeOf t1 ps]]
      ms1 <- withMonomorphic qs (concatMapM collectConstraints d)

      case e1 of
        Just (ERecursiveLet _ _ (ELambda _ _ (ECodataFields _ _ fields)) _) -> do
          forM_ (Map.toList d) $
            \(name, elem) -> do
              q <- lookupCodataAccessor name
              case (q, Map.lookup ("$_" <> name) fields) of
                (Just CodataAccessor{..}, Just e4) -> do
                  t3 <- supplied (TVariable . TypeIndex KType)
                  tellRight [Explicit InferenceRulePlaceholder (t0 `TArrow` typeOf elem) codataAccessorScheme]
                  tellRight [Equality InferenceRulePlaceholder [typeOf e4, t3 `TArrow` typeOf elem]]
                _ ->
                  tellLeft [ENoCodataAccessor loc name]

      ms2 <- concatMapM collectConstraints e1
      names <- concatForM qs (patternConstraints (assertEqualityAssumptions loc) ms1)

      pure (filter (assumptionNameIsNotOneOf (name : names)) (ms1 <> ms2))
    ECodataFields _ _ d -> do
      concatMapM collectConstraints d
    ERecord loc t d me ->
      emitERecordConstraints loc t d me
    ECodataSelect loc (Label t name) e e1 -> do
      ms1 <- collectConstraints e
      r <- lookupCodataAccessor name
      ms2 <-
        case r of
          Nothing -> do
            tellLeft [ENoCodataAccessor loc name]
            pure []
          Just CodataAccessor{..} -> do
            let t1 = typeOf e `TArrow` t
            tellRight [Explicit InferenceRulePlaceholder t1 codataAccessorScheme]
            case e1 of
              Just (ERecursiveLet _ (PVariable _ (Label t2 n)) e2 e3) -> do
                ms2 <- collectConstraints e2
                ms3 <- collectConstraints e3
                assertEqualityAssumptions loc t2 (filter (assumptionNameIs n) ms3)
                t0 <- supplied (TVariable . TypeIndex KType)
                tellRight [Explicit InferenceRulePlaceholder (t0 `TArrow` typeOf e3) codataAccessorScheme]
                pure (ms2 <> filter (not . assumptionNameIs n) ms3)
              _ ->
                pure []
      pure (ms1 <> ms2)
    ETuple loc t es ->
      emitTupleConstraints loc t es
    _ ->
      error "Not implemented"

listConstructorTypeScheme :: IndexedScheme
listConstructorTypeScheme = forall1 (\a -> a ~> listType a ~> listType a)
