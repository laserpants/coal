{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.TypeSystem.Constraint.Generation (
  ConstraintsGenContext (..),
  ConstraintsGenError (..),
  emitConstraints,
  runConstraintsGenStack,
  evalConstraintsGenStack,
) where

import qualified Coal.Common.Environment as Environment
import Coal.Common.Label (Label (..))
import Coal.Common.Supply (supplied)
import Coal.Language
import Coal.TypeSystem.Constraint (Constraint (..))
import Coal.TypeSystem.Constraint.Assumption
import Coal.TypeSystem.Constraint.Generation.Internal
import Coal.TypeSystem.Constraint.Generation.TypeAnnotation (instantiateAnnotation)
import Control.Monad.Reader (asks)
import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty (..), toList)
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import qualified Data.Text as Text
import Extras

type ConstraintsGen a = ConstraintsGenStack a TypeIndex Kind IndexedType

{-# INLINE lookupDataConstructor #-}
lookupDataConstructor :: Name -> ConstraintsGenStack g o a t (Maybe (DataConstructor o a t))
lookupDataConstructor name = asks (Environment.lookup name . constraintsGenContextDataConstructorEnv)

{-# INLINE lookupCodataAccessor #-}
lookupCodataAccessor :: Name -> ConstraintsGenStack g o a t (Maybe (CodataAccessor o a t))
lookupCodataAccessor name = asks (Environment.lookup name . constraintsGenContextCodataAccessorEnv)

assertEqualityAssumptions :: a -> IndexedType -> [Assumption a IndexedType] -> ConstraintsGen a ()
assertEqualityAssumptions _ t ms =
  tellRight $ do
    Assumption{..} <- ms
    pure (Equality (InferenceRulePlaceholder "assertEqualityAssumptions") [assumptionType, t])

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

emitPConstructorConstraints :: (Data a) => a -> Label IndexedType -> [Pattern a IndexedType] -> ConstraintsGen a ()
emitPConstructorConstraints loc (Label t name) ps = do
  r <- lookupDataConstructor name
  case r of
    Nothing ->
      tellLeft [ENoDataConstructor loc name]
    Just DataConstructor{..}
      | constructorArity /= length ps ->
          tellLeft [EDataConstructorArityMismatch loc name constructorArity (length ps)]
    Just DataConstructor{..} ->
      tellRight [Explicit (InferenceRulePlaceholder "emitPConstructorConstraints") (foldTypeOf t ps) constructorScheme]

emitPOrConstraints :: (Data a) => IndexedType -> Pattern a IndexedType -> Pattern a IndexedType -> ConstraintsGen a ()
emitPOrConstraints t p1 p2 = tellRight [Equality (InferenceRulePlaceholder "emitPOrConstraints") [t, typeOf p1, typeOf p2]]

emitPListConsConstraints :: (Data a) => IndexedType -> Pattern a IndexedType -> Pattern a IndexedType -> ConstraintsGen a ()
emitPListConsConstraints t p1 p2 = tellRight [Explicit (InferenceRulePlaceholder "emitPListConsConstraints") (foldTypeOf t [p1, p2]) listConstructorScheme]

emitPListLiteralConstraints :: (Data a) => IndexedType -> [Pattern a IndexedType] -> ConstraintsGen a ()
emitPListLiteralConstraints t ps =
  tellRight
    [ Equality (InferenceRulePlaceholder "emitPListLiteralConstraints.1") (t : (typeOf <$> ps))
    , Explicit (InferenceRulePlaceholder "emitPListLiteralConstraints.2") t (forall1 listType)
    ]

emitPTupleConstraints :: (Data a) => IndexedType -> NonEmpty (Pattern a IndexedType) -> ConstraintsGen a ()
emitPTupleConstraints t ps =
  tellRight
    [ Equality (InferenceRulePlaceholder "emitPTupleConstraints.1") [t, tupleType (typeOf <$> ps)]
    , Explicit (InferenceRulePlaceholder "emitPTupleConstraints.2") t (tupleScheme (length ps))
    ]

emitPAsConstraints :: (Data a) => IndexedType -> Pattern a IndexedType -> ConstraintsGen a ()
emitPAsConstraints t p = tellRight [Equality (InferenceRulePlaceholder "emitPAsConstraints") [t, typeOf p]]

emitPRecordConstraints :: (Data a) => IndexedType -> Dictionary (Pattern a IndexedType) -> Maybe (Pattern a IndexedType) -> ConstraintsGen a ()
emitPRecordConstraints t fields p = do
  row <- tailRow p
  tellRight [Equality (InferenceRulePlaceholder "emitPRecordConstraints.1") [t, fieldsRecordType (typeOf <$> fields) row]]
  case row of
    r@RVariable{} ->
      forM_ (Map.keys fields) $
        \field ->
          tellRight [Lacks (InferenceRulePlaceholder "emitPRecordConstraints.2") (TRow r) field]
    _ ->
      pure ()

emitPatternConstraints :: (Show a, Data a) => Assertion a -> [Assumption a IndexedType] -> Pattern a IndexedType -> ConstraintsGen a [Name]
emitPatternConstraints assertF ms =
  \case
    PAnnotation loc t p -> do
      emitPAnnotationConstraints loc t p
      emitPatternConstraints assertF ms p
    PVariable _ (Label t name) -> do
      assertF t (filter (assumptionNameIs name) ms)
      pure [name]
    PConstructor loc ll ps -> do
      emitPConstructorConstraints loc ll ps
      concatForM ps (emitPatternConstraints assertF ms)
    POr _ t p1 p2 -> do
      emitPOrConstraints t p1 p2
      ps1 <- emitPatternConstraints assertF ms p1
      ps2 <- emitPatternConstraints assertF ms p2
      pure (ps1 <> ps2)
    PShorthand _ (Label t name) -> do
      assertF t (filter (assumptionNameIs name) ms)
      pure [name]
    PRecord _ t fields p -> do
      emitPRecordConstraints t fields p
      forM_ (Map.toList fields) $
        \(name, p1) ->
          assertF (typeOf p1) (filter (assumptionNameIs name) ms)
      ms1 <- concatForM (Map.elems fields <> maybeToList p) (emitPatternConstraints assertF ms)
      pure (ms1 <> Map.keys fields)
    PAny{} ->
      pure []
    PListCons _ t p1 p2 -> do
      emitPListConsConstraints t p1 p2
      ms1 <- emitPatternConstraints assertF ms p1
      ms2 <- emitPatternConstraints assertF ms p2
      pure (ms1 <> ms2)
    PListLiteral _ t ps -> do
      emitPListLiteralConstraints t ps
      concatForM ps (emitPatternConstraints assertF ms)
    PAtVariable _ (Label _ name) ->
      pure [name]
    PAs _ (Label t name) p -> do
      names <- emitPatternConstraints assertF ms p
      emitPAsConstraints t p
      assertF t (filter (assumptionNameIs name) ms)
      pure (name : names)
    PInteger{} ->
      pure []
    PLiteral{} ->
      pure []
    PTuple _ t ps -> do
      emitPTupleConstraints t ps
      concatForM ps (emitPatternConstraints assertF ms)
    PNamedFold a _ _ -> do
      tellLeft [EFoldPatternInRegularMatch a]
      pure []
    _ ->
      error "Not implemented"

emitEAnnotationConstraints :: (Show a, Data a) => a -> Type Parameter () -> Expression a IndexedType -> ConstraintsGen a [Assumption a IndexedType]
emitEAnnotationConstraints loc t e = do
  r <- instantiateAnnotation loc t
  case r of
    Left err ->
      tellLeft [EIllFormedTypeAnnotation err]
    Right t1 ->
      tellRight [Equality (RuleAnnotation loc (typeOf e) t1) [typeOf e, t1]]
  emitConstraints e

emitEConstructorConstraints :: a -> Label IndexedType -> ConstraintsGen a [Assumption a IndexedType]
emitEConstructorConstraints loc (Label t name) = do
  r <- lookupDataConstructor name
  case r of
    Nothing ->
      tellLeft [ENoDataConstructor loc name]
    Just DataConstructor{..} ->
      tellRight [Explicit (RuleDataConstructor loc name t constructorScheme) t constructorScheme]
  pure []

emitELambdaConstraints :: (Show a, Data a) => a -> NonEmpty (Pattern a IndexedType) -> Expression a IndexedType -> ConstraintsGen a [Assumption a IndexedType]
emitELambdaConstraints loc ps e = do
  ms <- withMonomorphic ps (emitConstraints e)
  names <- concatForM ps (emitPatternConstraints (assertEqualityAssumptions loc) ms)
  pure (filter (assumptionNameIsNotOneOf names) ms)

emitERecursiveLetConstraints :: (Show a, Data a) => a -> Pattern a IndexedType -> Expression a IndexedType -> Expression a IndexedType -> ConstraintsGen a [Assumption a IndexedType]
emitERecursiveLetConstraints loc p e1 e2 = do
  ms1 <- emitConstraints e2
  tellRight [Equality (RuleLetBindingPattern loc t1 t2) [t1, t2]]
  ms2 <- emitConstraints e1
  names <- emitPatternConstraints (assertEqualityAssumptions loc) (ms1 <> ms2) p
  pure (filter (assumptionNameIsNotOneOf names) (ms1 <> ms2))
 where
  t1 = typeOf p
  t2 = typeOf e1

emitELetConstraints :: (Show a, Data a) => a -> NonEmpty (Binding Expression a IndexedType) -> Expression a IndexedType -> ConstraintsGen a [Assumption a IndexedType]
emitELetConstraints loc gs e1 = do
  ms1 <- emitConstraints e1
  ms2 <- concatForM gs $
    \case
      BPattern _ p e -> do
        let t1 = typeOf p
            t2 = typeOf e
        tellRight [Equality (RuleLetBindingPattern loc t1 t2) [t1, t2]]
        emitConstraints e
      BFunction _ _ ps e -> do
        ms <- withMonomorphic ps (emitConstraints e)
        names <- concatForM ps (emitPatternConstraints (assertEqualityAssumptions loc) ms)
        pure (filter (assumptionNameIsNotOneOf names) ms)
  names <- concatForM gs $
    \case
      BPattern _ p _ ->
        emitPatternConstraints (assertImplicitAssumptions loc) ms1 p
      BFunction _ name ps e -> do
        let t1 = foldTypeOf e ps
        assertImplicitAssumptions loc t1 (filter (assumptionNameIs name) ms1)
        names <- concatMapM (emitPatternConstraints (assertEqualityAssumptions loc) ms1) ps
        pure (name : names)
  pure (filter (assumptionNameIsNotOneOf names) ms1 <> ms2)

emitESelectConstraints :: (Show a, Data a) => a -> Label IndexedType -> Expression a IndexedType -> ConstraintsGen a [Assumption a IndexedType]
emitESelectConstraints _ (Label t name) e = do
  row <- supplied (RVariable . TypeIndex KRow)
  let t1 = recordType (RExtend name t row)
  tellRight [Equality (InferenceRulePlaceholder "emitESelectConstraints") [t1, typeOf e]]
  emitConstraints e

emitERecordConstraints :: (Show a, Data a) => a -> IndexedType -> Dictionary (Expression a IndexedType) -> Maybe (Expression a IndexedType) -> ConstraintsGen a [Assumption a IndexedType]
emitERecordConstraints _ t fields expr = do
  ms1 <- concatMapM emitConstraints expr
  ms2 <- concatMapM emitConstraints fields
  r1 <- tailRow expr
  let t1 = TIntrinsic (IRecord (TRow (fromDictionary (typeOf <$> fields) r1)))
  tellRight [Equality (InferenceRulePlaceholder "emitERecordConstraints.1") [t, t1]]
  case r1 of
    r@RVariable{} ->
      forM_ (Map.keys fields) $
        \field ->
          tellRight [Lacks (InferenceRulePlaceholder "emitERecordConstraints.2") (TRow r) field]
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
      tellRight [Equality (InferenceRulePlaceholder "tailRow") [TIntrinsic (IRecord (TRow r)), typeOf t]]
      pure r

emitEIfConstraints :: (Show a, Data a) => a -> IndexedType -> Expression a IndexedType -> Expression a IndexedType -> Expression a IndexedType -> ConstraintsGen a [Assumption a IndexedType]
emitEIfConstraints loc t e1 e2 e3 = do
  ms1 <- emitConstraints e1
  ms2 <- emitConstraints e2
  ms3 <- emitConstraints e3
  tellRight [Equality (RuleIfCondition loc t1) [t1, TIntrinsic IBool]]
  tellRight [Equality (RuleIfBranches loc t2 t3) [t, t2, t3]]
  pure (ms1 <> ms2 <> ms3)
 where
  t1 = typeOf e1
  t2 = typeOf e2
  t3 = typeOf e3

emitEApplicationConstraints :: (Show a, Data a) => a -> IndexedType -> Expression a IndexedType -> NonEmpty (Expression a IndexedType) -> ConstraintsGen a [Assumption a IndexedType]
emitEApplicationConstraints loc t e1 es = do
  ms1 <- emitConstraints e1
  ms2 <- concatMapM emitConstraints es
  tellRight [Equality (RuleApplication loc t1 (toList ts)) [t1, t2]]
  pure (ms1 <> ms2)
 where
  t1 = typeOf e1
  t2 = foldType t ts
  ts = typeOf <$> es

emitEListConsConstraints :: (Show a, Data a) => a -> IndexedType -> Expression a IndexedType -> Expression a IndexedType -> ConstraintsGen a [Assumption a IndexedType]
emitEListConsConstraints _ t e1 e2 = do
  ms1 <- emitConstraints e1
  ms2 <- emitConstraints e2
  tellRight [Explicit (InferenceRulePlaceholder "emitEListConsConstraints") t1 listConstructorScheme]
  pure (ms1 <> ms2)
 where
  t1 = typeOf e1 `TArrow` typeOf e2 `TArrow` t

emitEListLiteralConstraints :: (Show a, Data a) => a -> IndexedType -> [Expression a IndexedType] -> ConstraintsGen a [Assumption a IndexedType]
emitEListLiteralConstraints _ t es = do
  ms1 <- concatMapM emitConstraints es
  tellRight
    [ Equality (InferenceRulePlaceholder "emitEListLiteralConstraints.1") (t : (listType . typeOf <$> es))
    , Explicit (InferenceRulePlaceholder "emitEListLiteralConstraints.2") t (forall1 listType)
    ]
  pure ms1

emitETupleConstraints :: (Show a, Data a) => a -> IndexedType -> NonEmpty (Expression a IndexedType) -> ConstraintsGen a [Assumption a IndexedType]
emitETupleConstraints _ t es = do
  ms1 <- concatMapM emitConstraints es
  tellRight
    [ Equality (InferenceRulePlaceholder "emitETupleConstraints.1") [t, tupleType (typeOf <$> es)]
    , Explicit (InferenceRulePlaceholder "emitETupleConstraints.2") t (tupleScheme (length es))
    ]
  pure ms1

emitClauseConstraints :: (Show a, Data a) => a -> IndexedType -> Expression a IndexedType -> [Expression a IndexedType] -> NonEmpty (Clause a IndexedType) -> ConstraintsGen a [Assumption a IndexedType]
emitClauseConstraints loc t e es cs = do
  ms1 <- emitConstraints e
  (ts1, ts2, ms2) <- unzip3 <$> traverse clauseConstraintsImpl (toList cs)
  -- Pattern types
  tellRight [Equality (RuleMatchClausePatterns loc) (typeOf e : ts1)]
  -- Expression types
  tellRight [Equality (RuleMatchClauseExpressions loc) (foldTypeOf t es : concat ts2)]
  pure (ms1 <> concat ms2)

clauseConstraintsImpl :: (Show a, Data a) => Clause a IndexedType -> ConstraintsGen a (IndexedType, [IndexedType], [Assumption a IndexedType])
clauseConstraintsImpl (EClause loc p cs) = do
  (ts1, ms) <- second concat . unzip <$$> withMonomorphic p $
    forM (toList cs) $
      \case
        CPlain _ gs e -> do
          ms1 <- concatForM gs $
            \(CGuard g) -> do
              tellRight [Equality (RuleMatchClauseGuard loc) [typeOf g, TIntrinsic IBool]]
              emitConstraints g
          ms2 <- emitConstraints e
          pure (typeOf e, ms1 <> ms2)
        CLambda{} ->
          error "TODO"
  names <- emitPatternConstraints (assertEqualityAssumptions loc) ms p
  pure (typeOf p, ts1, filter (assumptionNameIsNotOneOf names) ms)

-- emitECodataSelectConstraints :: (Show a, Data a) => a -> Label IndexedType -> Expression a IndexedType -> Expression a IndexedType -> ConstraintsGen a [Assumption a IndexedType]
-- emitECodataSelectConstraints loc (Label t name) e e1 = do
emitECodataSelectConstraints :: (Show a, Data a) => a -> Label IndexedType -> Expression a IndexedType -> ConstraintsGen a [Assumption a IndexedType]
emitECodataSelectConstraints loc (Label t name) e1 = do
  --  ms1 <- emitConstraints e
  r <- lookupCodataAccessor name
  case r of
    Nothing -> do
      tellLeft [ENoCodataAccessor loc name]
      pure []
    Just CodataAccessor{..} -> do
      -- tellRight [Explicit InferenceRulePlaceholder t1 codataAccessorScheme]
      case e1 of
        ERecursiveLet _ (PVariable _ (Label t2 n)) e2 e3 -> do
          ms2 <- emitConstraints e2
          ms3 <- emitConstraints e3
          assertEqualityAssumptions loc t2 (filter (assumptionNameIs n) ms3)
          t0 <- supplied (TVariable . TypeIndex KType)
          tellRight [Explicit (InferenceRulePlaceholder "emitECodataSelectConstraints.1") (t0 `TArrow` typeOf e3) codataAccessorScheme]
          tellRight [Equality (InferenceRulePlaceholder "emitECodataSelectConstraints.2") [t, typeOf e3]]
          -- pure (ms1 <> ms2 <> filter (not . assumptionNameIs n) ms3)
          pure (ms2 <> filter (not . assumptionNameIs n) ms3)
        _ ->
          pure []

emitEFFICallConstraints :: (Show a, Data a) => a -> Label (Type Parameter ()) -> [Expression a IndexedType] -> Expression a IndexedType -> ConstraintsGen a [Assumption a IndexedType]
emitEFFICallConstraints loc (Label t name) es e =
  -- TODO
  pure []

emitConstraints :: (Show a, Data a) => Expression a IndexedType -> ConstraintsGen a [Assumption a IndexedType]
emitConstraints =
  \case
    EAnnotation loc t e ->
      emitEAnnotationConstraints loc t e
    EConstructor loc ll ->
      emitEConstructorConstraints loc ll
    EVariable loc (Label t name) ->
      pure [Assumption loc name t]
    ELambda loc ps e ->
      emitELambdaConstraints loc ps e
    ERecursiveLet loc p e1 e2 ->
      emitERecursiveLetConstraints loc p e1 e2
    ELet loc gs e1 ->
      emitELetConstraints loc gs e1
    EIf loc t e1 e2 e3 ->
      emitEIfConstraints loc t e1 e2 e3
    EApplication loc t e1 es ->
      emitEApplicationConstraints loc t e1 es
    ELiteral{} ->
      pure []
    EListCons loc t e1 e2 ->
      emitEListConsConstraints loc t e1 e2
    EListLiteral loc t es ->
      emitEListLiteralConstraints loc t es
    EMatch loc t e cs ->
      emitClauseConstraints loc t e [] cs
    ELambdaMatch _ _ _ (Just e) ->
      emitConstraints e
    ECompiledMatch{} ->
      error "Not implemented"
    EUnaryOperator loc t op -> do
      tellRight [Explicit (RuleUnaryOperator loc) t (unaryOperatorTypeScheme op)]
      pure []
    EBinaryOperator loc t op -> do
      tellRight [Explicit (RuleBinaryOperator loc) t (binaryOperatorTypeScheme op)]
      pure []
    ESelect loc ll e ->
      emitESelectConstraints loc ll e
    EFold loc t (e :| es) cs e1 -> do
      ms1 <- emitClauseConstraints loc t e es cs
      ms2 <- concatMapM emitConstraints es
      ms3 <- concatMapM emitConstraints e1
      case e1 of
        Just (ERecursiveLet _ (PVariable _ (Label t1 _)) _ _) ->
          tellRight [Equality (InferenceRulePlaceholder "emitConstraints.1") [foldTypeOf t (e :| es), t1]]
        _ ->
          pure ()
      pure (ms1 <> ms2 <> ms3)
    ECodataRecord loc _ d -> do
      concatForM (Map.toList d) $
        \(field, e) -> do
          env <- asks constraintsGenContextCodataAccessorEnv
          case Environment.lookup (Text.drop 2 field) env of
            Just (CodataAccessor _ s) -> do
              case typeOf e of
                TArrow _ t2 -> do
                  t1 <- supplied (TVariable . TypeIndex KType)
                  tellRight [Explicit (RuleCodataRecord loc (t1 `TArrow` t2) s) (t1 `TArrow` t2) s]
                _ ->
                  error "Implementation error"
            Nothing ->
              pure ()
          emitConstraints e
    ERecord loc t d me ->
      emitERecordConstraints loc t d me
    ECodataSelect loc ll _ (Just e1) ->
      emitECodataSelectConstraints loc ll e1
    ETuple loc t es ->
      emitETupleConstraints loc t es
    EFFICall loc ll es e ->
      emitEFFICallConstraints loc ll es e
    ECodataSelect{} ->
      error "Not implemented"
    EFocus{} ->
      error "Not implemented"
    ETraitDictionary{} ->
      error "Not implemented"
    ELambdaMatch{} ->
      error "Not implemented"
