{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.TypeSystem.Constraint.Generation (
  ConstraintsGenContext (..),
  ConstraintsGenError (..),
  emitConstraints,
  protoOemitConstraints,
  runConstraintsGenStack,
  evalConstraintsGenStack,
) where

import qualified Coal.Common.Environment as Environment
import Coal.Common.Label (Label (..))
import Coal.Common.Supply (supplied)
import Coal.Language
import Coal.ProtoCompiler.ProtoBuild.ProtoNameEntry
import Coal.ProtoTypeSystem.Annotations
import Coal.TypeSystem.Constraint (Constraint (..))
import Coal.TypeSystem.Constraint.Assumption
import Coal.TypeSystem.Constraint.Generation.Annotation (instantiateAnnotation)
import Coal.TypeSystem.Constraint.Generation.Context
import Coal.TypeSystem.Constraint.Generation.Stack
import Coal.TypeSystem.Constraint.Generation.State
import Control.Monad.Reader (ask, asks)
import Control.Monad.State (modify)
import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty (..), toList)
import qualified Data.Map.Strict as Map
import Data.Maybe (maybeToList)
import Extras

type ConstraintsGen a = ConstraintsGenStack a TypeIndex Kind IndexedType

lookupDataConstructor :: Name -> ConstraintsGen a (Maybe (DataConstructor TypeIndex Kind IndexedType))
lookupDataConstructor name = do
  env <- asks constraintsGenContextDataConstructors
  case Environment.lookup name env of
    Nothing ->
      pure Nothing
    Just (ProtoDataConstructorEntry _ _ ctor _) ->
      pure (Just ctor)

assertEqualityAssumptions :: a -> IndexedType -> [Assumption a IndexedType] -> ConstraintsGen a ()
assertEqualityAssumptions loc t ms =
  tellRight $ do
    Assumption{..} <- ms
    pure (Equality (RuleAssumption loc assumptionType t) [assumptionType, t])

assertImplicitAssumptions :: a -> IndexedType -> [Assumption a IndexedType] -> ConstraintsGen a ()
assertImplicitAssumptions loc t ms = do
  set <- asks constraintsGenContextMonomorphicSet
  tellRight $ do
    Assumption{..} <- ms
    pure (Implicit (RuleLetImplicit loc assumptionName assumptionType t) assumptionType t set)

withMonomorphic :: (TypeIndexed Kind t) => t -> ConstraintsGen a c -> ConstraintsGen a c
withMonomorphic = localMonoset . monosetInsertMultiple . typeIndexesIn

type Assertion a = IndexedType -> [Assumption a IndexedType] -> ConstraintsGen a ()

emitPAnnotationConstraints :: (Data a) => a -> Type Parameter () -> Pattern a () IndexedType -> ConstraintsGen a ()
emitPAnnotationConstraints loc t p = do
  r <- instantiateAnnotation loc t
  case r of
    Left err ->
      tellLeft [EIllFormedTypeAnnotation err]
    Right t1 ->
      tellRight [Equality (RuleAnnotation loc (typeOf p) t1) [typeOf p, t1]]

protoOemitPAnnotationConstraints :: (Show a, Data a) => a -> Type Parameter Kind -> Pattern a Kind IndexedType -> ConstraintsGen a ()
protoOemitPAnnotationConstraints loc t p = do
  r <- protoOinstantiateAnnotation loc t
  case r of
    Left err ->
      tellLeft [EIllFormedTypeAnnotation err]
    Right t1 ->
      tellRight [Equality (RuleAnnotation loc (typeOf p) t1) [typeOf p, t1]]

emitPConstructorConstraints :: (Data a) => a -> Label IndexedType -> [Pattern a () IndexedType] -> ConstraintsGen a ()
emitPConstructorConstraints loc (Label t name) ps = do
  r <- lookupDataConstructor name
  case r of
    Nothing ->
      tellLeft [ENoDataConstructor loc name]
    Just DataConstructor{..}
      | constructorArity /= length ps ->
          tellLeft [EDataConstructorArityMismatch loc name constructorArity (length ps)]
    Just DataConstructor{..} -> do
      let t1 = foldTypeOf t ps
      tellRight [Explicit (RuleDataConstructor loc constructorName t1 constructorScheme) t1 constructorScheme]

protoOemitPConstructorConstraints :: (Data a) => a -> Label IndexedType -> [Pattern a Kind IndexedType] -> ConstraintsGen a ()
protoOemitPConstructorConstraints loc (Label t name) ps = do
  r <- lookupDataConstructor name
  case r of
    Nothing -> do
      --      error (show name)
      tellLeft [ENoDataConstructor loc name]
    Just DataConstructor{..}
      | constructorArity /= length ps ->
          tellLeft [EDataConstructorArityMismatch loc name constructorArity (length ps)]
    Just DataConstructor{..} -> do
      let t1 = foldTypeOf t ps
      tellRight [Explicit (RuleDataConstructor loc constructorName t1 constructorScheme) t1 constructorScheme]

emitPOrConstraints :: (Data a) => a -> IndexedType -> Pattern a () IndexedType -> Pattern a () IndexedType -> ConstraintsGen a ()
emitPOrConstraints loc t p1 p2 = do
  tellRight [Equality (RuleOrConstraint loc t1 t2) [t, t1, t2]]
 where
  t1 = typeOf p1
  t2 = typeOf p2

protoOemitPOrConstraints :: (Data a) => a -> IndexedType -> Pattern a Kind IndexedType -> Pattern a Kind IndexedType -> ConstraintsGen a ()
protoOemitPOrConstraints loc t p1 p2 = do
  tellRight [Equality (RuleOrConstraint loc t1 t2) [t, t1, t2]]
 where
  t1 = typeOf p1
  t2 = typeOf p2

emitPListConsConstraints :: (Data a) => a -> IndexedType -> Pattern a () IndexedType -> Pattern a () IndexedType -> ConstraintsGen a ()
emitPListConsConstraints loc t p1 p2 = do
  let t1 = foldTypeOf t [p1, p2]
  tellRight [Explicit (RuleListConstructor loc t1 listConstructorScheme) t1 listConstructorScheme]

protoOemitPListConsConstraints :: (Data a) => a -> IndexedType -> Pattern a Kind IndexedType -> Pattern a Kind IndexedType -> ConstraintsGen a ()
protoOemitPListConsConstraints loc t p1 p2 = do
  let t1 = foldTypeOf t [p1, p2]
  tellRight [Explicit (RuleListConstructor loc t1 listConstructorScheme) t1 listConstructorScheme]

emitPListLiteralConstraints :: (Data a) => a -> IndexedType -> [Pattern a () IndexedType] -> ConstraintsGen a ()
emitPListLiteralConstraints loc t ps =
  case ts of
    t1 : _ ->
      tellRight
        [ Equality (RuleListLiteral loc ts) ts
        , Equality (RuleAssumption loc t t1) [t, t1]
        ]
    _ ->
      pure ()
 where
  ts = listType . typeOf <$> ps

protoOemitPListLiteralConstraints :: (Data a) => a -> IndexedType -> [Pattern a Kind IndexedType] -> ConstraintsGen a ()
protoOemitPListLiteralConstraints loc t ps =
  case ts of
    t1 : _ ->
      tellRight
        [ Equality (RuleListLiteral loc ts) ts
        , Equality (RuleAssumption loc t t1) [t, t1]
        ]
    _ ->
      pure ()
 where
  ts = listType . typeOf <$> ps

emitPTupleConstraints :: (Data a) => a -> IndexedType -> NonEmpty (Pattern a () IndexedType) -> ConstraintsGen a ()
emitPTupleConstraints loc t ps =
  tellRight
    [ Equality (RuleTuple loc t t1) [t, t1]
    ]
 where
  t1 = tupleType (typeOf <$> ps)

protoOemitPTupleConstraints :: (Data a) => a -> IndexedType -> NonEmpty (Pattern a Kind IndexedType) -> ConstraintsGen a ()
protoOemitPTupleConstraints loc t ps =
  tellRight
    [ Equality (RuleTuple loc t t1) [t, t1]
    ]
 where
  t1 = tupleType (typeOf <$> ps)

emitPAsConstraints :: (Data a) => a -> IndexedType -> Pattern a () IndexedType -> ConstraintsGen a ()
emitPAsConstraints loc t p = tellRight [Equality (RuleAsConstraint loc) [t, typeOf p]]

protoOemitPAsConstraints :: (Data a) => a -> IndexedType -> Pattern a Kind IndexedType -> ConstraintsGen a ()
protoOemitPAsConstraints loc t p = tellRight [Equality (RuleAsConstraint loc) [t, typeOf p]]

emitPRecordConstraints :: (Data a) => a -> IndexedType -> Dictionary (Pattern a () IndexedType) -> Maybe (Pattern a () IndexedType) -> ConstraintsGen a ()
emitPRecordConstraints loc t fields p = do
  row <- tailRow loc p
  let t1 = fieldsRecordType (typeOf <$> fields) row
  tellRight [Equality (RuleRecordEquality loc t t1) [t, t1]]
  case row of
    r@RVariable{} ->
      forM_ (Map.keys fields) $
        \field ->
          tellRight [Lacks (RuleRecordField loc field (TRow r)) (TRow r) field]
    _ ->
      pure ()

protoOemitPRecordConstraints :: (Data a) => a -> IndexedType -> Dictionary (Pattern a Kind IndexedType) -> Maybe (Pattern a Kind IndexedType) -> ConstraintsGen a ()
protoOemitPRecordConstraints loc t fields p = do
  row <- tailRow loc p
  let t1 = fieldsRecordType (typeOf <$> fields) row
  tellRight [Equality (RuleRecordEquality loc t t1) [t, t1]]
  case row of
    r@RVariable{} ->
      forM_ (Map.keys fields) $
        \field ->
          tellRight [Lacks (RuleRecordField loc field (TRow r)) (TRow r) field]
    _ ->
      pure ()

protoOemitPatternConstraints :: (Show a, Data a) => Assertion a -> [Assumption a IndexedType] -> Pattern a Kind IndexedType -> ConstraintsGen a [Name]
protoOemitPatternConstraints assertF assumptions =
  \case
    PAnnotation loc t p -> do
      protoOemitPAnnotationConstraints loc t p
      protoOemitPatternConstraints assertF assumptions p
    PVariable _ (Label t name) -> do
      assertF t (filter (assumptionNameIs name) assumptions)
      pure [name]
    PConstructor loc ll ps -> do
      protoOemitPConstructorConstraints loc ll ps
      concatForM ps (protoOemitPatternConstraints assertF assumptions)
    POr loc t p1 p2 -> do
      protoOemitPOrConstraints loc t p1 p2
      ps1 <- protoOemitPatternConstraints assertF assumptions p1
      ps2 <- protoOemitPatternConstraints assertF assumptions p2
      pure (ps1 <> ps2)
    PShorthand _ (Label t name) -> do
      assertF t (filter (assumptionNameIs name) assumptions)
      pure [name]
    PRecord loc t fields p -> do
      protoOemitPRecordConstraints loc t fields p
      forM_ (Map.toList fields) $
        \(name, p1) ->
          assertF (typeOf p1) (filter (assumptionNameIs name) assumptions)
      concatForM (Map.elems fields <> maybeToList p) (protoOemitPatternConstraints assertF assumptions)
    PAny{} ->
      pure []
    PListCons loc t p1 p2 -> do
      protoOemitPListConsConstraints loc t p1 p2
      assumptions1 <- protoOemitPatternConstraints assertF assumptions p1
      assumptions2 <- protoOemitPatternConstraints assertF assumptions p2
      pure (assumptions1 <> assumptions2)
    PListLiteral loc t ps -> do
      protoOemitPListLiteralConstraints loc t ps
      concatForM ps (protoOemitPatternConstraints assertF assumptions)
    PAtVariable _ (Label _ name) ->
      pure [name]
    PAs loc (Label t name) p -> do
      names <- protoOemitPatternConstraints assertF assumptions p
      protoOemitPAsConstraints loc t p
      assertF t (filter (assumptionNameIs name) assumptions)
      pure (name : names)
    PInteger{} ->
      pure []
    PLiteral{} ->
      pure []
    PTuple loc t ps -> do
      protoOemitPTupleConstraints loc t ps
      concatForM ps (protoOemitPatternConstraints assertF assumptions)
    PNamedFold a _ _ -> do
      tellLeft [EFoldPatternInRegularMatch a]
      pure []
    _ ->
      error "Not implemented"

emitPatternConstraints :: (Show a, Data a) => Assertion a -> [Assumption a IndexedType] -> Pattern a () IndexedType -> ConstraintsGen a [Name]
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
    POr loc t p1 p2 -> do
      emitPOrConstraints loc t p1 p2
      ps1 <- emitPatternConstraints assertF ms p1
      ps2 <- emitPatternConstraints assertF ms p2
      pure (ps1 <> ps2)
    PShorthand _ (Label t name) -> do
      assertF t (filter (assumptionNameIs name) ms)
      pure [name]
    PRecord loc t fields p -> do
      emitPRecordConstraints loc t fields p
      forM_ (Map.toList fields) $
        \(name, p1) ->
          assertF (typeOf p1) (filter (assumptionNameIs name) ms)
      concatForM (Map.elems fields <> maybeToList p) (emitPatternConstraints assertF ms)
    PAny{} ->
      pure []
    PListCons loc t p1 p2 -> do
      emitPListConsConstraints loc t p1 p2
      ms1 <- emitPatternConstraints assertF ms p1
      ms2 <- emitPatternConstraints assertF ms p2
      pure (ms1 <> ms2)
    PListLiteral loc t ps -> do
      emitPListLiteralConstraints loc t ps
      concatForM ps (emitPatternConstraints assertF ms)
    PAtVariable _ (Label _ name) ->
      pure [name]
    PAs loc (Label t name) p -> do
      names <- emitPatternConstraints assertF ms p
      emitPAsConstraints loc t p
      assertF t (filter (assumptionNameIs name) ms)
      pure (name : names)
    PInteger{} ->
      pure []
    PLiteral{} ->
      pure []
    PTuple loc t ps -> do
      emitPTupleConstraints loc t ps
      concatForM ps (emitPatternConstraints assertF ms)
    PNamedFold a _ _ -> do
      tellLeft [EFoldPatternInRegularMatch a]
      pure []
    _ ->
      error "Not implemented"

emitEAnnotationConstraints :: (Show a, Data a) => a -> Type Parameter () -> Expression a () IndexedType -> ConstraintsGen a [Assumption a IndexedType]
emitEAnnotationConstraints loc t e = do
  r <- instantiateAnnotation loc t
  case r of
    Left err ->
      tellLeft [EIllFormedTypeAnnotation err]
    Right t1 ->
      tellRight [Equality (RuleAnnotation loc (typeOf e) t1) [typeOf e, t1]]
  emitConstraints e

protoOemitEAnnotationConstraints :: (Show a, Data a) => a -> Type Parameter Kind -> Expression a Kind IndexedType -> ConstraintsGen a [Assumption a IndexedType]
protoOemitEAnnotationConstraints loc t e = do
  r <- protoOinstantiateAnnotation loc t
  case r of
    Left err ->
      tellLeft [EIllFormedTypeAnnotation err]
    Right t1 ->
      tellRight [Equality (RuleAnnotation loc (typeOf e) t1) [typeOf e, t1]]
  protoOemitConstraints e

emitEConstructorConstraints :: a -> Label IndexedType -> ConstraintsGen a [Assumption a IndexedType]
emitEConstructorConstraints loc (Label t name) = do
  r <- lookupDataConstructor name
  case r of
    Nothing ->
      tellLeft [ENoDataConstructor loc name]
    Just DataConstructor{..} ->
      tellRight [Explicit (RuleDataConstructor loc name t constructorScheme) t constructorScheme]
  pure []

emitELambdaConstraints :: (Show a, Data a) => a -> NonEmpty (Pattern a () IndexedType) -> Expression a () IndexedType -> ConstraintsGen a [Assumption a IndexedType]
emitELambdaConstraints loc ps e = do
  ms <- withMonomorphic ps (emitConstraints e)
  names <- concatForM ps (emitPatternConstraints (assertEqualityAssumptions loc) ms)
  pure (filter (assumptionNameIsNotOneOf names) ms)

protoOemitELambdaConstraints :: (Show a, Data a) => a -> NonEmpty (Pattern a Kind IndexedType) -> Expression a Kind IndexedType -> ConstraintsGen a [Assumption a IndexedType]
protoOemitELambdaConstraints loc ps e = do
  ms <- withMonomorphic ps (protoOemitConstraints e)
  names <- concatForM ps (protoOemitPatternConstraints (assertEqualityAssumptions loc) ms)
  pure (filter (assumptionNameIsNotOneOf names) ms)

emitERecursiveLetConstraints :: (Show a, Data a) => a -> Pattern a () IndexedType -> Expression a () IndexedType -> Expression a () IndexedType -> ConstraintsGen a [Assumption a IndexedType]
emitERecursiveLetConstraints loc p e1 e2 = do
  ms1 <- emitConstraints e2
  tellRight [Equality (RuleLetBindingPattern loc t1 t2) [t1, t2]]
  ms2 <- emitConstraints e1
  names <- emitPatternConstraints (assertEqualityAssumptions loc) (ms1 <> ms2) p
  pure (filter (assumptionNameIsNotOneOf names) (ms1 <> ms2))
 where
  t1 = typeOf p
  t2 = typeOf e1

protoOemitERecursiveLetConstraints :: (Show a, Data a) => a -> Pattern a Kind IndexedType -> Expression a Kind IndexedType -> Expression a Kind IndexedType -> ConstraintsGen a [Assumption a IndexedType]
protoOemitERecursiveLetConstraints loc p e1 e2 = do
  ms1 <- protoOemitConstraints e2
  tellRight [Equality (RuleLetBindingPattern loc t1 t2) [t1, t2]]
  ms2 <- protoOemitConstraints e1
  names <- protoOemitPatternConstraints (assertEqualityAssumptions loc) (ms1 <> ms2) p
  pure (filter (assumptionNameIsNotOneOf names) (ms1 <> ms2))
 where
  t1 = typeOf p
  t2 = typeOf e1

normalizeBinding :: (Data a) => Binding Expression a () IndexedType -> Binding Expression a () IndexedType
normalizeBinding =
  \case
    b@BPattern{} ->
      b
    BFunction loc name ps e ->
      BPattern loc (PVariable loc (Label (foldTypeOf e ps) name)) (ELambda loc ps e)

protoOnormalizeBinding :: (Data a) => Binding Expression a Kind IndexedType -> Binding Expression a Kind IndexedType
protoOnormalizeBinding =
  \case
    b@BPattern{} ->
      b
    BFunction loc name ps e ->
      BPattern loc (PVariable loc (Label (foldTypeOf e ps) name)) (ELambda loc ps e)

emitELetConstraints :: (Show a, Data a) => a -> NonEmpty (Binding Expression a () IndexedType) -> Expression a () IndexedType -> ConstraintsGen a [Assumption a IndexedType]
emitELetConstraints loc gs e1 = do
  let gs' = normalizeBinding <$> gs
  ms1 <- emitConstraints e1
  ms2 <- concatForM gs' $
    \case
      BPattern _ p e -> do
        let t1 = typeOf p
            t2 = typeOf e
        tellRight [Equality (RuleLetBindingPattern loc t1 t2) [t1, t2]]
        emitConstraints e
      BFunction{} ->
        error "Implementation error"
  names <- concatForM gs' $
    \case
      BPattern _ p _ ->
        emitPatternConstraints (assertImplicitAssumptions loc) ms1 p
      BFunction{} ->
        error "Implementation error"
  pure (filter (assumptionNameIsNotOneOf names) ms1 <> ms2)

protoOemitELetConstraints :: (Show a, Data a) => a -> NonEmpty (Binding Expression a Kind IndexedType) -> Expression a Kind IndexedType -> ConstraintsGen a [Assumption a IndexedType]
protoOemitELetConstraints loc gs e1 = do
  let gs' = protoOnormalizeBinding <$> gs
  ms1 <- protoOemitConstraints e1
  ms2 <- concatForM gs' $
    \case
      BPattern _ p e -> do
        let t1 = typeOf p
            t2 = typeOf e
        tellRight [Equality (RuleLetBindingPattern loc t1 t2) [t1, t2]]
        protoOemitConstraints e
      BFunction{} ->
        error "Implementation error"
  names <- concatForM gs' $
    \case
      BPattern _ p _ ->
        protoOemitPatternConstraints (assertImplicitAssumptions loc) ms1 p
      BFunction{} ->
        error "Implementation error"
  pure (filter (assumptionNameIsNotOneOf names) ms1 <> ms2)

emitESelectConstraints :: (Show a, Data a) => a -> Label IndexedType -> Expression a () IndexedType -> ConstraintsGen a [Assumption a IndexedType]
emitESelectConstraints loc (Label t name) e = do
  row <- supplied (RVariable . TypeIndex KRow)
  let t1 = recordType (RExtend name t row)
      t2 = typeOf e
  tellRight [Equality (RuleSelectEquality loc t1 t2) [t1, t2]]
  emitConstraints e

protoOemitESelectConstraints :: (Show a, Data a) => a -> Label IndexedType -> Expression a Kind IndexedType -> ConstraintsGen a [Assumption a IndexedType]
protoOemitESelectConstraints loc (Label t name) e = do
  row <- supplied (RVariable . TypeIndex KRow)
  let t1 = recordType (RExtend name t row)
      t2 = typeOf e
  tellRight [Equality (RuleSelectEquality loc t1 t2) [t1, t2]]
  protoOemitConstraints e

emitERecordConstraints :: (Show a, Data a) => a -> IndexedType -> Dictionary (Expression a () IndexedType) -> Maybe (Expression a () IndexedType) -> ConstraintsGen a [Assumption a IndexedType]
emitERecordConstraints loc t fields expr = do
  ms1 <- concatMapM emitConstraints expr
  ms2 <- concatMapM emitConstraints fields
  r1 <- tailRow loc expr
  let t1 = TRecord (TRow (fromDictionary (typeOf <$> fields) r1))
  tellRight [Equality (RuleRecordEquality loc t t1) [t, t1]]
  case r1 of
    r@RVariable{} ->
      forM_ (Map.keys fields) $
        \field ->
          tellRight [Lacks (RuleRecordLacks loc field (TRow r)) (TRow r) field]
    _ ->
      pure ()
  pure (ms1 <> ms2)

protoOemitERecordConstraints :: (Show a, Data a) => a -> IndexedType -> Dictionary (Expression a Kind IndexedType) -> Maybe (Expression a Kind IndexedType) -> ConstraintsGen a [Assumption a IndexedType]
protoOemitERecordConstraints loc t fields expr = do
  ms1 <- concatMapM protoOemitConstraints expr
  ms2 <- concatMapM protoOemitConstraints fields
  r1 <- tailRow loc expr
  let t1 = TRecord (TRow (fromDictionary (typeOf <$> fields) r1))
  tellRight [Equality (RuleRecordEquality loc t t1) [t, t1]]
  case r1 of
    r@RVariable{} ->
      forM_ (Map.keys fields) $
        \field ->
          tellRight [Lacks (RuleRecordLacks loc field (TRow r)) (TRow r) field]
    _ ->
      pure ()
  pure (ms1 <> ms2)

tailRow :: (HasType TypeIndex Kind t) => a -> Maybe t -> ConstraintsGen a (Row TypeIndex Kind IndexedType)
tailRow loc =
  \case
    Nothing ->
      pure RNil
    Just t -> do
      r <- supplied (RVariable . TypeIndex KRow)
      let t1 = TRecord (TRow r)
          t2 = typeOf t
      tellRight [Equality (RuleTailRow loc t1 t2) [t1, t2]]
      pure r

emitEIfConstraints :: (Show a, Data a) => a -> IndexedType -> Expression a () IndexedType -> Expression a () IndexedType -> Expression a () IndexedType -> ConstraintsGen a [Assumption a IndexedType]
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

protoOemitEIfConstraints :: (Show a, Data a) => a -> IndexedType -> Expression a Kind IndexedType -> Expression a Kind IndexedType -> Expression a Kind IndexedType -> ConstraintsGen a [Assumption a IndexedType]
protoOemitEIfConstraints loc t e1 e2 e3 = do
  ms1 <- protoOemitConstraints e1
  ms2 <- protoOemitConstraints e2
  ms3 <- protoOemitConstraints e3
  tellRight [Equality (RuleIfCondition loc t1) [t1, TIntrinsic IBool]]
  tellRight [Equality (RuleIfBranches loc t2 t3) [t, t2, t3]]
  pure (ms1 <> ms2 <> ms3)
 where
  t1 = typeOf e1
  t2 = typeOf e2
  t3 = typeOf e3

emitEApplicationConstraints :: (Show a, Data a) => a -> IndexedType -> Expression a () IndexedType -> NonEmpty (Expression a () IndexedType) -> ConstraintsGen a [Assumption a IndexedType]
emitEApplicationConstraints loc t e1 es = do
  ms1 <- emitConstraints e1
  ms2 <- concatMapM emitConstraints es
  tellRight [Equality (RuleApplication loc t1 (toList ts)) [t1, t2]]
  pure (ms1 <> ms2)
 where
  t1 = typeOf e1
  t2 = foldType t ts
  ts = typeOf <$> es

protoOemitEApplicationConstraints :: (Show a, Data a) => a -> IndexedType -> Expression a Kind IndexedType -> NonEmpty (Expression a Kind IndexedType) -> ConstraintsGen a [Assumption a IndexedType]
protoOemitEApplicationConstraints loc t e1 es = do
  ms1 <- protoOemitConstraints e1
  ms2 <- concatMapM protoOemitConstraints es
  tellRight [Equality (RuleApplication loc t1 (toList ts)) [t1, t2]]
  pure (ms1 <> ms2)
 where
  t1 = typeOf e1
  t2 = foldType t ts
  ts = typeOf <$> es

emitEListConsConstraints :: (Show a, Data a) => a -> IndexedType -> Expression a () IndexedType -> Expression a () IndexedType -> ConstraintsGen a [Assumption a IndexedType]
emitEListConsConstraints loc t e1 e2 = do
  ms1 <- emitConstraints e1
  ms2 <- emitConstraints e2
  tellRight [Explicit (RuleListConstructor loc t1 listConstructorScheme) t1 listConstructorScheme]
  pure (ms1 <> ms2)
 where
  t1 = typeOf e1 `TArrow` typeOf e2 `TArrow` t

protoOemitEListConsConstraints :: (Show a, Data a) => a -> IndexedType -> Expression a Kind IndexedType -> Expression a Kind IndexedType -> ConstraintsGen a [Assumption a IndexedType]
protoOemitEListConsConstraints loc t e1 e2 = do
  ms1 <- protoOemitConstraints e1
  ms2 <- protoOemitConstraints e2
  tellRight [Explicit (RuleListConstructor loc t1 listConstructorScheme) t1 listConstructorScheme]
  pure (ms1 <> ms2)
 where
  t1 = typeOf e1 `TArrow` typeOf e2 `TArrow` t

emitEListLiteralConstraints :: (Show a, Data a) => a -> IndexedType -> [Expression a () IndexedType] -> ConstraintsGen a [Assumption a IndexedType]
emitEListLiteralConstraints loc t es = do
  case ts of
    t1 : _ ->
      tellRight
        [ Equality (RuleListLiteral loc ts) ts
        , Equality (RuleAssumption loc t t1) [t, t1]
        ]
    _ ->
      pure ()
  concatMapM emitConstraints es
 where
  ts = listType . typeOf <$> es

protoOemitEListLiteralConstraints :: (Show a, Data a) => a -> IndexedType -> [Expression a Kind IndexedType] -> ConstraintsGen a [Assumption a IndexedType]
protoOemitEListLiteralConstraints loc t es = do
  case ts of
    t1 : _ ->
      tellRight
        [ Equality (RuleListLiteral loc ts) ts
        , Equality (RuleAssumption loc t t1) [t, t1]
        ]
    _ ->
      pure ()
  concatMapM protoOemitConstraints es
 where
  ts = listType . typeOf <$> es

emitETupleConstraints :: (Show a, Data a) => a -> IndexedType -> NonEmpty (Expression a () IndexedType) -> ConstraintsGen a [Assumption a IndexedType]
emitETupleConstraints loc t es = do
  tellRight
    [ Equality (RuleTuple loc t t1) [t, t1]
    ]
  concatMapM emitConstraints es
 where
  t1 = tupleType (typeOf <$> es)

protoOemitETupleConstraints :: (Show a, Data a) => a -> IndexedType -> NonEmpty (Expression a Kind IndexedType) -> ConstraintsGen a [Assumption a IndexedType]
protoOemitETupleConstraints loc t es = do
  tellRight
    [ Equality (RuleTuple loc t t1) [t, t1]
    ]
  concatMapM protoOemitConstraints es
 where
  t1 = tupleType (typeOf <$> es)

emitClauseConstraints :: (Show a, Data a) => a -> IndexedType -> Expression a () IndexedType -> [Expression a () IndexedType] -> NonEmpty (Clause a () IndexedType) -> ConstraintsGen a [Assumption a IndexedType]
emitClauseConstraints loc t e es cs = do
  ms1 <- emitConstraints e
  (ts1, ts2, ms2) <- unzip3 <$> traverse clauseConstraintsImpl (toList cs)
  -- Pattern types
  tellRight [Equality (RuleMatchClausePatterns loc) (typeOf e : ts1)]
  -- Expression types
  tellRight [Equality (RuleMatchClauseExpressions loc) (foldTypeOf t es : concat ts2)]
  pure (ms1 <> concat ms2)

protoOemitClauseConstraints :: (Show a, Data a) => a -> IndexedType -> Expression a Kind IndexedType -> [Expression a Kind IndexedType] -> NonEmpty (Clause a Kind IndexedType) -> ConstraintsGen a [Assumption a IndexedType]
protoOemitClauseConstraints loc t e es cs = do
  ms1 <- protoOemitConstraints e
  (ts1, ts2, ms2) <- unzip3 <$> traverse protoOclauseConstraintsImpl (toList cs)
  -- Pattern types
  tellRight [Equality (RuleMatchClausePatterns loc) (typeOf e : ts1)]
  -- Expression types
  tellRight [Equality (RuleMatchClauseExpressions loc) (foldTypeOf t es : concat ts2)]
  pure (ms1 <> concat ms2)

clauseConstraintsImpl :: (Show a, Data a) => Clause a () IndexedType -> ConstraintsGen a (IndexedType, [IndexedType], [Assumption a IndexedType])
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
  names <- emitPatternConstraints (assertEqualityAssumptions loc) ms p
  pure (typeOf p, ts1, filter (assumptionNameIsNotOneOf names) ms)

protoOclauseConstraintsImpl :: (Show a, Data a) => Clause a Kind IndexedType -> ConstraintsGen a (IndexedType, [IndexedType], [Assumption a IndexedType])
protoOclauseConstraintsImpl (EClause loc p cs) = do
  (ts1, ms) <- second concat . unzip <$$> withMonomorphic p $
    forM (toList cs) $
      \case
        CPlain _ gs e -> do
          ms1 <- concatForM gs $
            \(CGuard g) -> do
              tellRight [Equality (RuleMatchClauseGuard loc) [typeOf g, TIntrinsic IBool]]
              protoOemitConstraints g
          ms2 <- protoOemitConstraints e
          pure (typeOf e, ms1 <> ms2)
  names <- protoOemitPatternConstraints (assertEqualityAssumptions loc) ms p
  pure (typeOf p, ts1, filter (assumptionNameIsNotOneOf names) ms)

emitEFFICallConstraints :: (Show a, Data a) => a -> IndexedType -> Label (Type Parameter ()) -> [Expression a () IndexedType] -> Expression a () IndexedType -> ConstraintsGen a [Assumption a IndexedType]
emitEFFICallConstraints loc u (Label t _) es e = do
  ms1 <- emitConstraints e
  ms2 <- concatMapM emitConstraints es
  r <- instantiateAnnotation loc t
  case r of
    Left err -> do
      tellLeft [EIllFormedTypeAnnotation err]
      pure []
    Right t1 -> do
      t0 <- supplied (TVariable . TypeIndex KType)
      let t2 = foldTypeOf t0 es
          t3 = t0 `TArrow` u
          t4 = typeOf e
      tellRight [Equality (RuleAnnotation loc t2 t1) [t2, t1]]
      tellRight [Equality (RuleAnnotation loc t3 t4) [t3, t4]]
      pure (ms1 <> ms2)

protoOemitEFFICallConstraints :: (Show a, Data a) => a -> IndexedType -> Label (Type Parameter Kind) -> [Expression a Kind IndexedType] -> Expression a Kind IndexedType -> ConstraintsGen a [Assumption a IndexedType]
protoOemitEFFICallConstraints loc u (Label t _) es e = do
  ms1 <- protoOemitConstraints e
  ms2 <- concatMapM protoOemitConstraints es
  r <- protoOinstantiateAnnotation loc t
  case r of
    Left err -> do
      tellLeft [EIllFormedTypeAnnotation err]
      pure []
    Right t1 -> do
      t0 <- supplied (TVariable . TypeIndex KType)
      let t2 = foldTypeOf t0 es
          t3 = t0 `TArrow` u
          t4 = typeOf e
      tellRight [Equality (RuleAnnotation loc t2 t1) [t2, t1]]
      tellRight [Equality (RuleAnnotation loc t3 t4) [t3, t4]]
      pure (ms1 <> ms2)

protoOemitConstraints :: (Show a, Data a) => Expression a Kind IndexedType -> ConstraintsGen a [Assumption a IndexedType]
protoOemitConstraints =
  \case
    EAnnotation loc t e ->
      protoOemitEAnnotationConstraints loc t e
    EConstructor loc ll ->
      emitEConstructorConstraints loc ll
    EVariable loc (Label t name) ->
      pure [Assumption loc name t]
    ELambda loc ps e ->
      protoOemitELambdaConstraints loc ps e
    ERecursiveLet loc p e1 e2 ->
      protoOemitERecursiveLetConstraints loc p e1 e2
    ELet loc gs e1 ->
      protoOemitELetConstraints loc gs e1
    EIf loc t e1 e2 e3 ->
      protoOemitEIfConstraints loc t e1 e2 e3
    EApplication loc t e1 es ->
      protoOemitEApplicationConstraints loc t e1 es
    ELiteral{} ->
      pure []
    EListCons loc t e1 e2 ->
      protoOemitEListConsConstraints loc t e1 e2
    EListLiteral loc t es ->
      protoOemitEListLiteralConstraints loc t es
    EMatch loc t e cs ->
      protoOemitClauseConstraints loc t e [] cs
    EOperator loc t op -> do
      tellRight [Explicit (RuleOperator loc) t (operatorTypeScheme op)]
      pure []
    ESelect loc ll e ->
      protoOemitESelectConstraints loc ll e
    ERecord loc t d me ->
      protoOemitERecordConstraints loc t d me
    ETuple loc t es ->
      protoOemitETupleConstraints loc t es
    EFFICall loc t ll es e ->
      protoOemitEFFICallConstraints loc t ll es e
    EFocus{} ->
      error "Implementation error"
    ETraitInstance{} ->
      error "Implementation error"
    ELambdaMatch{} ->
      error "Implementation error"
    EDoBlock{} ->
      error "Implementation error"
    ECompiledMatch{} ->
      error "Implementation error"
    EFold{} ->
      error "Implementation error"

emitConstraints :: (Show a, Data a) => Expression a () IndexedType -> ConstraintsGen a [Assumption a IndexedType]
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
    EOperator loc t op -> do
      tellRight [Explicit (RuleOperator loc) t (operatorTypeScheme op)]
      pure []
    ESelect loc ll e ->
      emitESelectConstraints loc ll e
    ERecord loc t d me ->
      emitERecordConstraints loc t d me
    ETuple loc t es ->
      emitETupleConstraints loc t es
    EFFICall loc t ll es e ->
      emitEFFICallConstraints loc t ll es e
    EFocus{} ->
      error "Implementation error"
    ETraitInstance{} ->
      error "Implementation error"
    ELambdaMatch{} ->
      error "Implementation error"
    EDoBlock{} ->
      error "Implementation error"
    ECompiledMatch{} ->
      error "Implementation error"
    EFold{} ->
      error "Implementation error"

protoOinstantiateAnnotation :: (Show a) => a -> Type Parameter Kind -> ConstraintsGen a (Either (TypeAnnotationError a) (Type TypeIndex Kind))
protoOinstantiateAnnotation loc a = do
  env <- ask
  (t, s) <- runAnnotationsT loc env (indexTypeAnnotations a)
  forM_ (Map.toList s) $
    \(n, k) ->
      modify (overConstraintsGenStateTypeIndexes (Map.insert n (loc, k)))
  return t
