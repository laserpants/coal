{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.SystemF.Constraint.Generation (
  ConstraintsGenContext (..),
  ConstraintsGenError (..),
  collectConstraints,
  runConstraintsGenStack,
) where

import Control.Monad.Reader (asks)
import Data.Data (Data)
import Data.Maybe (maybeToList)
import Data.Tuple.Extra (second, third3)
import Debug.Trace
import Lang.Common.List1 (NonEmpty ((:|)), fromList1)
import Lang.Common.Supply (supplied)
import Lang.Label (Label (..))
import Lang.Utils (
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
import Noll.Ast.HasType (foldTypeOf)
import Noll.Language (
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
  binaryOperatorTypeScheme,
  foldType,
  forall1,
  fromDictionary,
  typeOf,
  (~>),
 )
import Noll.SystemF.Constraint (Constraint (..))
import Noll.SystemF.Constraint.Assumption (
  Assumption (..),
  assumptionNameIs,
  assumptionNameIsNotOneOf,
 )
import Noll.SystemF.Constraint.Generation.Internal (
  ConstraintsGenContext (..),
  ConstraintsGenError (..),
  ConstraintsGenStack (..),
  InferenceRule (..),
  localMonoset,
  monosetInsertMultiple,
  runConstraintsGenStack,
 )
import Noll.SystemF.Constraint.Generation.TypeAnnotation (instantiateAnnotation)

import qualified Data.Map.Strict as Map
import qualified Lang.Common.Environment as Environment

type ConstraintsGen a = ConstraintsGenStack a TypeIndex Kind IndexedType

{-# INLINE lookupDataConstructor #-}
lookupDataConstructor :: Name -> ConstraintsGenStack c o k t (Maybe (Constructor o k t))
lookupDataConstructor name = asks (Environment.lookup name . constraintsGenContextDataConstructorEnv)

assertEqualityAssumptions :: a -> IndexedType -> [Assumption IndexedType] -> ConstraintsGen a ()
assertEqualityAssumptions _ t ms =
  tellRight $ do
    Assumption{..} <- ms
    -- TODO
    pure (Equality (InferenceRule 1) [assumptionType, t])

assertImplicitAssumptions :: a -> IndexedType -> [Assumption IndexedType] -> ConstraintsGen a ()
assertImplicitAssumptions loc t ms = do
  set <- asks constraintsGenContextMonomorphicSet
  tellRight $ do
    Assumption{..} <- ms
    -- TODO
    pure (Implicit (InferLetImplicit loc assumptionName assumptionType t) assumptionType t set)

withMonomorphic :: (TypeIndexed Kind t) => t -> ConstraintsGen a c -> ConstraintsGen a c
withMonomorphic = localMonoset . monosetInsertMultiple . typeIndexesIn

type Assertion a = IndexedType -> [Assumption IndexedType] -> ConstraintsGen a ()

patternConstraints :: (Data a) => Assertion a -> [Assumption IndexedType] -> Pattern a IndexedType -> ConstraintsGen a [Name]
patternConstraints assert ms =
  \case
    PAnnotation loc t p -> do
      r <- instantiateAnnotation loc t
      case r of
        Left err ->
          tellLeft [IllFormedTypeAnnotation err]
        Right t1 ->
          tellRight [Equality (InferAnnotation loc (typeOf p) t1) [typeOf p, t1]]
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
          tellRight [Explicit (InferenceRule 3) (foldTypeOf t ps) constructorScheme]
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
    PAny{} ->
      pure []
    PListCons _ t p1 p2 -> do
      ms1 <- patternConstraints assert ms p1
      ms2 <- patternConstraints assert ms p2
      tellRight [Explicit (InferenceRule 3) (foldTypeOf t [p1, p2]) listConstructorTypeScheme]
      pure (ms1 <> ms2)
    PListLiteral _ t ps -> do
      tellRight
        [ Equality (InferenceRule 3) (t : (typeOf <$> ps))
        , Explicit (InferenceRule 33) t (forall1 (\a -> TIntrinsic (IList a)))
        ]
      concatForM ps (patternConstraints assert ms)
    PAtVariable _ (Label _ name) -> do
      pure [name]
    PLiteral{} ->
      pure []

-- TODO: move
extractRow :: Type TypeIndex Kind -> Row TypeIndex Kind (Type TypeIndex Kind)
extractRow =
  \case
    TIntrinsic (IRecord (TRow r)) ->
      r
    _ ->
      error "TODO"

clauseAssumptions :: (Show a, Data a) => Clause a IndexedType -> ConstraintsGen a (IndexedType, [IndexedType], [Assumption IndexedType])
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
        CLambda{} ->
          error "TODO"
  names <- patternConstraints (assertEqualityAssumptions loc) ms p
  pure (typeOf p, ts1, filter (assumptionNameIsNotOneOf names) ms)

collectConstraints :: (Show a, Data a) => Expression a IndexedType -> ConstraintsGen a [Assumption IndexedType]
collectConstraints =
  \case
    EAnnotation loc t e -> do
      r <- instantiateAnnotation loc t
      case r of
        Left err ->
          tellLeft [IllFormedTypeAnnotation err]
        Right t1 ->
          tellRight [Equality (InferAnnotation loc (typeOf e) t1) [typeOf e, t1]]
      collectConstraints e
    EConstructor loc (Label t name) -> do
      r <- lookupDataConstructor name
      case r of
        Nothing ->
          tellLeft [NoDataConstructor loc name]
        Just Constructor{..} ->
          tellRight [Explicit (InferenceRule 4) t constructorScheme]
      pure []
    EVariable _ (Label t name) ->
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
    EListCons _ t e1 e2 -> do
      ms1 <- collectConstraints e1
      ms2 <- collectConstraints e2
      let t1 = typeOf e1 `TArrow` typeOf e2 `TArrow` t
      tellRight [Explicit (InferenceRule 402) t1 listConstructorTypeScheme]
      pure (ms1 <> ms2)
    EListLiteral _ t es -> do
      ms1 <- concatMapM collectConstraints es
      tellRight
        [ Equality (InferenceRule 555) (t : (TIntrinsic . IList . typeOf <$> es))
        , Explicit (InferenceRule 777) t (forall1 (\a -> TIntrinsic (IList a)))
        ]
      pure ms1
    EMatch loc t e cs -> do
      ms1 <- collectConstraints e
      (ts1, ts2, ms2) <- (third3 concat . unzip3 <$$> traverse clauseAssumptions) (fromList1 cs)
      -- Pattern types
      tellRight [Equality (InferMatchClausePatterns loc) (typeOf e : ts1)]
      -- Expression types
      tellRight [Equality (InferMatchClauseExpressions loc) (t : concat ts2)]
      pure (ms1 <> ms2)
    ECompiledMatch{} ->
      error "TODO"
    EUnaryOperator{} ->
      error "TODO"
    EBinaryOperator loc t op -> do
      tellRight [Explicit (InferBinaryOperator loc) t (binaryOperatorTypeScheme op)]
      pure []
    ESelect _ (Label t name) e -> do
      rvar <- supplied (RVariable . TypeIndex KRow)
      let t1 = TIntrinsic (IRecord (TRow (RExtend name t rvar)))
      tellRight [Equality (InferenceRule 302) [t1, typeOf e]]
      collectConstraints e
    EFold _ t (e :| es) cs e1 -> do
      ms1 <- collectConstraints e
      ms2 <- concatMapM collectConstraints es
      (ts1, ts2, ms3) <- (third3 concat . unzip3 <$$> traverse clauseAssumptions) (fromList1 cs)
      -- Pattern types
      tellRight [Equality (InferenceRule 401) (typeOf e : ts1)]
      -- Expression types
      tellRight [Equality (InferenceRule 99402) (foldTypeOf t es : concat ts2)]
      ms4 <- concatMapM collectConstraints e1
      case e1 of
        Just (ERecursiveLet _ (PVariable _ (Label t1 _)) _ _) ->
          tellRight [Equality (InferenceRule 999) [foldTypeOf t (e :| es), t1]]
        _ ->
          pure ()

      pure (ms1 <> ms2 <> ms3 <> ms4)
    ERecord _ t d e -> do
      ms1 <- concatMapM collectConstraints e
      ms2 <- concatMapM collectConstraints d
      let d1 = pure . typeOf <$> d
          e1 = extractRow . typeOf <$> e
          t1 = TIntrinsic (IRecord (TRow (fromDictionary d1 (fromMaybe RNil e1))))
      tellRight [Equality (InferenceRule 301) [t, t1]]
      pure (ms1 <> ms2)

--    EDictionaryLambda{} ->
--      error "TODO"
--    EDictionaryApplication{} ->
--      error "TODO"

listConstructorTypeScheme :: Scheme TypeIndex Kind IndexedType
listConstructorTypeScheme = forall1 (\a -> a ~> TIntrinsic (IList a) ~> TIntrinsic (IList a))
