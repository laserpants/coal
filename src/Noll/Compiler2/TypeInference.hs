{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler2.TypeInference (typeDefinitionsC) where

import Control.Monad.Reader (ReaderT, ask, asks, lift, runReaderT)
import Control.Monad.State (MonadState, gets)
import Control.Monad.Writer (WriterT, execWriter, execWriterT, tell)
import Data.Data (Data)
import Data.Either.Extra (partitionEithers)
import Lang.Common.Environment (Environment (..))
import Lang.Common.List1 (List1, NonEmpty (..), fromList1)
import Lang.Common.Supply (Supply (..), supplied)
import Lang.Label (Label (..))
import Lang.Utils (Dictionary, Name, forM_, traverse_)
import Noll.Compiler2.Internal
import Noll.Language
import Noll.Module (Constant (..), Definition (..), Function (..))
import Noll.Module.Definition (definitionName)
import Noll.SystemF

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Lang.Common.Environment as Environment

type ConstraintsGenResult c o k t r = (r, Dictionary (c, o k), [ConstraintsGenOutput c o k t])

runConstraintsGenC :: (Monad m) => ConstraintsGenStack c TypeIndex Kind IndexedType r -> Compiler2T a m (ConstraintsGenResult c TypeIndex Kind IndexedType r)
runConstraintsGenC stack = do
  env <- ask
  sup <- gets compiler2Supply
  let (result, ConstraintsGenState{..}, output) = runConstraintsGenStack sup (context env) stack
  updateSupplyC constraintsGenStateSupply
  pure (result, constraintsGenStateTypeIndexes, output)
 where
  context Compiler2Environment{..} =
    ConstraintsGenContext
      { constraintsGenContextMonomorphicSet = mempty
      , constraintsGenContextDataConstructorEnv = compiler2DataConstructorEnv
      , constraintsGenContextTypeConstructorEnv = compiler2TypeConstructorEnv
      }

generateConstraintsC :: (Monad m, Data a, Show a) => Expression a IndexedType -> Compiler2T a m ([CompilerAssumption], [CompilerConstraint a])
generateConstraintsC e = do
  (assumptions, params, result) <- runConstraintsGenC (collectConstraints e)
  let (errors, constraints) = partitionEithers result
  compiler2ReportConstraintsGenErrors errors
  compiler2SetTypeAnnotationParams params
  pure (assumptions, constraints)

assumptionConstraints :: (Monad m) => CompilerAssumption -> Compiler2T a m (Either CompilerAssumption (CompilerConstraint a))
assumptionConstraints Assumption{..} = do
  names <- gets compiler2NameStore
  case Environment.lookup assumptionName names of
    Nothing ->
      pure $ Left Assumption{..}
    Just s ->
      pure $ Right (Explicit InferenceRulePlaceholder assumptionType s)

solveConstraintsC :: (Monad m, Data a, Eq a, Show a) => [CompilerConstraint a] -> Compiler2T a m Substitution
solveConstraintsC cs = do
  dict <- gets compiler2TypeAnnotationParams
  n <- gets compiler2Supply
  let (sub, m, rs) = solveConstraints n cs
  updateSupplyC m
  let errors = execWriter (checkTypeAnnotationParameters (Map.toList dict) sub)
  compiler2ReportSolverRuleViolations (apply sub rs)
  compiler2ReportConstraintsGenErrors (EIllFormedTypeAnnotation <$> errors)
  pure sub

compileConstraintsC :: (Monad m, Data a, Show a) => Expression a IndexedType -> Compiler2T a m ()
compileConstraintsC expr = do
  (ms1, cs1) <- generateConstraintsC expr
  (ms2, cs2) <- partitionEithers <$> traverse assumptionConstraints ms1
  sub <- gets compiler2Substitution
  insertAssumptionsC (apply sub ms2)
  insertConstraintsC (cs1 <> cs2)

compileFunctionC :: (Monad m, Data a, Show a) => Function Expression a IndexedType -> Compiler2T a m ()
compileFunctionC (Function loc (With _ t) ps e) = do
  insertConstraintsC [Equality (RuleTopLevelFunction loc) [t, typeOf e]]
  t1 <- supplied (TVariable . TypeIndex KType)
  compileConstraintsC $
    ELet
      loc
      (BFunction loc placeholder ps e :| [])
      (EVariable loc (Label (foldTypeOf t1 ps) placeholder))
 where
  placeholder = "###.function"

compileConstantC :: (Monad m, Data a, Show a) => Constant Expression a IndexedType -> Compiler2T a m ()
compileConstantC (Constant loc (With _ t) e) = do
  insertConstraintsC [Equality (RuleTopLevelConstant loc) [t, typeOf e]]
  compileConstraintsC $
    ELet
      loc
      (BPattern loc (PVariable loc (Label t placeholder)) e :| [])
      (EVariable loc (Label t placeholder))
 where
  placeholder = "###.constant"

compileDefinitionC :: (Monad m, Data a, Show a) => Definition a k IndexedType -> Compiler2T a m ()
compileDefinitionC =
  \case
    DFunction _ f ->
      compileFunctionC f
    DConstant _ c ->
      compileConstantC c
    DAnnotation _ d ->
      compileDefinitionC d
    _ ->
      error "TODO"

solveC :: (Monad m, Data a, Eq a, Show a) => Compiler2T a m Substitution
solveC = do
  constraints <- gets compiler2Constraints
  sub1 <- gets compiler2Substitution
  sub2 <- solveConstraintsC constraints
  clearConstraintsC
  -- TODO: Clear typeAnnotationParameters?
  updateSubstitutionC (sub2 <> sub1)
  gets compiler2Substitution

typeDefinitionsC :: (Monad m, Data a, Show a, Eq a) => [Definition a Kind IndexedType] -> Compiler2T a m ([Definition a Kind IndexedType], [CompilerAssumption])
typeDefinitionsC ds = do
  forM_ ds typeDefinitionC
  sub <- gets compiler2Substitution
  ams <- gets compiler2Assumptions
  Environment env <- gets compiler2NameStore
  insertConstraintsC $ do
    (n1, s) <- Map.toList env
    Assumption n2 t <- ams
    [Explicit InferenceRulePlaceholder (apply sub t) s | n1 == n2]
  sub1 <- solveC
  pure (fmap (fmap normalizeRowTypes) (apply sub1 ds), apply sub1 ams)

typeDefinitionC :: (Monad m, Data a, Show a, Eq a) => Definition a Kind IndexedType -> Compiler2T a m ()
typeDefinitionC =
  \case
    DImport{} ->
      pure ()
    DTypeAlias{} ->
      pure ()
    DType{} ->
      pure ()
    DCodata{} ->
      pure ()
    DSignature{} ->
      pure ()
    DTrait _ _ _ ds ->
      forM_ ds $
        \(n, s) -> defineC n =<< instantiateVars s
    DInstance trait t1 ds -> do
      env <- asks compiler2TraitEnvironment
      case Environment.lookup trait env of
        Nothing ->
          error ("Missing trait: " <> Text.unpack trait)
        Just (tx, defs) -> do
          forM_ ds $
            \d -> do
              case Environment.lookup (definitionName d) defs of
                Nothing ->
                  error ("Missing implementation: " <> Text.unpack (definitionName d))
                Just s -> do
                  ti <- instantiateVars t1
                  insertConstraintsC [Explicit InferenceRulePlaceholder (typeOf d) (instantiateTemplate tx ti s)]
                  compileDefinitionC d
    d -> do
      compileDefinitionC d
      sub <- solveC
      defineC (definitionName d) (typeOf (apply sub d))

instantiateTemplate :: TypeIndex Kind -> IndexedType -> Scheme TypeIndex Kind IndexedType -> Scheme TypeIndex Kind IndexedType
instantiateTemplate (TypeIndex _ n) t1 (Forall vs ts t) = Forall vs ts (apply (n `mapsTo` t1) t)

instantiateVars :: (Monad m) => Type Parameter () -> Compiler2T a m IndexedType
instantiateVars t = do
  ts <- execWriterT (params t)
  runReaderT (instantiateTypeVars t) (Environment.fromList ts)

instantiateTypeVars :: (Monad m) => Type Parameter () -> ReaderT (Environment (TypeIndex Kind)) (Compiler2T a m) IndexedType
instantiateTypeVars =
  \case
    TVariable (Parameter _ n) -> do
      env <- ask
      case Environment.lookup n env of
        Just v ->
          pure (TVariable v)
        Nothing ->
          error "Implementation error"
    TApplication _ t ts -> do
      u <- instantiateTypeVars t
      us <- traverse instantiateTypeVars ts
      case applyKind (kindOf <$> fromList1 us) (kindOf u) of
        Nothing ->
          error "Kind mismatch"
        Just k ->
          pure (TApplication k u us)
    TArrow t1 t2 ->
      TArrow <$> instantiateTypeVars t1 <*> instantiateTypeVars t2
    TIntrinsic t ->
      TIntrinsic <$> traverse instantiateTypeVars t
    TRow r ->
      TRow <$> instantiateRowVars r
    TAlias name ts t ->
      TAlias name <$> traverse instantiateTypeVars ts <*> instantiateTypeVars t
    TConstructor _ name -> do
      env <- lift (asks compiler2TypeConstructorEnv)
      case Environment.lookup name env of
        Just k ->
          pure (TConstructor k name)
        Nothing ->
          error ("No type constructor '" <> Text.unpack name <> "'")

instantiateRowVars :: (Monad m) => Row Parameter () (Type Parameter ()) -> ReaderT (Environment (TypeIndex Kind)) (Compiler2T a m) (Row TypeIndex Kind IndexedType)
instantiateRowVars =
  \case
    RVariable (Parameter _ n) -> do
      env <- ask
      case Environment.lookup n env of
        Just v ->
          pure (RVariable v)
        Nothing ->
          error "Implementation error"
    RExtend name t r ->
      RExtend name <$> instantiateTypeVars t <*> instantiateRowVars r
    RNil ->
      pure RNil

defineC :: (Monad m) => Name -> IndexedType -> Compiler2T a m ()
defineC name t = insertNameC name (Forall (typeIndexesIn s) [] s)
 where
  s = normalizeTypeIndexes t

class Params p where
  params :: (MonadState s m, Supply s) => p -> WriterT [(Name, TypeIndex Kind)] m ()

instance (Params a) => Params [a] where
  params = traverse_ params

instance (Params a) => Params (List1 a) where
  params = traverse_ params

instance Params (Type Parameter ()) where
  params =
    \case
      TVariable p ->
        params p
      TApplication _ t ts -> do
        params t
        params ts
      TArrow t1 t2 -> do
        params t1
        params t2
      TIntrinsic t ->
        params t
      TRow r ->
        params r
      TAlias _ _ t ->
        params t
      TConstructor{} ->
        pure ()

instance Params (Intrinsic (Type Parameter ())) where
  params =
    \case
      IList t ->
        params t
      IOption t ->
        params t
      IRecord t ->
        params t
      IResult t ->
        params t
      ITuple ts ->
        params ts
      _ ->
        pure ()

instance Params (Row Parameter () (Type Parameter ())) where
  params =
    \case
      RVariable p ->
        params p
      RExtend _ t r -> do
        params t
        params r
      RNil ->
        pure ()

instance Params (Parameter ()) where
  params p = do
    ti <- supplied (TypeIndex KType)
    tell [(parameterName p, ti)]
