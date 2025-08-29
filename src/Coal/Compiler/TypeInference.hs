{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.TypeInference (typeDefinitionsC) where

import Coal.Common.Environment (Environment (..))
import Coal.Common.Label (Label (..))
import Coal.Common.List1 (NonEmpty (..))
import Coal.Common.Supply (supplied)
import Coal.Compiler.Stack
import Coal.Compiler.Transform.Type.Parameterized
import Coal.Language
import Coal.Language.Module (Constant (..), Definition (..), Function (..))
import Coal.Language.Module.Definition (definitionName)
import Coal.TypeSystem
import Control.Monad.Reader (ask, asks)
import Control.Monad.State (evalState, gets)
import Control.Monad.Writer (execWriter)
import Data.Data (Data)
import Data.Either.Extra (partitionEithers)
import Extra (Dictionary, Name, forM_, void)

import qualified Coal.Common.Environment as Environment
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text

type ConstraintsGenResult c o a t s = (s, Dictionary (c, o a), [ConstraintsGenOutput c o a t])

runConstraintsGenC :: (Monad m) => ConstraintsGenStack c TypeIndex Kind IndexedType r -> CompilerT a m (ConstraintsGenResult c TypeIndex Kind IndexedType r)
runConstraintsGenC stack = do
  env <- ask
  sup <- gets compilerSupply
  let (result, ConstraintsGenState{..}, output) = runConstraintsGenStack sup (context env) stack
  updateSupplyC constraintsGenStateSupply
  pure (result, constraintsGenStateTypeIndexes, output)
 where
  context CompilerEnvironment{..} =
    ConstraintsGenContext
      { constraintsGenContextMonomorphicSet = mempty
      , constraintsGenContextDataConstructorEnv = compilerDataConstructorEnvironment
      , constraintsGenContextCodataAccessorEnv = compilerCodataAccessorEnvironment
      , constraintsGenContextTypeConstructorEnv = compilerTypeConstructorEnvironment
      }

generateConstraintsC :: (Monad m, Data a, Show a) => Expression a IndexedType -> CompilerT a m ([CompilerAssumption a], [CompilerConstraint a])
generateConstraintsC e = do
  (assumptions, params, result) <- runConstraintsGenC (collectConstraints e)
  let (errors, constraints) = partitionEithers result
  compilerReportConstraintsGenErrors errors
  compilerSetTypeAnnotationParams params
  pure (assumptions, constraints)

assumptionConstraints :: (Monad m) => CompilerAssumption a -> CompilerT a m (Either (CompilerAssumption a) (CompilerConstraint a))
assumptionConstraints Assumption{..} = do
  names <- gets compilerNameStore
  pure $
    case Environment.lookup assumptionName names of
      Nothing ->
        Left Assumption{..}
      Just s ->
        Right (Explicit (RuleTypeConstraint assumptionMetadata assumptionName assumptionType s) assumptionType s)

solveConstraintsC :: (Monad m, Data a, Eq a) => [CompilerConstraint a] -> CompilerT a m Substitution
solveConstraintsC cs = do
  dict <- gets compilerTypeAnnotationParams
  n <- gets compilerSupply
  let (sub, m, rs) = solveConstraints n cs
  updateSupplyC m
  let errors = execWriter (checkTypeAnnotationParameters (Map.toList dict) sub)
  compilerReportSolverRuleViolations (apply sub rs)
  compilerReportConstraintsGenErrors (EIllFormedTypeAnnotation <$> errors)
  pure sub

compileConstraintsC :: (Monad m, Data a, Show a) => Expression a IndexedType -> CompilerT a m ()
compileConstraintsC expr = do
  (ms1, cs1) <- generateConstraintsC expr
  (ms2, cs2) <- partitionEithers <$> traverse assumptionConstraints ms1
  sub <- gets compilerSubstitution
  insertAssumptionsC (apply sub ms2)
  insertConstraintsC (cs1 <> cs2)

compileFunctionC :: (Monad m, Data a, Show a) => Function Expression a IndexedType -> CompilerT a m IndexedType
compileFunctionC (Function loc (With _ t) ps e) = do
  insertConstraintsC [Equality (RuleTopLevelFunction loc) [t, typeOf e]]
  t1 <- supplied (TVariable . TypeIndex KType)
  compileConstraintsC $
    ELet
      loc
      (BFunction loc placeholder ps e :| [])
      (EVariable loc (Label t1 placeholder))
  pure t
 where
  placeholder = "###.function"

compileConstantC :: (Monad m, Data a, Show a) => Constant Expression a IndexedType -> CompilerT a m IndexedType
compileConstantC (Constant loc (With _ t) e) = do
  insertConstraintsC [Equality (RuleTopLevelConstant loc) [t, typeOf e]]
  compileConstraintsC $
    ELet
      loc
      (BPattern loc (PVariable loc (Label t placeholder)) e :| [])
      (EVariable loc (Label t placeholder))
  pure t
 where
  placeholder = "###.constant"

compileDefinitionC :: (Monad m, Data a, Show a) => Definition a k IndexedType -> CompilerT a m ()
compileDefinitionC =
  \case
    DFunction _ f _ ->
      void (compileFunctionC f)
    DConstant _ c _ ->
      void (compileConstantC c)
    DAnnotation (With _ t) (DFunction _ f@(Function loc _ _ _) _) -> do
      t1 <- compileFunctionC f
      (r, _, _) <- runConstraintsGenC (instantiateAnnotation loc t)
      case r of
        Left err ->
          compilerReportConstraintsGenErrors [EIllFormedTypeAnnotation err]
        Right t2 ->
          insertConstraintsC [Equality (RuleAnnotation loc t1 t2) [t1, t2]]
    DAnnotation (With _ t) (DConstant _ c@(Constant loc _ _) _) -> do
      t1 <- compileConstantC c
      (r, _, _) <- runConstraintsGenC (instantiateAnnotation loc t)
      case r of
        Left err ->
          compilerReportConstraintsGenErrors [EIllFormedTypeAnnotation err]
        Right t2 ->
          insertConstraintsC [Equality (RuleAnnotation loc t1 t2) [t1, t2]]
    _ ->
      error "TODO"

solveC :: (Monad m, Data a, Eq a) => CompilerT a m Substitution
solveC = do
  constraints <- gets compilerConstraints
  sub1 <- gets compilerSubstitution
  sub2 <- solveConstraintsC constraints
  clearConstraintsC
  clearTypeAnnotationParamsC
  updateSubstitutionC (sub2 <> sub1)
  gets compilerSubstitution

typeDefinitionsC :: (Monad m, Data a, Show a, Eq a) => [Definition a Kind IndexedType] -> CompilerT a m ([Definition a Kind IndexedType], [CompilerAssumption a])
typeDefinitionsC ds = do
  forM_ ds typeDefinitionC
  sub <- gets compilerSubstitution
  ams <- gets compilerAssumptions
  Environment env <- gets compilerNameStore
  insertConstraintsC $ do
    (n1, s) <- Map.toList env
    Assumption _ n2 t <- ams
    [Explicit InferenceRulePlaceholder (apply sub t) s | n1 == n2]
  sub1 <- solveC
  pure (fmap (fmap normalizeRowTypes) (apply sub1 ds), apply sub1 ams)

typeDefinitionC :: (Monad m, Data a, Show a, Eq a) => Definition a Kind IndexedType -> CompilerT a m ()
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
    DTrait name _ (Parameter k q) ds ->
      forM_ ds $
        \(n, s) -> do
          env <- asks compilerTypeConstructorEnvironment
          let s1 = evalState (instantiateVars [(q, TypeIndex k 0)] env s) (1 :: Int)
          insertNameC n (Forall (typeIndexesIn s1) [Trait name (TVariable (TypeIndex k 0))] s1)
    DInstance trait _ t1 ds -> do
      env <- asks compilerTraitEnvironment
      case Environment.lookup trait env of
        Nothing ->
          error ("Missing trait: " <> Text.unpack trait)
        Just (_, tx, defs) ->
          forM_ ds $
            \d -> do
              case Environment.lookup (definitionName d) defs of
                Nothing ->
                  error ("Missing implementation: " <> Text.unpack (definitionName d))
                Just s -> do
                  ti <- instantiateVarsC t1
                  insertConstraintsC [Explicit InferenceRulePlaceholder (typeOf d) (instantiateTemplateC tx ti s)]
                  compileDefinitionC d
    d -> do
      compileDefinitionC d
      sub <- solveC
      defineC (definitionName d) (typeOf (apply sub d))

instantiateTemplateC :: TypeIndex Kind -> IndexedType -> IndexedScheme -> IndexedScheme
instantiateTemplateC (TypeIndex _ n) t1 (Forall vs ts t) = Forall vs ts (apply (n `mapsTo` t1) t)

instantiateVarsC :: (Monad m) => Type Parameter () -> CompilerT a m IndexedType
instantiateVarsC t = do
  env <- asks compilerTypeConstructorEnvironment
  instantiateVars [] env t

defineC :: (Monad m) => Name -> IndexedType -> CompilerT a m ()
defineC name t = insertNameC name (Forall (typeIndexesIn s) [] s)
 where
  s = normalizeTypeIndexes t
