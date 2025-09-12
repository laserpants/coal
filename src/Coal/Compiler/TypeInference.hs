{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

-- FIXME
module Coal.Compiler.TypeInference (typeDefinitionsC) where

import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Common.Label (Label (..))
import Coal.Common.Supply (supplied)
import Coal.Compiler.Stack
import Coal.Compiler.Transform.Type.Parameterized
import Coal.Language
import Coal.Language.Module
import Coal.TypeSystem
import Control.Monad.Reader (ask, asks)
import Control.Monad.State (evalState, gets)
import Control.Monad.Writer (execWriter)
import Data.Data (Data)
import Data.Either.Extra (partitionEithers)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Extra (Dictionary, Name, forM, forM_, void, (<$$>))

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
      , constraintsGenContextTopLevelFoldEnv = compilerFoldEnvironment
      }

generateConstraintsC :: (Monad m, Data a, Show a) => Expression a IndexedType -> CompilerT a m ([CompilerAssumption a], [CompilerConstraint a])
generateConstraintsC e = do
  (assumptions, params, result) <- runConstraintsGenC (emitConstraints e)
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

compileFunctionC :: (Monad m, Data a, Show a) => FunctionDef a IndexedType -> CompilerT a m IndexedType
compileFunctionC (FunctionDef loc _ (With _ t) ps e) = do
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

compileConstantC :: (Monad m, Data a, Show a) => ConstantDef a IndexedType -> CompilerT a m IndexedType
compileConstantC (ConstantDef loc _ (With _ t) e) = do
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
    DFunction _ _ f@(FunctionDef loc (Just (With _ t)) _ _ _) _ -> do
      t1 <- compileFunctionC f
      (r, _, _) <- runConstraintsGenC (instantiateAnnotation loc t)
      case r of
        Left err ->
          compilerReportConstraintsGenErrors [EIllFormedTypeAnnotation err]
        Right t2 ->
          insertConstraintsC [Equality (RuleAnnotation loc t1 t2) [t1, t2]]
    DConstant _ _ c@(ConstantDef loc (Just (With _ t)) _ _) _ -> do
      t1 <- compileConstantC c
      (r, _, _) <- runConstraintsGenC (instantiateAnnotation loc t)
      case r of
        Left err ->
          compilerReportConstraintsGenErrors [EIllFormedTypeAnnotation err]
        Right t2 ->
          insertConstraintsC [Equality (RuleAnnotation loc t1 t2) [t1, t2]]
    DFunction _ _ f _ ->
      void (compileFunctionC f)
    DConstant _ _ c _ ->
      void (compileConstantC c)
    --    DAnnotation _ (With _ t) (DFunction _ _ f@(Function loc _ _ _) _) -> do
    --      t1 <- compileFunctionC f
    --      (r, _, _) <- runConstraintsGenC (instantiateAnnotation loc t)
    --      case r of
    --        Left err ->
    --          compilerReportConstraintsGenErrors [EIllFormedTypeAnnotation err]
    --        Right t2 ->
    --          insertConstraintsC [Equality (RuleAnnotation loc t1 t2) [t1, t2]]
    --    DAnnotation _ (With _ t) (DConstant _ _ c@(Constant loc _ _) _) -> do
    --      t1 <- compileConstantC c
    --      (r, _, _) <- runConstraintsGenC (instantiateAnnotation loc t)
    --      case r of
    --        Left err ->
    --          compilerReportConstraintsGenErrors [EIllFormedTypeAnnotation err]
    --        Right t2 ->
    --          insertConstraintsC [Equality (RuleAnnotation loc t1 t2) [t1, t2]]
    DFold loc name (FoldDef (With _ t) cs (Just e)) -> do
      compileConstraintsC e
      let t1 = typeOf e
      (r, _, _) <- runConstraintsGenC (instantiateAnnotation loc t)
      case r of
        Left err ->
          compilerReportConstraintsGenErrors [EIllFormedTypeAnnotation err]
        Right t2 -> do
          insertConstraintsC [Equality (RuleAnnotation loc t1 t2) [t1, t2]]
    --      t1 <- supplied (TVariable . TypeIndex KType)
    --      t2 <- supplied (TVariable . TypeIndex KType)
    --      compileConstraintsC $
    --        ELambda
    --          loc
    --          (PVariable undefined (Label t1 "#.a") :| [])
    --          (
    --            EMatch
    --              loc
    --              t2
    --              (EVariable undefined (Label t1 "#.a"))
    --              cs
    --          )
    DUnfold loc _ (UnfoldDef (With _ t) ps d (Just e)) -> do
      compileConstraintsC e
      let t1 = typeOf e
      (r, _, _) <- runConstraintsGenC (instantiateAnnotation loc t)
      case r of
        Left err ->
          compilerReportConstraintsGenErrors [EIllFormedTypeAnnotation err]
        Right t2 -> do
          let t3 = foldType t2 (typeOf <$> ps)
              fields = Map.toList d
          insertConstraintsC [Equality (RuleAnnotation loc t1 t3) [t1, t3]]

          cs <- concat <$$> forM fields $
            \(name, elem1) -> do
              env <- asks compilerCodataAccessorEnvironment
              case Environment.lookup (Text.replace "@" "" name) env of
                Just CodataAccessor{..} -> do
                  if "@" `Text.isPrefixOf` name
                    then pure [Equality InferenceRulePlaceholder [typeOf (NonEmpty.head ps), typeOf elem1]]
                    else pure [Explicit InferenceRulePlaceholder (t2 `TArrow` typeOf elem1) codataAccessorScheme]
                Nothing ->
                  pure []
          if length cs == length fields
            then insertConstraintsC cs
            else compilerReportConstraintsGenErrors [ECodataFieldMismatch loc]

    -- case (q, Map.lookup ("$_" <> name) fields) of
    --  (Just CodataAccessor{..}, Just e4) -> do
    --    t3 <- supplied (TVariable . TypeIndex KType)
    --    tellRight [Explicit InferenceRulePlaceholder (t0 `TArrow` typeOf elem) codataAccessorScheme]
    --    tellRight [Equality InferenceRulePlaceholder [typeOf e4, t3 `TArrow` typeOf elem]]
    --  _ ->

    -- DAnnotation _ (With _ t) (DFold loc name cs (Just e)) -> do
    --  compileConstraintsC e
    --  let t1 = typeOf e
    --  (r, _, _) <- runConstraintsGenC (instantiateAnnotation loc t)
    --  case r of
    --    Left err ->
    --      compilerReportConstraintsGenErrors [EIllFormedTypeAnnotation err]
    --    Right t2 -> do
    --      insertConstraintsC [Equality (RuleAnnotation loc t1 t2) [t1, t2]]
    --          t1 <- supplied (TVariable . TypeIndex KType)
    --          t3 <- supplied (TVariable . TypeIndex KType)
    --          insertConstraintsC [Equality InferenceRulePlaceholder [t2, t1 `TArrow` t3]]
    --          compileConstraintsC $
    --            ELambda
    --              loc
    --              (PVariable loc (Label t1 "#.a") :| [])
    --              ( EMatch
    --                  loc
    --                  t3
    --                  (EVariable loc (Label t1 "#.a"))
    --                  cs
    --              )
    _ ->
      error "Not implemented"

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
    DCotype{} ->
      pure ()
    DTrait _ name (TraitDef _ (Parameter k q) ds) ->
      forM_ ds $
        \(n, s) -> do
          env <- asks compilerTypeConstructorEnvironment
          let s1 = evalState (instantiateVars [(q, TypeIndex k 0)] env s) (1 :: Int)
          insertNameC n (Forall (typeIndexesIn s1) [Trait name (TVariable (TypeIndex k 0))] s1)
    DInstance _ trait (InstanceDef _ t1 ds) -> do
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
