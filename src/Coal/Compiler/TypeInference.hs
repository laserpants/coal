{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.TypeInference (typeDefinitionsC) where

import Coal.AST.Type.Parameterized
import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Common.Label (Label (..))
import Coal.Common.Supply (supplied)
import Coal.Compiler.Build
import Coal.Compiler.Journal
import Coal.Compiler.Stack
import Coal.Language
import Coal.Language.Module
import Coal.TypeSystem
import Control.Monad.Except
import Control.Monad.Extra (concatForM)
import Control.Monad.Reader (asks)
import Control.Monad.State (evalState, gets)
import Control.Monad.Writer (execWriter)
import Data.Data (Data)
import Data.Either.Extra (partitionEithers)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Tuple.Extra (fst3)
import Extras (Dictionary, Name)

type ConstraintsGenResult g o a t s = (s, Dictionary (g, o a), [ConstraintsGenOutput g o a t])

runConstraintsGenC :: (Monad m) => ConstraintsGenStack a TypeIndex Kind IndexedType r -> CompilerT a m (ConstraintsGenResult a TypeIndex Kind IndexedType r)
runConstraintsGenC stack = do
  sup <- gets compilerSupply
  build <- getCurrentBuildC
  let (result, ConstraintsGenState{..}, output) = runConstraintsGenStack sup (context build) stack
  updateSupplyC constraintsGenStateSupply
  pure (result, constraintsGenStateTypeIndexes, output)
 where
  context build =
    ConstraintsGenContext
      { constraintsGenContextMonomorphicSet = mempty
      , constraintsGenContextModules = build
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
    DFunction _ _ (f@(FunctionDef loc (Just (With _ t)) _ _ _) :| _) _ -> do
      t1 <- compileFunctionC f
      r <- runConstraintsGenC (instantiateAnnotation loc t)
      case fst3 r of
        Left err ->
          compilerReportConstraintsGenErrors [EIllFormedTypeAnnotation err]
        Right t2 ->
          insertConstraintsC [Equality (RuleAnnotation loc t1 t2) [t1, t2]]
    DConstant _ _ c@(ConstantDef loc (Just (With _ t)) _ _) _ -> do
      t1 <- compileConstantC c
      r <- runConstraintsGenC (instantiateAnnotation loc t)
      case fst3 r of
        Left err ->
          compilerReportConstraintsGenErrors [EIllFormedTypeAnnotation err]
        Right t2 ->
          insertConstraintsC [Equality (RuleAnnotation loc t1 t2) [t1, t2]]
    DFunction _ _ (f :| _) _ ->
      void (compileFunctionC f)
    DConstant _ _ c _ ->
      void (compileConstantC c)
    DFold loc _ (FoldDef (With _ t) _ (Just e)) -> do
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
          let t3 = foldTypeOf t2 ps
              fields = Map.toList d
          insertConstraintsC [Equality (RuleAnnotation loc t1 t3) [t1, t3]]

          cs <- concatForM fields $
            \(name, elem1) -> do
              env <- asks compilerCodataAccessorEnvironment
              case Environment.lookup (Text.replace "@" "" name) env of
                Just (CodataAccessorInfo _ _ CodataAccessor{..}) -> do
                  if "@" `Text.isPrefixOf` name
                    then do
                      let tl = typeOf (NonEmpty.head ps)
                          tr = typeOf elem1
                      pure [Equality (RuleUnfoldEquality loc tl tr) [tl, tr]]
                    else do
                      let tl = t2 `TArrow` typeOf elem1
                      pure [Explicit (RuleUnfoldExplicit loc tl codataAccessorScheme) tl codataAccessorScheme]
                Nothing ->
                  pure []
          if length cs == length fields
            then insertConstraintsC cs
            else compilerReportConstraintsGenErrors [ECodataFieldMismatch loc]
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
    [Explicit (InferenceRulePlaceholder "typeDefinitionsC") (apply sub t) s | n1 == n2]
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
        \(n, Forall _ _ s) -> do
          env <- asks compilerTypeConstructorEnvironment
          let s1 = evalState (instantiateVars [(q, TypeIndex k 0)] env s) (1 :: Int)
          insertNameC n (Forall (typeIndexesIn s1) [Trait name (TVariable (TypeIndex k 0))] s1)
    DInstance _ trait (InstanceDef _ t0 ds) -> do
      env <- asks compilerTraitEnvironment
      kinds <- asks compilerTypeConstructorEnvironment
      case Environment.lookup trait env of
        Nothing ->
          error ("Missing trait: " <> Text.unpack trait)
        Just (TraitInfo _ _ p@(Parameter k _) traitInfoEntries) ->
          forM_ ds $
            \d -> do
              case Environment.lookup (definitionName d) traitInfoEntries of
                Nothing ->
                  error ("Missing implementation: " <> Text.unpack (definitionName d))
                Just s0 -> do
                  t1 <- instantiateVarsC t0
                  let s1 = instantiateTemplateC (TypeIndex k 0) t1 (toIndexedScheme kinds p s0)
                  insertConstraintsC [Explicit (InferenceRulePlaceholder "typeDefinitionC") (typeOf d) s1]
                  compileDefinitionC d
    d@(DFunction loc name (FunctionDef _ _ (With _ t) ps _ :| _) _) -> do
      checkIfNameExists loc name
      checkMain loc t ps name
      compileDefinitionC d
      sub <- solveC
      defineC name (typeOf (apply sub d))
    d@(DConstant loc name _ _) -> do
      checkIfNameExists loc name
      compileDefinitionC d
      sub <- solveC
      defineC name (typeOf (apply sub d))
    d -> do
      compileDefinitionC d
      sub <- solveC
      defineC (definitionName d) (typeOf (apply sub d))

checkMain :: (Monad m, Data a) => a -> IndexedType -> NonEmpty (Pattern a IndexedType) -> Name -> CompilerT a m ()
checkMain loc t ps name = do
  path <- gets compilerCurrentModule
  when (Path ["Main"] == path && "main" == name) $
    insertConstraintsC
      [ Explicit
          (RuleEntrypoint loc t1)
          t1
          (Forall mempty [] (TIntrinsic IUnit `TArrow` TApplication KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| [])))
      ]
 where
  t1 = foldTypeOf t ps

checkIfNameExists :: (Monad m) => a -> Name -> CompilerT a m ()
checkIfNameExists loc name = do
  env <- gets compilerNameStore
  when (Environment.contains name env) $ do
    path <- gets compilerCurrentModule
    tellErrors [NameAlreadyDefined name (ErrorLocation (principalPath path) loc)]
    throwError PreflightFailure

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
