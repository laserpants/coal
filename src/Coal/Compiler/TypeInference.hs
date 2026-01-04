{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Coal.Compiler.TypeInference (typeDefinitionsC, toIndexedType, toIndexedScheme) where

import Coal.AST.Type.Parameterized
import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Common.Label (Label (..))
import Coal.Common.Supply (supplied)
import Coal.Compiler.Build
import Coal.Compiler.Journal (tellErrors)
import Coal.Compiler.Stack
import Coal.Language
import Coal.Language.Module
import Coal.TypeSystem
import Coal.TypeSystem.Kind.Inference
import Control.Monad.Except (MonadError (..), forM_, void, when)
import Control.Monad.Extra (concatForM)
import Control.Monad.Reader (asks)
import Control.Monad.State (evalState, gets)
import Control.Monad.Writer (execWriter)
import Data.Data (Data)
import Data.Either.Extra (partitionEithers)
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import Data.Tuple.Extra (fst3)
import Extras (Dictionary, Name)

class GenerateConstraints a o where
  generateConstraints :: (Monad m, Data a, Show a) => o -> CompilerT a m ()

instance GenerateConstraints a (Expression a IndexedType) where
  generateConstraints expr = do
    (ms1, cs1) <- generateExpressionConstraints expr
    (ms2, cs2) <- partitionEithers <$> traverse assumptionConstraints ms1
    sub <- gets compilerSubstitution
    insertAssumptionsC (apply sub ms2)
    insertConstraintsC (cs1 <> cs2)

instance GenerateConstraints a (FunctionDefinition a IndexedType) where
  generateConstraints (FunctionDefinition loc _ (With _ t) ps e) = do
    insertConstraintsC [Equality (RuleTopLevelFunction loc) [t, typeOf e]]
    t1 <- supplied (TVariable . TypeIndex KType)
    generateConstraints $
      ELet
        loc
        (BFunction loc placeholder ps e :| [])
        (EVariable loc (Label t1 placeholder))
   where
    placeholder = "###.function"

instance GenerateConstraints a (ConstantDefinition a IndexedType) where
  generateConstraints (ConstantDefinition loc _ (With _ t) e) = do
    insertConstraintsC [Equality (RuleTopLevelConstant loc) [t, typeOf e]]
    generateConstraints $
      ELet
        loc
        (BPattern loc (PVariable loc (Label t placeholder)) e :| [])
        (EVariable loc (Label t placeholder))
   where
    placeholder = "###.constant"

instance GenerateConstraints a (Definition a Kind IndexedType) where
  generateConstraints =
    \case
      DFunction _ _ (f@(FunctionDefinition loc (Just (With _ t)) (With _ t1) _ _) :| _) _ -> do
        generateConstraints f
        r <- runConstraintsGen (instantiateAnnotation loc t)
        case fst3 r of
          Left err ->
            compilerReportConstraintsGenErrors [EIllFormedTypeAnnotation err]
          Right t2 ->
            insertConstraintsC [Equality (RuleAnnotation loc t1 t2) [t1, t2]]
      DConstant _ _ c@(ConstantDefinition loc (Just (With _ t)) (With _ t1) _) _ -> do
        generateConstraints c
        r <- runConstraintsGen (instantiateAnnotation loc t)
        case fst3 r of
          Left err ->
            compilerReportConstraintsGenErrors [EIllFormedTypeAnnotation err]
          Right t2 ->
            insertConstraintsC [Equality (RuleAnnotation loc t1 t2) [t1, t2]]
      DFunction _ _ (f :| _) _ ->
        void (generateConstraints f)
      DConstant _ _ c _ ->
        void (generateConstraints c)
      DFold loc _ (FoldDefinition (With _ t) _ (Just e)) -> do
        generateConstraints e
        (r, _, _) <- runConstraintsGen (instantiateAnnotation loc t)
        case r of
          Left err ->
            compilerReportConstraintsGenErrors [EIllFormedTypeAnnotation err]
          Right t2 -> do
            insertConstraintsC [Equality (RuleAnnotation loc t1 t2) [t1, t2]]
       where
        t1 = typeOf e
      DUnfold loc _ (UnfoldDefinition (With _ t) ps d (Just e)) -> do
        generateConstraints e
        (r, _, _) <- runConstraintsGen (instantiateAnnotation loc t)
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
                  Just (CodataAccessorEntry _ _ CodataAccessor{..}) -> do
                    if "@" `Text.isPrefixOf` name
                      then do
                        let tl = typeOf (NonEmpty.head ps)
                            tr = typeOf elem1
                        pure [Equality (RuleUnfoldEquality loc name tl tr) [tl, tr]]
                      else do
                        let tl = t2 `TArrow` typeOf elem1
                        pure [Explicit (RuleUnfoldExplicit loc tl accessorScheme) tl accessorScheme]
                  Nothing ->
                    pure []
            if length cs == length fields
              then insertConstraintsC cs
              else compilerReportConstraintsGenErrors [ECodataFieldMismatch loc]
       where
        t1 = typeOf e
      _ ->
        error "Not implemented"

type ConstraintsGenResult g o a t s = (s, Dictionary (g, o a), [ConstraintsGenOutput g o a t])

runConstraintsGen :: (Monad m) => ConstraintsGenStack a TypeIndex Kind IndexedType r -> CompilerT a m (ConstraintsGenResult a TypeIndex Kind IndexedType r)
runConstraintsGen stack = do
  sup <- gets compilerSupply
  build <- getCurrentBuildC
  let (result, ConstraintsGenState{..}, output) = runConstraintsGenStack sup (emptyConstraintsGenContext{constraintsGenContextModules = build}) stack
  updateSupplyC constraintsGenStateSupply
  pure (result, constraintsGenStateTypeIndexes, output)

generateExpressionConstraints :: (Monad m, Data a, Show a) => Expression a IndexedType -> CompilerT a m ([CompilerAssumption a], [CompilerConstraint a])
generateExpressionConstraints e = do
  (assumptions, params, result) <- runConstraintsGen (emitConstraints e)
  let (errors, constraints) = partitionEithers result
  compilerReportConstraintsGenErrors errors
  compilerSetTypeAnnotationParams params
  pure (assumptions, constraints)

assumptionConstraints :: (Monad m) => CompilerAssumption a -> CompilerT a m (Either (CompilerAssumption a) (CompilerConstraint a))
assumptionConstraints Assumption{..} = do
  names <- gets compilerNameStore
  pure $
    case Environment.lookup (normalizedName assumptionName) names of
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
    Assumption loc n2 t <- ams
    let t1 = apply sub t
    [Explicit (RuleAssumptionExplicit loc t1 s) t1 s | n1 == normalizedName n2]
  sub1 <- solveC
  pure (fmap (fmap normalizeRowTypes) (apply sub1 ds), apply sub1 ams)

typeDefinitionC :: (Monad m, Data a, Show a, Eq a) => Definition a Kind IndexedType -> CompilerT a m ()
typeDefinitionC =
  \case
    DTrait loc name def -> do
      kenv <- asks compilerTypeConstructorEnvironment
      case inferTraitKinds kenv def of
        Left errs -> do
          this <- gets (principalPath . compilerCurrentModule)
          tellErrors [KindError err (ErrorLocation this loc) | err <- nub errs]
        Right (TraitDefinition _ (Parameter k q) ds) ->
          forM_ ds $
            \(n, Forall _ _ s) -> do
              env <- asks compilerTypeConstructorEnvironment
              let s1 = evalState (instantiateVars [(q, TypeIndex k 0)] env s) (1 :: Int)
              insertNameC n (Forall (typeIndexesIn s1) [Trait name (TVariable (TypeIndex k 0))] s1)
    DInstance loc trait (InstanceDefinition _ t0 ds) -> do
      env <- asks compilerTraitEnvironment
      kinds <- asks compilerTypeConstructorEnvironment
      case Environment.lookup trait env of
        Nothing ->
          error ("Missing trait: " <> Text.unpack trait)
        Just (TraitEntry _ _ p@(Parameter k _) _ traitInfoEntries) ->
          forM_ ds $
            \d -> do
              case Environment.lookup (definitionName d) traitInfoEntries of
                Nothing ->
                  error ("Missing method: " <> Text.unpack (definitionName d))
                Just s0 -> do
                  t1 <- instantiateVarsC t0
                  let s1 = instantiateTemplate (TypeIndex k 0) t1 (toIndexedScheme kinds p s0)
                  insertConstraintsC [Explicit (RuleTraitInstance loc (typeOf d) s1) (typeOf d) s1]
                  generateConstraints d
    d@(DFunction loc name (FunctionDefinition _ _ (With _ t) ps _ :| _) _) -> do
      checkIfNameExists loc name
      checkMain loc t ps name
      generateConstraints d
      sub <- solveC
      define name (typeOf (apply sub d))
    d@(DConstant loc name _ _) -> do
      checkIfNameExists loc name
      generateConstraints d
      sub <- solveC
      define name (typeOf (apply sub d))
    DImport{} -> pure ()
    DQualifiedImport{} -> pure ()
    DTypeAlias{} -> pure ()
    DType{} -> pure ()
    DCotype{} -> pure ()
    d -> do
      generateConstraints d
      sub <- solveC
      define (definitionName d) (typeOf (apply sub d))

toIndexedScheme :: Environment Kind -> Parameter Kind -> Scheme Parameter k (Type Parameter k) -> IndexedScheme
toIndexedScheme env p (Forall _ _ t) = scheme [] (toIndexedType env p t)

toIndexedType :: Environment Kind -> Parameter Kind -> Type Parameter k -> IndexedType
toIndexedType env (Parameter k n) t = evalState (instantiateVars [(n, TypeIndex k 0)] env t) (1 :: Int)

checkMain :: (Monad m, Data a) => a -> IndexedType -> NonEmpty (Pattern a IndexedType) -> Name -> CompilerT a m ()
checkMain loc t ps name = do
  path <- gets compilerCurrentModule
  when (Path ["Main"] == path && "main" == name) $
    insertConstraintsC
      [ Explicit
          (RuleEntrypoint loc t1)
          t1
          (Forall mempty [] (TIntrinsic IUnit `TArrow` TApplication KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit)))
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

instantiateTemplate :: TypeIndex Kind -> IndexedType -> IndexedScheme -> IndexedScheme
instantiateTemplate (TypeIndex _ n) t1 (Forall vs ts t) = Forall vs ts (apply (n `mapsTo` t1) t)

instantiateVarsC :: (Monad m) => Type Parameter () -> CompilerT a m IndexedType
instantiateVarsC t = do
  env <- asks compilerTypeConstructorEnvironment
  instantiateVars [] env t

define :: (Monad m) => Name -> IndexedType -> CompilerT a m ()
define name t = insertNameC name (Forall (typeIndexesIn s) [] s)
 where
  s = normalizeTypeIndexes t
