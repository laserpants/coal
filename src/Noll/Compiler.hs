{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler where

-- module Noll.Compiler (
--  CompilerEnvironment (..),
--  CompilerAssumption,
--  insertNamesC,
--  CompilerT (..),
--  CompilerState (..),
--  runCompilerT,
--  evalCompilerT,
--  updateSubstitutionC,
--  runConstraintsGenC,
----  lookupCompilerDefinitionC,
--  generateConstraintsC,
--  typeCheckExpressionC,
--  typeCheckDefinitionC,
--  typeCheckFunctionC,
--  indexedC,
--  typeCheckConstantC,
--  solveConstraintsC,
--  getConstraintsGenErrorsC,
--  getSolverRuleViolationsC,
-- ) where

import Debug.Trace
import Control.Monad.Reader (MonadReader, ReaderT, ask, asks, runReaderT)
import Control.Monad.State (MonadState, StateT, gets, modify, put, runState, runStateT)
import Control.Monad.Writer (execWriter)
import Data.Data (Data)
import Data.Either.Extra (partitionEithers)
import Debug.Trace
import Lang.Common.Environment (Environment (..))
import Lang.Common.List1 (NonEmpty ((:|)))
import Lang.Common.Supply (Supply (..), supplied)
import Lang.Label (Label (..))
import Lang.Utils (Dictionary, Name, Over, forM_, (<$$$>))
import Noll.Language
import Noll.Language.HasType (foldTypeOf)
import Noll.Language.Type.Indexed (indexed)
import Noll.Module (Constant (..), Definition (..), Function (..), definitionName)
import Noll.SystemF
import Noll.SystemF.Substitution (mapsTo)

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Lang.Common.Environment as Environment

type CompilerAssumption = Assumption IndexedType

type CompilerConstraint a = Constraint (InferenceRule Kind a) TypeIndex Kind IndexedType

type TraitImplementationEnv = Environment (Scheme TypeIndex Kind IndexedType)

data CompilerEnvironment = CompilerEnvironment
  { compilerDataConstructorEnv :: Environment (Constructor TypeIndex Kind IndexedType)
  , compilerTypeConstructorEnv :: Environment Kind
  , compilerTraitEnvironment :: Environment (TypeIndex Kind, TraitImplementationEnv)
  }
  deriving (Show, Eq, Ord, Read)

data CompilerState a = CompilerState
  { compilerConstraints :: [CompilerConstraint a]
  , compilerConstraintsGenErrors :: [ConstraintsGenError a]
  , compilerTypeAnnotationParams :: Dictionary (a, TypeIndex Kind)
  , compilerSolverRuleViolations :: [InferenceRule Kind a]
  , compilerNameEnvironment :: Environment (Scheme TypeIndex Kind IndexedType)
  , --  , compilerDefinitions :: Environment IndexedType
    compilerAssumptions :: [CompilerAssumption]
  , compilerSubstitution :: Substitution
  , compilerSupply :: Int
  }
  deriving (Show, Eq, Ord, Read)

{-# INLINE overCompilerStateConstraintsGenErrors #-}
overCompilerStateConstraintsGenErrors :: Over (CompilerState a) [ConstraintsGenError a]
overCompilerStateConstraintsGenErrors fn CompilerState{..} = CompilerState{compilerConstraintsGenErrors = fn compilerConstraintsGenErrors, ..}

{-# INLINE overCompilerTypeAnnotationParams #-}
overCompilerTypeAnnotationParams :: Over (CompilerState a) (Dictionary (a, TypeIndex Kind))
overCompilerTypeAnnotationParams fn CompilerState{..} = CompilerState{compilerTypeAnnotationParams = fn compilerTypeAnnotationParams, ..}

{-# INLINE overCompilerSolverRuleViolations #-}
overCompilerSolverRuleViolations :: Over (CompilerState a) [InferenceRule Kind a]
overCompilerSolverRuleViolations fn CompilerState{..} = CompilerState{compilerSolverRuleViolations = fn compilerSolverRuleViolations, ..}

{-# INLINE overCompilerNameEnvironment #-}
overCompilerNameEnvironment :: Over (CompilerState a) (Environment (Scheme TypeIndex Kind IndexedType))
overCompilerNameEnvironment fn CompilerState{..} = CompilerState{compilerNameEnvironment = fn compilerNameEnvironment, ..}

-- {-# INLINE overCompilerDefinitions #-}
-- overCompilerDefinitions :: Over (CompilerState a) (Environment IndexedType)
-- overCompilerDefinitions fn CompilerState{..} = CompilerState{compilerDefinitions = fn compilerDefinitions, ..}

{-# INLINE overCompilerSupply #-}
overCompilerSupply :: Over (CompilerState a) Int
overCompilerSupply fn CompilerState{..} = CompilerState{compilerSupply = fn compilerSupply, ..}

{-# INLINE overCompilerConstraints #-}
overCompilerConstraints :: Over (CompilerState a) [CompilerConstraint a]
overCompilerConstraints fn CompilerState{..} = CompilerState{compilerConstraints = fn compilerConstraints, ..}

{-# INLINE overCompilerAssumptions #-}
overCompilerAssumptions :: Over (CompilerState a) [CompilerAssumption]
overCompilerAssumptions fn CompilerState{..} = CompilerState{compilerAssumptions = fn compilerAssumptions, ..}

{-# INLINE overCompilerSubstitution #-}
overCompilerSubstitution :: Over (CompilerState a) Substitution
overCompilerSubstitution fn CompilerState{..} = CompilerState{compilerSubstitution = fn compilerSubstitution, ..}

{-# INLINE initialCompilerState #-}
initialCompilerState :: CompilerState a
initialCompilerState =
  CompilerState
    { compilerConstraints = []
    , compilerConstraintsGenErrors = []
    , compilerTypeAnnotationParams = mempty
    , compilerSolverRuleViolations = []
    , compilerNameEnvironment = mempty
    , --    , compilerDefinitions = mempty
      compilerAssumptions = []
    , compilerSubstitution = mempty
    , compilerSupply = 0
    }

-- RWS?
newtype CompilerT a m c = Compiler {compilerStack :: ReaderT CompilerEnvironment (StateT (CompilerState a) m) c}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader CompilerEnvironment
    , MonadState (CompilerState a)
    )

instance Supply (CompilerState a) where
  updateSupply = overCompilerSupply
  getSupply = compilerSupply

{-# INLINE compilerReportConstraintsGenErrors #-}
compilerReportConstraintsGenErrors :: (Monad m) => [ConstraintsGenError a] -> CompilerT a m ()
compilerReportConstraintsGenErrors errors = modify (overCompilerStateConstraintsGenErrors (<> errors))

{-# INLINE compilerSetTypeAnnotationParams #-}
compilerSetTypeAnnotationParams :: (Monad m) => Dictionary (a, TypeIndex Kind) -> CompilerT a m ()
compilerSetTypeAnnotationParams params = modify (overCompilerTypeAnnotationParams (const params))

{-# INLINE compilerReportSolverRuleViolations #-}
compilerReportSolverRuleViolations :: (Monad m) => [InferenceRule Kind a] -> CompilerT a m ()
compilerReportSolverRuleViolations errors = modify (overCompilerSolverRuleViolations (<> errors))

{-# INLINE getConstraintsGenErrorsC #-}
getConstraintsGenErrorsC :: (Monad m) => CompilerT a m [ConstraintsGenError a]
getConstraintsGenErrorsC = gets compilerConstraintsGenErrors

{-# INLINE getSolverRuleViolationsC #-}
getSolverRuleViolationsC :: (Monad m) => CompilerT a m [InferenceRule Kind a]
getSolverRuleViolationsC = gets compilerSolverRuleViolations

{-# INLINE insertNameC #-}
insertNameC :: (Monad m) => Name -> Scheme TypeIndex Kind IndexedType -> CompilerT a m ()
insertNameC name scheme = modify (overCompilerNameEnvironment (Environment.insert name scheme))

-- {-# INLINE insertDefinitionC #-}
-- insertCompilerDefinitionC :: (Monad m) => Name -> IndexedType -> CompilerT a m ()
-- insertCompilerDefinitionC name t = modify (overCompilerDefinitions (Environment.insert name t))

-- {-# INLINE lookupCompilerDefinitionC #-}
-- lookupCompilerDefinitionC :: (Monad m) => Name -> CompilerT a m (Maybe IndexedType)
-- lookupCompilerDefinitionC name = Environment.lookup name <$> gets compilerDefinitions

{-# INLINE insertNamesC #-}
insertNamesC :: (Monad m) => [(Name, Scheme TypeIndex Kind IndexedType)] -> CompilerT a m ()
insertNamesC names = modify (overCompilerNameEnvironment (Environment.insertMultiple names))

{-# INLINE updateSupplyC #-}
updateSupplyC :: (Monad m) => Int -> CompilerT a m ()
updateSupplyC supply = modify (overCompilerSupply (const supply))

{-# INLINE updateSubstitutionC #-}
updateSubstitutionC :: (Monad m) => Substitution -> CompilerT a m ()
updateSubstitutionC sub = modify (overCompilerSubstitution (const sub))

{-# INLINE insertConstraintsC #-}
insertConstraintsC :: (Monad m) => [CompilerConstraint a] -> CompilerT a m ()
insertConstraintsC cs = modify (overCompilerConstraints (<> cs))

{-# INLINE clearConstraintsC #-}
clearConstraintsC :: (Monad m) => CompilerT a m ()
clearConstraintsC = modify (overCompilerConstraints (const mempty))

{-# INLINE insertAssumptionsC #-}
insertAssumptionsC :: (Monad m) => [CompilerAssumption] -> CompilerT a m ()
insertAssumptionsC as = modify (overCompilerAssumptions (<> as))

{-# INLINE runCompilerT #-}
runCompilerT :: (Monad m) => CompilerEnvironment -> CompilerT a m c -> m (c, CompilerState a)
runCompilerT env com = runStateT (runReaderT (compilerStack com) env) initialCompilerState

{-# INLINE evalCompilerT #-}
evalCompilerT :: (Monad m) => CompilerEnvironment -> CompilerT a m c -> m c
evalCompilerT = fst <$$$> runCompilerT

type ConstraintsGenResult c o k t r = (r, Dictionary (c, o k), [ConstraintsGenOutput c o k t])

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
      , constraintsGenContextDataConstructorEnv = compilerDataConstructorEnv
      , constraintsGenContextTypeConstructorEnv = compilerTypeConstructorEnv
      }

generateConstraintsC :: (Show a, Monad m, Data a) => Expression a IndexedType -> CompilerT a m ([CompilerAssumption], [CompilerConstraint a])
generateConstraintsC e = do
  (assumptions, params, result) <- runConstraintsGenC (collectConstraints e)
  let (errors, constraints) = partitionEithers result
  compilerReportConstraintsGenErrors errors
  compilerSetTypeAnnotationParams params
  pure (assumptions, constraints)

assumptionConstraints :: (Monad m) => CompilerAssumption -> CompilerT a m (Either CompilerAssumption (CompilerConstraint a))
assumptionConstraints Assumption{..} = do
  names <- gets compilerNameEnvironment
  case Environment.lookup assumptionName names of
    Nothing ->
      pure $ Left Assumption{..}
    Just s ->
      pure $ Right (Explicit (InferenceRule 220) assumptionType s)

solveConstraintsC :: (Monad m, Data a, Show a, Eq a) => [CompilerConstraint a] -> CompilerT a m Substitution
solveConstraintsC cs = do
  dict <- gets compilerTypeAnnotationParams
  n <- gets compilerSupply
  let (sub, m, rs) = solveConstraints n cs
  updateSupplyC m
  let errors = execWriter (checkTypeAnnotationParameters (Map.toList dict) sub)
  compilerReportSolverRuleViolations (apply sub rs)
  compilerReportConstraintsGenErrors (IllFormedTypeAnnotation <$> errors)
  pure sub

--solveC :: (Monad m, Data a, Show a, Eq a) => [CompilerConstraint a] -> CompilerT a m ()
--solveC constraints = do
--  undefined
--  sub1 <- gets compilerSubstitution
--  sub2 <- solveConstraintsC constraints
--  updateSubstitutionC (sub2 <> sub1)

--

compileConstraintsC :: (Show a, Monad m, Data a) => Expression a IndexedType -> CompilerT a m ()
compileConstraintsC expr = do
  (ms1, cs1) <- generateConstraintsC expr
  (ms2, cs2) <- partitionEithers <$> traverse assumptionConstraints ms1
  sub <- gets compilerSubstitution
  insertAssumptionsC (apply sub ms2)
  insertConstraintsC (cs1 <> cs2)

compileFunctionC :: (Show a, Monad m, Data a) => Function Expression a IndexedType -> CompilerT a m ()
compileFunctionC (Function loc (With _ t) ps e) = do
  insertConstraintsC [Equality (InferTopLevelFunction loc) [t, typeOf e]]
  t1 <- supplied (TVariable . TypeIndex KType)
  compileConstraintsC $
    ELet
      loc
      (BFunction loc placeholder ps e :| [])
      (EVariable loc (Label (foldTypeOf t1 ps) placeholder))
 where
  placeholder = "###.function"

compileConstantC :: (Show a, Monad m, Data a) => Constant Expression a IndexedType -> CompilerT a m ()
compileConstantC (Constant loc (With _ t) e) = do
  insertConstraintsC [Equality (InferTopLevelConstant loc) [t, typeOf e]]
  compileConstraintsC $
    ELet
      loc
      (BPattern loc (PVariable loc (Label t placeholder)) e :| [])
      (EVariable loc (Label t placeholder))
 where
  placeholder = "###.constant"

compileDefinitionC :: (Monad m, Data a, Show a, Eq a) => Definition a k IndexedType -> CompilerT a m ()
compileDefinitionC = do
  \case
    DFunction _ f ->
      compileFunctionC f
    DConstant _ c ->
      compileConstantC c
    DAnnotation a d -> do
      -- TODO
      compileDefinitionC d
    DInstance _ t ds ->
      forM_ ds compileDefinitionC
    _ ->
      -- TODO ?
      pure ()

solveC :: (Monad m, Data a, Show a, Eq a) => CompilerT a m Substitution
solveC = do
  constraints <- gets compilerConstraints
  sub1 <- gets compilerSubstitution
  sub2 <- solveConstraintsC constraints
  clearConstraintsC
  -- TODO: clear typeAnnotationParameters ????
  updateSubstitutionC (sub2 <> sub1)
  gets compilerSubstitution

--

--compileConstraintsC ::
--  ( Functor f
--  , Monad m
--  , Substitutable (f IndexedType)
--  , TypeIndexed Kind (f IndexedType)
--  , Show a
--  , Data a
--  , Eq a
--  ) =>
--  [CompilerConstraint a] ->
--  f IndexedType ->
--  Expression a IndexedType ->
--  CompilerT a m ()
--compileConstraintsC cs o e = do
--  undefined
----  (ms0, cs0) <- generateConstraintsC e
----
----  (ms1, cs1) <- partitionEithers <$> traverse assumptionConstraints ms0
----  sub <- gets compilerSubstitution
----  insertAssumptionsC (apply sub ms1)
----  solveC (cs <> cs0 <> cs1)

--typeCheckExpressionC ::
--  (Monad m, Data a, Show a, Eq a) =>
--  Expression a IndexedType ->
--  CompilerT a m (Expression a IndexedType, [CompilerAssumption])
--typeCheckExpressionC e = do
--  undefined
--  compileConstraintsC [] e e
--  ams <- gets compilerAssumptions
--  sub <- gets compilerSubstitution
--  pure (normalizeRowTypes <$> apply sub e, apply sub ams)

--compileFunctionC ::
--  (Monad m, Data a, Show a, Eq a) =>
--  Function Expression a IndexedType ->
--  CompilerT a m ()
--compileFunctionC f@(Function loc (With _ t) ps e) = do
--  undefined
--  t1 <- supplied (TVariable . TypeIndex KType)
--  let t2 = foldTypeOf t1 ps
--  compileConstraintsC [Equality (InferenceRule 999) [t, typeOf e]] f $
--    ELet
--      loc
--      (BFunction loc placeholder ps e :| [])
--      (EVariable loc (Label t2 placeholder))
-- where
--  placeholder = "###.function"

--typeCheckFunctionC ::
--  (Monad m, Data a, Show a, Eq a) =>
--  Function Expression a IndexedType ->
--  CompilerT a m (Function Expression a IndexedType, [CompilerAssumption])
--typeCheckFunctionC f = do
--  undefined
--  compileFunctionC f
--  ams <- gets compilerAssumptions
--  sub <- gets compilerSubstitution
--  pure (normalizeRowTypes <$> apply sub f, ams)

--compileConstantC ::
--  (Monad m, Data a, Show a, Eq a) =>
--  Constant Expression a IndexedType ->
--  CompilerT a m ()
--compileConstantC g@(Constant loc (With _ t) e) = do
--  undefined
--  sub <- gets compilerSubstitution
--  compileConstraintsC [Equality (InferenceRule 999) [t, typeOf e]] g $
--    ELet
--      loc
--      (BPattern loc (PVariable loc (Label t placeholder)) e :| [])
--      (EVariable loc (Label t placeholder))
-- where
--  placeholder = "###.constant"

--typeCheckConstantC ::
--  (Monad m, Data a, Show a, Eq a) =>
--  Constant Expression a IndexedType ->
--  CompilerT a m (Constant Expression a IndexedType, [CompilerAssumption])
--typeCheckConstantC c = do
--  undefined
--  compileConstantC c
--  ams <- gets compilerAssumptions
--  sub <- gets compilerSubstitution
--  pure (normalizeRowTypes <$> apply sub c, ams)
--
--typeCheckModuleC = do
--  undefined
--
--compileDefinitionC :: (Monad m, Data a, Show a, Eq a) => Definition a k IndexedType -> CompilerT a m ()
--compileDefinitionC = do
--  undefined
--  \case
--    DFunction _ f ->
--      compileFunctionC f
--    DConstant _ c ->
--      compileConstantC c

insertDefinitionC :: (Monad m, HasType TypeIndex Kind (Definition a k (Type TypeIndex Kind))) => Definition a k IndexedType -> CompilerT a m ()
insertDefinitionC =
  \case
    d@(DFunction name _) -> do
      insertNameC name (Forall (typeIndexesIn t) [] t)
     where
      --      insertCompilerDefinitionC name t

      t = normalizeTypeIndexes (typeOf d)

--typeCheckDefinitionC ::
--  ( Monad m
--  , Show a
--  , Data a
--  , Data k
--  , Ord k
--  , Eq a
--  , HasType TypeIndex Kind (Definition a k (Type TypeIndex Kind))
--  , TypeIndexed Kind (Definition a k IndexedType)
--  ) =>
--  Definition a k IndexedType ->
--  CompilerT a m (Definition a k IndexedType, [CompilerAssumption])
--typeCheckDefinitionC d = do
--  undefined
--  compileDefinitionC d
--  ams <- gets compilerAssumptions
--  sub <- gets compilerSubstitution
--  let def = normalizeRowTypes <$> apply sub d
--  insertDefinitionC def
--  pure (def, ams)

-- typeCheckDefinitionsC ::
--  ( Monad m
--  , Show a
--  , Show k
--  , Eq a
----  , HasType TypeIndex Kind (Definition a k IndexedType)
--  ) =>
--  [Definition a k IndexedType] ->
--  CompilerT a m ([Definition a Kind IndexedType], [CompilerAssumption])
typeCheckDefinitionsC ds = do
  forM_ ds typeCheckDefinition
  sub <- gets compilerSubstitution
  ams <- gets compilerAssumptions
  Environment env <- gets compilerNameEnvironment
  insertConstraintsC $ do
    (n1, s) <- Map.toList env
    Assumption n2 t <- ams
    [Explicit (InferenceRule 200) (apply sub t) s | n1 == n2]
  sub <- solveC
  pure (fmap (fmap normalizeRowTypes) (apply sub ds), apply sub ams)

typeCheckDefinition d =
  case d of
    DImport{} ->
      pure ()
    DTrait{} ->
      pure ()
    DTypeAlias{} ->
      pure ()
    DType{} ->
      pure ()
    DSignature{} ->
      pure ()
    DInstance trait t1 ds -> do
      traitEnvironment <- asks compilerTraitEnvironment
      case Environment.lookup trait traitEnvironment of
        Nothing ->
          error ("Missing trait: " <> Text.unpack trait)
        Just (tx, defs) -> do
          forM_ ds $ \d -> do
            case Environment.lookup (definitionName d) defs of
              Nothing ->
                error ("Missing implementation: " <> Text.unpack (definitionName d))
              Just s -> do
                insertConstraintsC [Explicit (InferenceRule 999) (typeOf d) (instantiateTemplate tx t1 s)]
                compileDefinitionC d
    _ -> do
      compileDefinitionC d
      sub <- solveC
      -- traceShowM (definitionName d, typeOf (apply sub d) :: Type TypeIndex Kind)
      defineC (definitionName d) (typeOf (apply sub d))

instantiateTemplate :: TypeIndex Kind -> IndexedType -> Scheme TypeIndex Kind IndexedType -> Scheme TypeIndex Kind IndexedType
instantiateTemplate (TypeIndex _ n) t1 (Forall vs ts t) = Forall vs ts (apply (n `mapsTo` t1) t)

defineC :: (Monad m) => Name -> IndexedType -> CompilerT a m ()
defineC name t = insertNameC name (Forall (typeIndexesIn s) [] s)
 where
  s = normalizeTypeIndexes t

indexedC :: (Monad m, Traversable t) => t e -> CompilerT a m (t IndexedType)
indexedC t = do
  (r, q) <- gets (runState (indexed t) . compilerSupply)
  updateSupplyC q
  return r
