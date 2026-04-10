{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

module Coal.Compiler.TypeInference where -- (typeDefinitionsC, toIndexedType, toIndexedScheme) where

import Coal.AST.Type.Parameterized
import Coal.Common.Environment (Environment (..))
import qualified Coal.Common.Environment as Environment
import Coal.Common.Label (Label (..))
import Coal.Common.Supply (supplied)
import Coal.Compiler.Journal (tellErrors)
import Coal.Compiler.Stack
import Coal.Language
import Coal.Language.Module.Path
import Coal.ProtoCompiler.KindEnvironment (moduleKindEnvironment)
import Coal.ProtoCompiler.ProtoBuild
import Coal.ProtoCompiler.ProtoBuild.ProtoNameEntry
import Coal.ProtoCompiler.ProtoStack (ProtoCompilerT (..), protoOclearConstraintsC, protoOclearKindConstraintsC, protoOclearTypeAnnotationParamsC, protoOcompilerReportConstraintsGenErrors, protoOcompilerReportKindConstraintsGenErrors, protoOcompilerReportSolverRuleViolations, protoOgetCurrentBuildC, protoOinsertAssumptionsC, protoOinsertConstraintsC, protoOinsertKindConstraintsC, protoOinsertNameC, protoOsetSubstitutionC, protoOupdateSupplyC, setTypeAnnotationParamsC)
import Coal.ProtoCompiler.ProtoState (ProtoCompilerState (..))
import Coal.ProtoLanguage.ProtoDefinition
import Coal.ProtoLanguage.ProtoModule
import Coal.ProtoTypeSystem.Kind.Constraint.Generation
import Coal.ProtoTypeSystem.Parameterized
import Coal.TypeSystem
import Control.Monad.Except (MonadError (..), forM_)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (evalState, get, gets)
import Control.Monad.Trans (lift)
import Control.Monad.Writer (execWriter)
import Data.Data (Data)
import Data.Either.Extra (partitionEithers)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Extras (Dictionary, Name)

generateKindConstraints :: (Monad m) => ProtoModule a Kind () -> ProtoCompilerT m a ()
generateKindConstraints modul = do
  env <- moduleKindEnvironment modul
  (_, result) <- runProtoKindConstraintsGen env (protoOemitKindConstraints modul)
  let (errors, constraints) = partitionEithers result
  protoOinsertKindConstraintsC constraints
  protoOcompilerReportKindConstraintsGenErrors errors

class ProtoGenerateConstraints a c where
  protoOgenerateConstraints :: (Monad m) => c -> ProtoCompilerT m a ()

instance (Data a, Show a) => ProtoGenerateConstraints a (Expression a Kind IndexedType) where
  protoOgenerateConstraints expr = do
    (asms1, cs1) <- protoOgenerateExpressionConstraints expr
    (asms2, cs2) <- partitionEithers <$> traverse protoOassumptionConstraints asms1
    sub <- gets protoOcompilerSubstitution
    protoOinsertAssumptionsC (apply sub asms2)
    protoOinsertConstraintsC (cs1 <> cs2)

instance (Show a, Data a) => ProtoGenerateConstraints a (ProtoDefinition a Kind IndexedType) where
  protoOgenerateConstraints =
    \case
      ProtoDFunction
        _
        name
        ProtoFunctionDefinition
          { protoOfunctionDefinitionMetadata = loc
          , protoOfunctionDefinitionType = With _ functionType
          , ..
          } -> do
          protoOinsertConstraintsC
            [ Equality
                (RuleTopLevelFunction loc)
                [ functionType
                , typeOf protoOfunctionDefinitionExpression
                ]
            ]
          expressionType <- freshTypeVariable
          protoOgenerateConstraints $
            ELet
              loc
              (BFunction loc placeholder protoOfunctionDefinitionPatterns functionExpr :| mempty)
              (EVariable loc (Label expressionType placeholder))
         where
          placeholder = "#_function__" <> name
          functionExpr =
            case protoOfunctionDefinitionAnnotation of
              Nothing ->
                protoOfunctionDefinitionExpression
              Just (With _ annotationType) ->
                EAnnotation loc annotationType protoOfunctionDefinitionExpression
      ProtoDLet
        _
        name
        ProtoLetDefinition
          { protoOletDefinitionMetadata = loc
          , protoOletDefinitionType = With _ letType
          , ..
          } -> do
          protoOinsertConstraintsC
            [ Equality
                (RuleTopLevelConstant loc)
                [ letType
                , typeOf protoOletDefinitionExpression
                ]
            ]
          protoOgenerateConstraints $
            ELet
              loc
              (BPattern loc (PVariable loc (Label letType placeholder)) letExpr :| mempty)
              (EVariable loc (Label letType placeholder))
         where
          placeholder = "#_constant__" <> name
          letExpr =
            case protoOletDefinitionAnnotation of
              Nothing ->
                protoOletDefinitionExpression
              Just (With _ annotationType) ->
                EAnnotation loc annotationType protoOletDefinitionExpression
      ProtoDInstance _ ProtoInstanceDefinition{..} -> do
        ProtoBuild{..} <- protoOgetCurrentBuildC
        case Environment.lookup protoOinstanceDefinitionTraitName protoObuildTraits of
          Nothing ->
            error "TODO"
          Just ProtoTraitEntry{..} ->
            forM_ protoOinstanceDefinitionImplementations $
              \case
                d@(ProtoDFunction loc name def) ->
                  case Environment.lookup name protoOtraitEntryInterface of
                    Nothing ->
                      error "TODO"
                    Just sig -> do
                      s <- protoOtoIndexedScheme (replaceParamInScheme protoOtraitEntryParameter protoOinstanceDefinitionType sig)
                      protoOinsertConstraintsC [Explicit (RuleTraitInstance loc (typeOf d) s) (typeOf d) s]
                      protoOgenerateConstraints $ ProtoDFunction loc (instanceLabel trait name) def
                d@(ProtoDLet loc name def) ->
                  case Environment.lookup name protoOtraitEntryInterface of
                    Nothing ->
                      error "TODO"
                    Just sig -> do
                      s <- protoOtoIndexedScheme (replaceParamInScheme protoOtraitEntryParameter protoOinstanceDefinitionType sig)
                      protoOinsertConstraintsC [Explicit (RuleTraitInstance loc (typeOf d) s) (typeOf d) s]
                      protoOgenerateConstraints $ ProtoDLet loc (instanceLabel trait name) def
                _ ->
                  pure ()
           where
            trait = Trait protoOinstanceDefinitionTraitName protoOinstanceDefinitionType
      _ ->
        pure ()

protoOtoIndexedScheme :: (Monad m) => Scheme Parameter Kind (Type Parameter Kind) -> ProtoCompilerT m a (Scheme TypeIndex Kind IndexedType)
protoOtoIndexedScheme Forall{..} = do
  env <- protoOinstantiateTypeIndexes schemeTypeVariables
  flip runReaderT (Environment.fromList env) $
    Forall
      <$> toIndexed schemeTypeVariables
      <*> toIndexed schemeTraits
      <*> toIndexed schemeTypeBody

freshTypeVariable :: (Monad m) => ProtoCompilerT m a (Type TypeIndex Kind)
freshTypeVariable = supplied (TVariable . TypeIndex KType)

protoOgenerateExpressionConstraints :: (Monad m, Data a, Show a) => Expression a Kind IndexedType -> ProtoCompilerT m a ([CompilerAssumption a], [CompilerConstraint a])
protoOgenerateExpressionConstraints expr = do
  (assumptions, params, result) <- protoOrunConstraintsGen (protoOemitConstraints expr)
  let (errors, constraints) = partitionEithers result
  protoOcompilerReportConstraintsGenErrors errors
  setTypeAnnotationParamsC params
  pure (assumptions, constraints)

protoOrunConstraintsGen :: (Monad m) => ConstraintsGenStack a TypeIndex Kind IndexedType r -> ProtoCompilerT m a (r, Dictionary (a, TypeIndex Kind), [ConstraintsGenOutput a TypeIndex Kind IndexedType])
protoOrunConstraintsGen stack = do
  ProtoCompilerState{..} <- get
  ProtoBuild{..} <- protoOgetCurrentBuildC
  let (result, ConstraintsGenState{..}, output) =
        runConstraintsGenStack
          protoOcompilerSupply
          ( emptyConstraintsGenContext
              { constraintsGenContextDataConstructors =
                  Environment.mapEnvironment protoOdataConstructorEntryConstructor protoObuildDataConstructors
              , constraintsGenContextTypeConstructors =
                  Environment.mapEnvironment protoOtypeConstructorEntryKind protoObuildTypeConstructors
                    <> Environment.mapEnvironment (kindOf . protoOaliasEntryType) protoObuildAliases
              }
          )
          stack
  protoOupdateSupplyC constraintsGenStateSupply
  pure (result, constraintsGenStateTypeIndexes, output)

protoOdefine :: (Monad m) => Name -> IndexedType -> ProtoCompilerT m a ()
protoOdefine name t = protoOinsertNameC name (Forall (typeIndexesIn s) mempty s)
 where
  s = normalizeTypeIndexes t

--

-- class GenerateConstraints a o where
--  generateConstraints :: (Monad m, Data a, Show a) => o -> CompilerT a m ()

-- instance GenerateConstraints a (Expression a () IndexedType) where
--  generateConstraints expr = do
--    undefined
--    (ms1, cs1) <- generateExpressionConstraints expr
--    (ms2, cs2) <- partitionEithers <$> traverse assumptionConstraints ms1
--    sub <- gets compilerSubstitution
--    insertAssumptionsC (apply sub ms2)
--    insertConstraintsC (cs1 <> cs2)

-- instance GenerateConstraints a (FunctionDefinition a IndexedType) where
--  generateConstraints (FunctionDefinition loc ann (With _ t) ps e) = do
--    undefined
--    insertConstraintsC [Equality (RuleTopLevelFunction loc) [t, typeOf e]]
--    t1 <- supplied (TVariable . TypeIndex KType)
--    generateConstraints $
--      ELet
--        loc
--        (BFunction loc placeholder ps expr :| mempty)
--        (EVariable loc (Label t1 placeholder))
--   where
--    placeholder = "###.function"
--    expr =
--      case ann of
--        Nothing ->
--          e
--        Just (With _ t1) ->
--          EAnnotation loc t1 e

-- instance GenerateConstraints a (ConstantDefinition a IndexedType) where
--  generateConstraints (ConstantDefinition loc ann (With _ t) e) = do
--    undefined
--    insertConstraintsC [Equality (RuleTopLevelConstant loc) [t, typeOf e]]
--    generateConstraints $
--      ELet
--        loc
--        (BPattern loc (PVariable loc (Label t placeholder)) expr :| mempty)
--        (EVariable loc (Label t placeholder))
--   where
--    placeholder = "###.constant"
--    expr =
--      case ann of
--        Nothing ->
--          e
--        Just (With _ t1) ->
--          EAnnotation loc t1 e

-- instance GenerateConstraints a (Definition a Kind IndexedType) where
--  generateConstraints =
--    undefined
--    \case
--      DFunction _ _ (f@(FunctionDefinition loc (Just (With _ t)) (With _ t1) _ _) :| _) _ -> do
--        generateConstraints f
--        r <- runConstraintsGen (instantiateAnnotation loc t)
--        case fst3 r of
--          Left err ->
--            compilerReportConstraintsGenErrors [EIllFormedTypeAnnotation err]
--          Right t2 ->
--            insertConstraintsC [Equality (RuleAnnotation loc t1 t2) [t1, t2]]
--      DConstant _ _ c@(ConstantDefinition loc (Just (With _ t)) (With _ t1) _) _ -> do
--        generateConstraints c
--        r <- runConstraintsGen (instantiateAnnotation loc t)
--        case fst3 r of
--          Left err ->
--            compilerReportConstraintsGenErrors [EIllFormedTypeAnnotation err]
--          Right t2 ->
--            insertConstraintsC [Equality (RuleAnnotation loc t1 t2) [t1, t2]]
--      DFunction _ _ (f :| _) _ ->
--        void (generateConstraints f)
--      DConstant _ _ c _ ->
--        void (generateConstraints c)
--      _ ->
--        error "Not implemented"

type ConstraintsGenResult g o a t s =
  ( s
  , Dictionary (g, o a)
  , [ConstraintsGenOutput g o a t]
  )

---- TODO: remove
-- tmpConvert1 :: DataConstructorEntry a -> DataConstructor TypeIndex Kind IndexedType -- ProtoDataConstructorEntry a
-- tmpConvert1 (DataConstructorEntry v1 v2 v3 v4) = v3 -- ProtoDataConstructorEntry v1 v2 v3 v4
--
---- TODO: remove
-- tmpConvert2 :: TypeConstructorEntry a -> Kind -- ProtoTypeConstructorEntry a
-- tmpConvert2 (TypeConstructorEntry v1 v2 v3 v4) = v3 -- ProtoTypeConstructorEntry v1 v2 v3 v4

-- runConstraintsGen :: (Monad m) => ConstraintsGenStack a TypeIndex Kind IndexedType r -> CompilerT a m (ConstraintsGenResult a TypeIndex Kind IndexedType r)
-- runConstraintsGen stack = do
--  undefined
--  sup <- gets compilerSupply
--  build <- getCurrentBuildC
--  let (result, ConstraintsGenState{..}, output) =
--        runConstraintsGenStack
--          sup
--          ( emptyConstraintsGenContext
--              { constraintsGenContextDataConstructors = Environment.mapEnvironment tmpConvert1 (moduleDataConstructors build)
--              , constraintsGenContextTypeConstructors =
--                  Environment.mapEnvironment tmpConvert2 (moduleTypeConstructors build)
--              }
--          )
--          stack
--  updateSupplyC constraintsGenStateSupply
--  pure (result, constraintsGenStateTypeIndexes, output)

-- generateExpressionConstraints :: (Monad m, Data a, Show a) => Expression a () IndexedType -> CompilerT a m ([CompilerAssumption a], [CompilerConstraint a])
-- generateExpressionConstraints e = do
--  undefined
--  (assumptions, params, result) <- runConstraintsGen (emitConstraints e)
--  let (errors, constraints) = partitionEithers result
--  compilerReportConstraintsGenErrors errors
--  compilerSetTypeAnnotationParams params
--  pure (assumptions, constraints)

-- assumptionConstraints :: (Monad m) => CompilerAssumption a -> CompilerT a m (Either (CompilerAssumption a) (CompilerConstraint a))
-- assumptionConstraints Assumption{..} = do
--  undefined
--  names <- gets compilerNameStore
--  pure $
--    case Environment.lookup (normalizedName assumptionName) names of
--      Nothing ->
--        Left Assumption{..}
--      Just s ->
--        Right (Explicit (RuleTypeConstraint assumptionMetadata assumptionName assumptionType s) assumptionType s)

protoOassumptionConstraints :: (Monad m) => CompilerAssumption a -> ProtoCompilerT m a (Either (CompilerAssumption a) (CompilerConstraint a))
protoOassumptionConstraints Assumption{..} = do
  names <- gets protoOcompilerNameStore
  pure $
    case Environment.lookup (normalizedName assumptionName) names of
      Nothing ->
        Left Assumption{..}
      Just s ->
        Right (Explicit (RuleTypeConstraint assumptionMetadata assumptionName assumptionType s) assumptionType s)

-- solveConstraintsC :: (Monad m, Data a, Eq a) => [CompilerConstraint a] -> CompilerT a m Substitution
-- solveConstraintsC cs = do
--  undefined
--  dict <- gets compilerTypeAnnotationParams
--  n <- gets compilerSupply
--  let (sub, m, rs) = solveConstraints n cs
--  updateSupplyC m
--  let errors = execWriter (checkTypeAnnotationParameters (Map.toList dict) sub)
--  compilerReportSolverRuleViolations (apply sub rs)
--  compilerReportConstraintsGenErrors (EIllFormedTypeAnnotation <$> errors)
--  pure sub

solveConstraintsX :: (Monad m, Data a, Eq a) => [CompilerConstraint a] -> ProtoCompilerT m a Substitution
solveConstraintsX constraints = do
  dict <- gets protoOcompilerTypeAnnotationParams
  n <- gets protoOcompilerSupply
  let (sub, m, rs) = solveConstraints n constraints
  protoOupdateSupplyC m
  let errors = execWriter (checkTypeAnnotationParameters (Map.toList dict) sub)
  protoOcompilerReportSolverRuleViolations (apply sub rs)
  protoOcompilerReportConstraintsGenErrors (EIllFormedTypeAnnotation <$> errors)
  pure sub

-- solveC :: (Monad m, Data a, Eq a) => CompilerT a m Substitution
-- solveC = do
--  undefined
--  constraints <- gets compilerConstraints
--  sub1 <- gets compilerSubstitution
--  sub2 <- solveConstraintsC constraints
--  clearConstraintsC
--  clearTypeAnnotationParamsC
--  setSubstitutionC (sub2 <> sub1)
--  gets compilerSubstitution

solveX :: (Monad m, Data a, Eq a) => ProtoCompilerT m a Substitution
solveX = do
  constraints <- gets protoOcompilerConstraints
  sub1 <- gets protoOcompilerSubstitution
  sub2 <- solveConstraintsX constraints
  protoOclearConstraintsC
  protoOclearKindConstraintsC
  protoOclearTypeAnnotationParamsC
  protoOsetSubstitutionC (sub2 <> sub1)
  gets protoOcompilerSubstitution

-- typeDefinitionsC :: (Monad m, Data a, Show a, Eq a) => [Definition a Kind IndexedType] -> CompilerT a m ([Definition a Kind IndexedType], [CompilerAssumption a])
-- typeDefinitionsC ds = do
--  undefined
--  forM_ ds typeDefinitionC
--  sub <- gets compilerSubstitution
--  ams <- gets compilerAssumptions
--  Environment env <- gets compilerNameStore
--  insertConstraintsC $ do
--    (n1, s) <- Map.toList env
--    Assumption loc n2 t <- ams
--    let t1 = apply sub t
--    [Explicit (RuleAssumptionExplicit loc t1 s) t1 s | n1 == normalizedName n2]
--  sub1 <- solveC
--  pure (fmap (fmap normalizeRowTypes) (apply sub1 ds), apply sub1 ams)

-- typeDefinitionC :: (Monad m, Data a, Show a, Eq a) => Definition a Kind IndexedType -> CompilerT a m ()
-- typeDefinitionC =
--  undefined
--  \case
--    DTrait loc name def -> do
--      kenv <- asks compilerTypeConstructorEnvironment
--      case inferTraitKinds kenv def of
--        Left errs -> do
--          this <- gets (principalPath . compilerCurrentModule)
--          tellErrors [KindError err (ErrorLocation this loc) | err <- nub errs]
--        Right (TraitDefinition _ (Parameter k q) ds) ->
--          forM_ ds $
--            \(n, Forall _ _ s) -> do
--              env <- asks compilerTypeConstructorEnvironment
--              let s1 = evalState (instantiateVars [(q, TypeIndex k 0)] env s) (1 :: Int)
--              case s1 of
--                Left err -> do
--                  path <- gets compilerCurrentModule
--                  tellErrors [KindError err (ErrorLocation (principalPath path) loc)]
--                  throwError PreflightFailure
--                Right sch ->
--                  insertNameC n (Forall (typeIndexesIn sch) (Set.fromList [Trait name (TVariable (TypeIndex k 0))]) sch)
--    DInstance loc trait (InstanceDefinition _ t0 ds) -> do
--      env <- undefined -- asks compilerTraitEnvironment
--      kinds <- asks compilerTypeConstructorEnvironment
--      case Environment.lookup trait env of
--        Nothing ->
--          -- TODO: Handle error
--          error ("Missing trait: " <> Text.unpack trait)
--        Just (ProtoTraitEntry _ _ p@(Parameter k _) _ traitInfoEntries) ->
--          forM_ ds $
--            \d -> do
--              case Environment.lookup (definitionName d) traitInfoEntries of
--                Nothing ->
--                  -- TODO: Handle error
--                  error ("Missing method: " <> Text.unpack (definitionName d))
--                Just s0 -> do
--                  t1 <- instantiateVarsC loc t0
--                  sch <- toIndexedScheme loc kinds p s0
--                  let s1 = instantiateTemplate (TypeIndex k 0) t1 sch
--                  insertConstraintsC [Explicit (RuleTraitInstance loc (typeOf d) s1) (typeOf d) s1]
--                  generateConstraints d
--                  sub <- solveC
--                  define (instanceLabel (Trait trait t0) (definitionName d)) (typeOf (apply sub d))
--    d@(DFunction loc name (FunctionDefinition _ _ (With _ t) ps _ :| _) _) -> do
--      checkIfNameExists loc name
--      checkMain loc t ps name
--      generateConstraints d
--      sub <- solveC
--      define name (typeOf (apply sub d))
--    d@(DConstant loc name _ _) -> do
--      checkIfNameExists loc name
--      generateConstraints d
--      sub <- solveC
--      define name (typeOf (apply sub d))
--    DImport{} ->
--      pure ()
--    DQualifiedImport{} ->
--      pure ()
--    DTypeAlias{} ->
--      pure ()
--    DType{} ->
--      pure ()
--    d -> do
--      generateConstraints d
--      sub <- solveC
--      define (definitionName d) (typeOf (apply sub d))

-- toIndexedScheme :: (Monad m) => a -> Environment Kind -> Parameter Kind -> Scheme Parameter k (Type Parameter k) -> CompilerT a m IndexedScheme
-- toIndexedScheme loc env p (Forall _ _ t) = scheme mempty <$> toIndexedType loc env p t
--
-- toIndexedType :: (Monad m) => a -> Environment Kind -> Parameter Kind -> Type Parameter k -> CompilerT a m IndexedType
-- toIndexedType loc env (Parameter k n) t =
--  case evalState (instantiateVars [(n, TypeIndex k 0)] env t) (1 :: Int) of
--    Left err -> do
-----      path--  <- gets compilerCurrentModule
--      path <- gets protoOcompilerCurrentPath
--      tellErrors [KindError err (ErrorLocation (principalPath path) loc)]
--      throwError PreflightFailure
--    Right r ->
--      pure r

-- checkMain :: (Monad m, Data a) => a -> IndexedType -> NonEmpty (Pattern a () IndexedType) -> Name -> CompilerT a m ()
-- checkMain loc t ps name = undefined -- do
--  path <- gets compilerCurrentModule
--  when (Path ["Main"] == path && "main" == name) $
--    insertConstraintsC
--      [ Explicit
--          (RuleEntrypoint loc t1)
--          t1
--          (Forall mempty mempty (TIntrinsic IUnit `TArrow` TApplication KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit)))
--      ]
-- where
--  t1 = foldTypeOf t ps

-- checkIfNameExists :: (Monad m) => a -> Name -> CompilerT a m ()
-- checkIfNameExists loc name = undefined -- do
--  env <- gets compilerNameStore
--  when (Environment.contains name env) $ do
--    path <- gets compilerCurrentModule
--    tellErrors [NameAlreadyDefined name (ErrorLocation (principalPath path) loc)]
--    throwError PreflightFailure

-- instantiateTemplate :: TypeIndex Kind -> IndexedType -> IndexedScheme -> IndexedScheme
-- instantiateTemplate (TypeIndex _ n) t1 (Forall vs ts t) = Forall vs ts (apply (n `mapsTo` t1) t)

-- instantiateVarsC :: (Monad m) => a -> Type Parameter () -> CompilerT a m IndexedType
-- instantiateVarsC loc t = do
--  undefined
--  env <- asks compilerTypeConstructorEnvironment
--  r <- instantiateVars mempty env t
--  case r of
--    Left err -> do
--      path <- gets compilerCurrentModule
--      tellErrors [KindError err (ErrorLocation (principalPath path) loc)]
--      throwError PreflightFailure
--    Right t1 ->
--      pure t1
