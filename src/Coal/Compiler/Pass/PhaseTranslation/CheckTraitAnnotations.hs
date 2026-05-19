{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Pass.PhaseTranslation.CheckTraitAnnotations (
  passCheckTraitAnnotations,
) where

import qualified Coal.Common.Environment as Environment
import Coal.Common.Supply (supplied)
import Coal.Compiler.Journal (listenErrors, tellErrors)
import Coal.Compiler.Metadata (Metadata (..))
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Compiler.State
import Coal.Language
import Coal.Language.Module.Path (principalPath)
import Coal.TypeSystem.Substitution
import Coal.TypeSystem.Unification
import Coal.Utils (lexOrderRank)
import Control.Monad (unless, when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (gets)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Set as Set
import Extras (Name, concatMapM, foldrM, forM_)

passCheckTraitAnnotations :: (MonadIO m) => Pass Metadata m (Module Metadata Kind IndexedType) (Module Metadata Kind IndexedType)
passCheckTraitAnnotations = Pass{runPass = passImpl}

passImpl :: (MonadIO m) => Module Metadata Kind IndexedType -> CompilerT Metadata m (Module Metadata Kind IndexedType)
passImpl m = do
  setCurrentModuleC m
  (_, es) <- listenErrors $ checkTraitAnnotations m
  unless (null es) (throwError TraitAnnotationError)
  return m

-- | Check if a return type annotation contains explicit type variables
isExplicitReturnType :: Maybe (Type Parameter Kind) -> Bool
isExplicitReturnType =
  \case
    Just t -> hasExplicitTypeVariables t
    Nothing -> False

-- | Determine if a type contains explicit type variables (TVariable with Parameter)
hasExplicitTypeVariables :: Type Parameter Kind -> Bool
hasExplicitTypeVariables =
  \case
    TVariable _ -> True
    TArrow t1 t2 -> hasExplicitTypeVariables t1 || hasExplicitTypeVariables t2
    TApplication _ t1 t2 -> hasExplicitTypeVariables t1 || hasExplicitTypeVariables t2
    TRecord t -> hasExplicitTypeVariables t
    TRow row -> hasExplicitTypeVariablesInRow row
    TAlias _ ts t -> any hasExplicitTypeVariables ts || hasExplicitTypeVariables t
    _ -> False

hasExplicitTypeVariablesInRow :: Row Parameter Kind (Type Parameter Kind) -> Bool
hasExplicitTypeVariablesInRow =
  \case
    RExtend _ t row -> hasExplicitTypeVariables t || hasExplicitTypeVariablesInRow row
    _ -> False

-- | Check if any pattern has an explicit type annotation
hasExplicitPatternAnnotations :: NonEmpty.NonEmpty (Pattern Metadata Kind IndexedType) -> Bool
hasExplicitPatternAnnotations patterns =
  any hasExplicitPatternAnnotation (NonEmpty.toList patterns)

hasExplicitPatternAnnotation :: Pattern Metadata Kind IndexedType -> Bool
hasExplicitPatternAnnotation =
  \case
    PAnnotation _ (TVariable _) _ -> True
    PAnnotation _ t _ -> hasExplicitTypeVariables t
    PConstructor _ _ ps -> any hasExplicitPatternAnnotation ps
    PRecord _ _ fields rest ->
      any hasExplicitPatternAnnotation fields || maybe False hasExplicitPatternAnnotation rest
    PListCons _ _ p1 p2 -> hasExplicitPatternAnnotation p1 || hasExplicitPatternAnnotation p2
    PListLiteral _ _ ps -> any hasExplicitPatternAnnotation ps
    PTuple _ _ ps -> any hasExplicitPatternAnnotation (NonEmpty.toList ps)
    POr _ _ p1 p2 -> hasExplicitPatternAnnotation p1 || hasExplicitPatternAnnotation p2
    PAs _ _ p -> hasExplicitPatternAnnotation p
    _ -> False

-- | Collect all type variables from a type (Parameter version - for user-written annotations)
collectTypeVariables :: Type Parameter Kind -> Set.Set Int
collectTypeVariables =
  \case
    TVariable Parameter{..} -> Set.singleton (annotationIndex parameterName)
    TArrow t1 t2 -> collectTypeVariables t1 <> collectTypeVariables t2
    TApplication _ t1 t2 -> collectTypeVariables t1 <> collectTypeVariables t2
    TRecord t -> collectTypeVariables t
    TRow row -> collectTypeVariablesInRow row
    TAlias _ ts t -> foldMap collectTypeVariables ts <> collectTypeVariables t
    _ -> Set.empty

collectTypeVariablesInRow :: Row Parameter Kind (Type Parameter Kind) -> Set.Set Int
collectTypeVariablesInRow =
  \case
    RExtend _ t row -> collectTypeVariables t <> collectTypeVariablesInRow row
    _ -> Set.empty

-- | Collect type variables from pattern annotations (user-written)
collectPatternTypeVariables :: Pattern Metadata Kind IndexedType -> Set.Set Int
collectPatternTypeVariables =
  \case
    PAnnotation _ t _ -> collectTypeVariables t
    PConstructor _ _ ps -> foldMap collectPatternTypeVariables ps
    PRecord _ _ fields rest ->
      foldMap collectPatternTypeVariables fields <> maybe Set.empty collectPatternTypeVariables rest
    PListCons _ _ p1 p2 -> collectPatternTypeVariables p1 <> collectPatternTypeVariables p2
    PListLiteral _ _ ps -> foldMap collectPatternTypeVariables ps
    PTuple _ _ ps -> foldMap collectPatternTypeVariables (NonEmpty.toList ps)
    POr _ _ p1 p2 -> collectPatternTypeVariables p1 <> collectPatternTypeVariables p2
    PAs _ _ p -> collectPatternTypeVariables p
    _ -> Set.empty

-- | Collect type variables from trait annotations (user-written)
collectTraitTypeVariables :: Trait (Type Parameter Kind) -> Set.Set Int
collectTraitTypeVariables (Trait _ t) = collectTypeVariables t

-- | Collect type variables from trait constraints (user-written)
collectConstraintTypeVariables :: [Trait (Type Parameter Kind)] -> Set.Set Int
collectConstraintTypeVariables = foldMap collectTraitTypeVariables

-- | Collect all type variables from an indexed type
collectIndexedTypeVariables :: IndexedType -> Set.Set Int
collectIndexedTypeVariables =
  \case
    TVariable (TypeIndex _ idx) -> Set.singleton idx
    TArrow t1 t2 -> collectIndexedTypeVariables t1 <> collectIndexedTypeVariables t2
    TApplication _ t1 t2 -> collectIndexedTypeVariables t1 <> collectIndexedTypeVariables t2
    TRecord t -> collectIndexedTypeVariables t
    TRow row -> collectIndexedTypeVariablesInRow row
    TAlias _ ts t -> foldMap collectIndexedTypeVariables ts <> collectIndexedTypeVariables t
    _ -> Set.empty

collectIndexedTypeVariablesInRow :: Row TypeIndex Kind IndexedType -> Set.Set Int
collectIndexedTypeVariablesInRow =
  \case
    RExtend _ t row -> collectIndexedTypeVariables t <> collectIndexedTypeVariablesInRow row
    RVariable (TypeIndex _ idx) -> Set.singleton idx
    _ -> Set.empty

-- | Check if a trait references any of the given type variables
traitReferencesVariables :: Set.Set Int -> Trait IndexedType -> Bool
traitReferencesVariables vars (Trait _ t) = typeReferencesVariables vars t

-- | Check if a type references any of the given type variables
typeReferencesVariables :: Set.Set Int -> IndexedType -> Bool
typeReferencesVariables vars =
  \case
    TVariable (TypeIndex _ idx) -> Set.member idx vars
    TArrow t1 t2 -> typeReferencesVariables vars t1 || typeReferencesVariables vars t2
    TApplication _ t1 t2 -> typeReferencesVariables vars t1 || typeReferencesVariables vars t2
    TRecord t -> typeReferencesVariables vars t
    TRow row -> rowReferencesVariables vars row
    TAlias _ ts t -> any (typeReferencesVariables vars) ts || typeReferencesVariables vars t
    _ -> False

rowReferencesVariables :: Set.Set Int -> Row TypeIndex Kind IndexedType -> Bool
rowReferencesVariables vars =
  \case
    RExtend _ t row -> typeReferencesVariables vars t || rowReferencesVariables vars row
    RVariable (TypeIndex _ idx) -> Set.member idx vars
    _ -> False

-- | Normalize parameter-based type variable indices to their substituted indexed type indices
normalizeExplicitVariables :: Substitution -> Set.Set Int -> Set.Set Int
normalizeExplicitVariables sub paramIndices =
  Set.fromList $ concatMap normalizeIndex (Set.toList paramIndices)
 where
  normalizeIndex idx =
    case apply sub (TVariable (TypeIndex KType idx)) of
      TVariable (TypeIndex _ idx') -> [idx']
      t -> Set.toList (collectIndexedTypeVariables t)

-- | Check if all inferred traits are covered by annotated traits
checkTraitCoverage ::
  (MonadIO m) =>
  Metadata ->
  Name ->
  Qualified IndexedType ->
  Qualified IndexedType ->
  CompilerT Metadata m ()
checkTraitCoverage loc name (With annTraits _) (With infTraits _) = do
  path <- gets compilerCurrentPath
  let missing = findMissingTraits annTraits infTraits
  unless (null missing) $
    tellErrors [MissingTraitAnnotation name missing (ErrorLocation (principalPath path) loc)]

-- | Find traits in inferred set that are not covered by annotated set
findMissingTraits :: [Trait IndexedType] -> [Trait IndexedType] -> [Trait IndexedType]
findMissingTraits annotated = filter (\infTrait -> not (isTraitCovered infTrait annotated))

-- | Check if a trait is covered by any trait in the list
isTraitCovered :: Trait IndexedType -> [Trait IndexedType] -> Bool
isTraitCovered (Trait name1 type1) =
  any (\(Trait name2 type2) -> name1 == name2 && typesStructurallyEqual type1 type2)

-- | Check structural equality of types (same structure, variables may differ)
typesStructurallyEqual :: IndexedType -> IndexedType -> Bool
typesStructurallyEqual t1 t2 =
  case (t1, t2) of
    (TVariable _, TVariable _) -> True
    (TArrow a1 b1, TArrow a2 b2) ->
      typesStructurallyEqual a1 a2 && typesStructurallyEqual b1 b2
    (TApplication _ a1 b1, TApplication _ a2 b2) ->
      typesStructurallyEqual a1 a2 && typesStructurallyEqual b1 b2
    (TConstructor _ c1, TConstructor _ c2) -> c1 == c2
    (TIntrinsic i1, TIntrinsic i2) -> i1 == i2
    (TRecord t1', TRecord t2') -> typesStructurallyEqual t1' t2'
    (TRow r1, TRow r2) -> rowsStructurallyEqual r1 r2
    (TAlias _ ts1 t1', TAlias _ ts2 t2') ->
      length ts1 == length ts2
        && all (uncurry typesStructurallyEqual) (zip ts1 ts2)
        && typesStructurallyEqual t1' t2'
    _ -> False

rowsStructurallyEqual :: Row TypeIndex Kind IndexedType -> Row TypeIndex Kind IndexedType -> Bool
rowsStructurallyEqual r1 r2 =
  case (r1, r2) of
    (RNil, RNil) -> True
    (RVariable _, RVariable _) -> True
    (RExtend n1 t1 r1', RExtend n2 t2 r2') ->
      n1 == n2 && typesStructurallyEqual t1 t2 && rowsStructurallyEqual r1' r2'
    _ -> False

indexedAnnotationTrait :: (MonadIO m) => Trait (Type Parameter Kind) -> CompilerT Metadata m [Trait IndexedType]
indexedAnnotationTrait =
  \case
    Trait name (TVariable p) -> do
      return [Trait name (parameterToIndexedType p)]
    _ ->
      return []

indexedConstraints :: (MonadIO m) => [Trait (Type Parameter Kind)] -> CompilerT Metadata m [Trait IndexedType]
indexedConstraints = concatMapM indexedAnnotationTrait

indexedReturnType :: (MonadIO m) => Maybe (Type Parameter Kind) -> CompilerT Metadata m IndexedType
indexedReturnType =
  \case
    Just (TVariable p) ->
      return (parameterToIndexedType p)
    _ ->
      supplied (TVariable . TypeIndex KType)

indexedPatternTypes :: (MonadIO m) => Pattern Metadata Kind IndexedType -> CompilerT Metadata m IndexedType
indexedPatternTypes =
  \case
    PAnnotation _ (TVariable p) _ ->
      return (parameterToIndexedType p)
    p ->
      return (typeOf p)

parameterToIndexedType :: Parameter Kind -> IndexedType
parameterToIndexedType Parameter{..} = TVariable (TypeIndex parameterKind (annotationIndex parameterName))

annotationIndex :: Name -> Int
annotationIndex name = negate (lexOrderRank name) - 1

liftUnifier :: (MonadIO m) => Unifier Substitution -> CompilerT Metadata m (Either UnificationError Substitution)
liftUnifier action = do
  n <- gets compilerSupply
  let (res, s) = runUnifier n action
  updateSupplyC s
  return res

checkTraitAnnotations :: (MonadIO m) => Module Metadata Kind IndexedType -> CompilerT Metadata m ()
checkTraitAnnotations Module{..} = do
  names <- gets compilerNameStore
  let checkDefinition =
        \case
          DFunction _ name FunctionDefinition{..} -> do
            when (isExplicitReturnType functionDefinitionAnnotation || hasExplicitPatternAnnotations functionDefinitionPatterns || not (null functionDefinitionConstraints)) $ do
              case Environment.lookup name names of
                Nothing ->
                  pure ()
                Just s -> do
                  -- Collect explicitly mentioned type variables from USER-WRITTEN annotations only
                  let explicitReturnVars = maybe Set.empty collectTypeVariables functionDefinitionAnnotation
                  let explicitPatternVars = foldMap collectPatternTypeVariables functionDefinitionPatterns
                  let explicitTraitVars = collectConstraintTypeVariables functionDefinitionConstraints
                  let allExplicitParamIndices = explicitReturnVars <> explicitPatternVars <> explicitTraitVars

                  annType <- indexedReturnType functionDefinitionAnnotation
                  argTypes <- traverse indexedPatternTypes functionDefinitionPatterns
                  let annFunctionType = foldType annType (NonEmpty.filter (hasKind KType) argTypes)

                  annTraits <- indexedConstraints functionDefinitionConstraints
                  let annQualifiedType = With annTraits annFunctionType

                  inferredType <- instantiate s
                  res <- liftUnifier (unify annQualifiedType inferredType)

                  case res of
                    Left _ ->
                      -- Type mismatch - should be caught by type checker earlier
                      pure ()
                    Right sub -> do
                      let normalizedAnn = apply sub annQualifiedType
                      let normalizedInf = apply sub inferredType

                      -- Normalize the explicit parameter indices to their substituted indexed type indices
                      let normalizedExplicitVars = normalizeExplicitVariables sub allExplicitParamIndices

                      -- Filter inferred traits to only those referencing explicit variables
                      let With infTraits infType = normalizedInf
                      let relevantInfTraits = filter (traitReferencesVariables normalizedExplicitVars) infTraits
                      let filteredInf = With relevantInfTraits infType
                      checkTraitCoverage functionDefinitionMetadata name normalizedAnn filteredInf
          DLet _ name LetDefinition{..} -> do
            when (isExplicitReturnType letDefinitionAnnotation || not (null letDefinitionConstraints)) $ do
              case Environment.lookup name names of
                Nothing ->
                  pure ()
                Just s -> do
                  annType <- indexedReturnType letDefinitionAnnotation
                  annTraits <- indexedConstraints letDefinitionConstraints
                  let annQualifiedType = With annTraits annType

                  inferredType <- instantiate s
                  res <- liftUnifier (unify annQualifiedType inferredType)

                  case res of
                    Left _ ->
                      -- Type mismatch - should be caught by type checker earlier
                      pure ()
                    Right sub -> do
                      let normalizedAnn = apply sub annQualifiedType
                      let normalizedInf = apply sub inferredType
                      checkTraitCoverage letDefinitionMetadata name normalizedAnn normalizedInf
          DInstance _ InstanceDefinition{..} ->
            forM_ instanceDefinitionImplementations $
              \case
                DFunction loc name def ->
                  checkDefinition (DFunction loc (instanceLabel (Trait instanceDefinitionTraitName instanceDefinitionType) name) def)
                DLet loc name def ->
                  checkDefinition (DLet loc (instanceLabel (Trait instanceDefinitionTraitName instanceDefinitionType) name) def)
                _ ->
                  pure ()
          _ ->
            pure ()

  forM_ moduleDefinitions checkDefinition

instantiate :: (MonadIO m) => IndexedScheme -> CompilerT Metadata m (Qualified IndexedType)
instantiate (Forall qs ts t) = do
  sub <- foldrM go mempty qs
  return (With (apply sub ts) (apply sub t))
 where
  go (TypeIndex k index) sub = do
    s <- supplied id
    pure (index `mapsTo` TVariable (TypeIndex k s) <> sub)
