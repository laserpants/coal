{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

{- |
Module: Coal.Compiler.Pass.PhaseTranslation.Placeholders
Description: Trait dictionary insertion and constraint elaboration

This module implements dictionary-passing style for type classes (traits) in Coal.
It transforms expressions with trait constraints into explicit dictionary parameters
and applications, enabling runtime polymorphism through dictionary passing.

Key transformations:

1. **Dictionary insertion**: Functions with trait constraints are transformed to
   accept explicit dictionary parameters (records containing trait methods).

2. **Instance resolution**: At call sites, the compiler looks up appropriate trait
   instances and inserts dictionary arguments automatically.

3. **Dictionary lambda creation**: Functions requiring dictionaries are wrapped in
   lambdas that accept dictionary parameters.

4. **Recursive let handling**: Handles recursive let bindings to properly thread
   trait dictionaries through recursive references.

Example transformation:
@
fun show(x: a) with Show\<a\> = ...
@
becomes:
@
fun show(implementation_Show : Show\<a\>, x : a) = ...
@

Call sites automatically insert the appropriate implementation:
@
show(42)  // becomes: show(Show\<int32\>, 42)
@

The pass runs twice to handle all trait dependencies correctly.
-}
module Coal.Compiler.Pass.PhaseTranslation.Placeholders (
  TraitContext (..),
  passPlaceholders,
) where

import Coal.Common.Environment (Environment)
import qualified Coal.Common.Environment as Environment
import Coal.Common.Label (Label (..))
import Coal.Common.Supply (supplied)
import Coal.Compiler.Build
import Coal.Compiler.Build.NameEntry
import Coal.Compiler.Journal (censorDictionaryTraits, listenDictionaryTraits, tellDictionaryTraits, tellErrors)
import Coal.Compiler.Metadata (Metadata (..))
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Compiler.State
import Coal.Language
import Coal.Language.Module.Path (Path (Path), principalPath)
import Coal.TypeSystem.Substitution (Substitutable (apply), Substitution, mapsTo)
import Coal.TypeSystem.Unification
import Control.Monad.Except (MonadError (throwError), forM)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (execStateT, get, gets, modify, put)
import Data.Data (Data)
import Data.Foldable (foldrM)
import Data.Generics.Uniplate.Data (descendM)
import Data.List.NonEmpty (NonEmpty (..), toList)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (isPrefixOf)
import Extras (Dictionary, Name, forM_, traverse_, twice)

passPlaceholders :: (MonadIO m) => Pass Metadata m (Module Metadata Kind IndexedType) (Module Metadata Kind IndexedType)
passPlaceholders = Pass{runPass = pass}

pass :: (MonadIO m) => Module Metadata Kind IndexedType -> CompilerT Metadata m (Module Metadata Kind IndexedType)
pass Module{..} = do
  setCurrentPathC modulePath
  build <- getCurrentBuildC
  setNamesC (extractTypeEnvironment build)
  twice (traverse_ collectDefinitionTraits moduleDefinitions)
  names <- gets compilerNameStore
  updateNames names
  newDefinitions <- traverse insertTraitDictionaries moduleDefinitions
  return
    Module
      { moduleDefinitions = newDefinitions
      , ..
      }

updateNames :: (Monad m) => Environment IndexedScheme -> CompilerT a m ()
updateNames store =
  updateCurrentBuildC $
    \build@Build{buildNames} ->
      flip execStateT build $
        forM_ (concat $ Environment.elems buildNames) $
          \case
            NName name _ ->
              case Environment.lookup name store of
                Nothing ->
                  pure ()
                Just s ->
                  modify (replaceBuildNameEntry (NName name s))
            _ ->
              pure ()

insertTraitDictionaries :: (Show a, Monad m, Monoid a, Data a) => Definition a Kind IndexedType -> CompilerT a m (Definition a Kind IndexedType)
insertTraitDictionaries =
  \case
    def@DLet{} ->
      expandTraits def
    DInstance loc InstanceDefinition{..} -> do
      newInstanceDefinitionImplementations <- forM instanceDefinitionImplementations insertTraitDictionariesInDef
      return $
        DInstance
          loc
          InstanceDefinition
            { instanceDefinitionImplementations = newInstanceDefinitionImplementations
            , ..
            }
    d ->
      pure d

insertTraitDictionariesInDef :: (Show a, Monad m, Monoid a, Data a) => Definition a Kind IndexedType -> CompilerT a m (Definition a Kind IndexedType)
insertTraitDictionariesInDef =
  \case
    def@DLet{} -> do
      expandTraits def
    d ->
      pure d

-- | Insert a name and its scheme into the compiler's name store from a let definition
insertName :: (Monad m) => Definition a k IndexedType -> Name -> CompilerT a m ()
insertName (DLet _ _ (LetDefinition _ _ (With ts t) _)) name = do
  let s = Forall (typeIndexesIn t) (Set.fromList ts) t
  insertNameC name s
insertName _ _ = pure () -- Other definitions do not need name insertion

{- | Collect all trait constraints required by a name at the given type.
Unifies the name's type scheme with the given type and returns required traits.
-}
collectTraits :: (Monad m) => IndexedType -> Name -> CompilerT a m (Set (Trait IndexedType))
collectTraits u name = do
  env <- gets compilerNameStore
  case Environment.lookup name env of
    Nothing ->
      pure mempty
    Just (Forall _ s _)
      | Set.null s ->
          pure mempty
    Just (Forall vs ts t) -> do
      sub1 <- foldrM instantiate mempty vs
      r <- tryMatch (apply sub1 t) u
      case r of
        Left{} -> do
          -- Type mismatch during trait collection - this shouldn't happen in well-typed code
          -- Return empty set and let type checker catch the error
          pure mempty
        Right sub2 ->
          pure (apply (sub2 <> sub1) ts)
 where
  instantiate (TypeIndex k index) acc = do
    var <- supplied (TVariable . TypeIndex k)
    pure (index `mapsTo` var <> acc)

-- | Try to unify two types, returning either an error or a substitution
tryMatch :: (Monad m) => IndexedType -> IndexedType -> CompilerT a m (Either UnificationError Substitution)
tryMatch t u = do
  var <- supplied id
  pure (evalUnifier var (match t u))

{- | Find the first matching trait instance for a trait constraint.
Returns the instance type, indexed type, and member type schemes if found.
-}
findFirstMatch :: (Monad m) => Trait IndexedType -> CompilerT a m (Maybe (Type Parameter Kind, IndexedType, Dictionary IndexedScheme))
findFirstMatch (Trait name t) = do
  Build{buildInstances} <- getCurrentBuildC
  case Environment.lookup name buildInstances of
    Nothing ->
      pure Nothing
    Just env1 -> do
      kvs <- go (`tryMatch` t) env1
      case kvs of
        [] ->
          pure Nothing
        (t1, k, v) : _ ->
          pure (Just (t1, k, v))
 where
  go f m = fmap catMaybes . forM (Map.toList m) $
    \(k, InstanceEntry{instanceEntryType, instanceEntryTypeSchemes}) -> do
      result <- f k
      case result of
        Left{} ->
          pure Nothing
        Right sub ->
          pure $ Just (instanceEntryType, k, Map.map (substituteInScheme sub) instanceEntryTypeSchemes)

substituteInScheme :: Substitution -> Scheme o Kind IndexedType -> IndexedScheme
substituteInScheme sub (Forall _ ts t) = scheme (apply sub ts) (apply sub t)

{- | Look up a trait instance and return its dictionary (record of method implementations).
Reports an error if the trait is concrete but no instance is found.
-}
lookupTraitInstance :: (Show a, Monoid a, Data a, Data k, Show k, Monad m) => a -> Trait IndexedType -> CompilerT a m (Maybe (Dictionary (Expression a k IndexedType)))
lookupTraitInstance loc trait@(Trait name _) = do
  found <- findFirstMatch trait
  case found of
    Nothing -> do
      if isConcrete trait
        then do
          path <- gets compilerCurrentPath
          tellErrors [MissingInstance trait (ErrorLocation (principalPath path) loc)]
          throwError TraitError
        else pure Nothing
    Just (t, a, b) ->
      Just <$> Map.traverseWithKey (go t (Trait name a)) b
 where
  go t1 (Trait tn _) n (Forall _ ts t) =
    applyTraits loc (Label t (instanceLabel (Trait tn t1) n)) ts
      >>= expandTraits

-- | Check if a trait's type is concrete (not a type variable)
isConcrete :: Trait IndexedType -> Bool
isConcrete (Trait _ TIntrinsic{}) = True
isConcrete (Trait _ TRecord{}) = True
isConcrete _ = False

-- | Apply trait dictionaries to a variable reference, wrapping in application if needed
applyTraits :: (Show a, Monoid a, Data a, Data k, Show k, Monad m) => a -> Label IndexedType -> Set (Trait IndexedType) -> CompilerT a m (Expression a k IndexedType)
applyTraits loc (Label t name) traits =
  if Set.null traits
    then pure (EVariable mempty (Label t name))
    else EApplication mempty t (EVariable mempty (Label t1 name)) <$> traverse insert_ (NonEmpty.fromList (Set.toList traits))
 where
  t1 = foldTypeOf t (Set.toList traits) -- (tr : trs)
  insert_ trait = do
    fields <- lookupTraitInstance loc trait
    case fields of
      Nothing | not (isVariable trait) -> do
        path <- gets compilerCurrentPath
        tellErrors [MissingInstance trait (ErrorLocation (principalPath path) loc)]
        throwError TraitError
      Nothing -> do
        tellDictionaryTraits (Set.singleton trait)
        pure (ETraitInstance mempty (typeOf trait) trait)
      Just r ->
        pure (ERecord mempty (typeOf trait) r Nothing)

{- | Types that can have trait dictionaries inserted during elaboration.
This typeclass enables trait constraint expansion for expressions, clauses, and definitions.
-}
class TraitContext a d where
  -- | Expand trait constraints into explicit dictionary parameters and applications
  expandTraits :: (Monad m) => d -> CompilerT a m d

-- | Transform a recursive let into the appropriate form for trait handling
expandRecursiveLet :: Expression a k IndexedType -> Expression a k IndexedType
expandRecursiveLet (ELet a (BPattern _ p e1 :| []) e2) = ERecursiveLet a p e1 e2
expandRecursiveLet _ = error "expandRecursiveLet: expected ELet with single BPattern, got unexpected form"

withLocalEnvironment :: (Monad m) => [(Name, IndexedScheme)] -> CompilerT a m r -> CompilerT a m r
withLocalEnvironment xs action = do
  old <- get
  insertNamesC xs
  r <- action
  put old
  return r

instance (Monoid a, Data a, Data k, Show a, Show k) => TraitContext a (Expression a k IndexedType) where
  expandTraits =
    \case
      ERecursiveLet a p e1 e2 ->
        expandRecursiveLet <$> expandTraits (ELet a (BPattern a p e1 :| []) e2)
      ELet a bs e -> do
        as <- censorDictionaryTraits (const mempty) (traverse transformBindingWithTraits bs)
        let xs = concat (toList (snd <$> as))
        withLocalEnvironment xs $
          ELet a (fst <$> as) <$> expandTraits e
      var@(EVariable _ (Label t name))
        | "$fold" `isPrefixOf` name -> do
            traits <- collectTraits t name
            tellDictionaryTraits traits
            pure var
      EVariable loc (Label t name) -> do
        traits <- collectTraits t name
        applyTraits loc (Label t name) traits
      ECompiledMatch a t e cs ->
        ECompiledMatch a t <$> expandTraits e <*> traverse expandTraits cs
      -- Transform a binding to collect trait dependencies and wrap the body in dictionary lambdas
      e ->
        descendM expandTraits e

-- | Transform a binding to collect trait dependencies and wrap the body in dictionary lambdas
transformBindingWithTraits :: (Monoid a, Data a, Data k, Show a, Show k, Monad m) => Binding Expression a k IndexedType -> CompilerT a m (Binding Expression a k IndexedType, [(Name, IndexedScheme)])
transformBindingWithTraits =
  \case
    BPattern a var@(PVariable _ (Label t name)) e
      | "$fold" `isPrefixOf` name -> do
          (body, traits) <- listenDictionaryTraits (expandTraits e)
          pure (BPattern a var body, [(name, Forall (typeIndexesIn t) traits t)])
    BPattern _ (PVariable a (Label t name)) e -> do
      (e1, traits) <- transformScopeWithTraits e
      let ll = Label (foldTypeOf t (Set.toList traits)) name
      pure (BPattern mempty (PVariable a ll) e1, [(name, Forall (typeIndexesIn t) traits t)])
    BPattern a (PAnnotation _ _ p) e ->
      transformBindingWithTraits (BPattern a p e)
    binding ->
      error $ "transformBindingWithTraits: unsupported binding pattern: " ++ show binding

-- | Transform an expression scope to collect trait constraints and create dictionary lambdas
transformScopeWithTraits :: (Monoid a, Data a, Data k, Monad m, Show a, Show k) => Expression a k IndexedType -> CompilerT a m (Expression a k IndexedType, Set (Trait IndexedType))
transformScopeWithTraits e = do
  (expr, traits) <- listenDictionaryTraits (expandTraits e)
  case Set.toList traits of
    [] -> pure (expr, traits)
    tr : trs -> pure (dictionaryLambda tr trs expr, traits)

instance (Monoid a, Data a, Data k, Show a, Show k) => TraitContext a (CompiledClause a k IndexedType) where
  expandTraits =
    \case
      ECompiledClause a lls e ->
        ECompiledClause a lls <$> expandTraits e

instance (Monoid a, Data a, Data k, Show a, Show k) => TraitContext a (Definition a k IndexedType) where
  expandTraits =
    \case
      DLet loc name letDefinition -> do
        newLetDefinition <- expandLetDefinitionTraits name letDefinition
        return $ DLet loc name newLetDefinition
      DInstance loc InstanceDefinition{..} -> do
        newInstanceDefinitionImplementations <- traverse expandTraits instanceDefinitionImplementations
        return $
          DInstance
            loc
            InstanceDefinition
              { instanceDefinitionImplementations = newInstanceDefinitionImplementations
              , ..
              }
      d ->
        return d

expandLetDefinitionTraits :: (Monad m, Monoid a, Data a, Data k, Show a, Show k) => Name -> LetDefinition a k IndexedType -> CompilerT a m (LetDefinition a k IndexedType)
expandLetDefinitionTraits name =
  \case
    LetDefinition loc with (With _ t) e -> do
      (expr, traits) <- listenDictionaryTraits (expandTraits e)
      case Set.toList traits of
        [] ->
          pure $ LetDefinition loc with (With [] t) expr
        tr : trs -> do
          path <- gets compilerCurrentPath
          -- Insert default int32 instance for Numeric and Ordered traits for main function
          if "main" == name && Path ["Main"] == path
            then do
              recs <- forM (tr :| trs) $
                \(Trait trait _) -> do
                  fields <- fromJust <$> lookupTraitInstance loc (Trait trait (TIntrinsic IInt32))
                  pure $
                    ERecord
                      mempty
                      (applyTypeArgs KTrait (TConstructor (KArrow KType KTrait) trait) (TIntrinsic IInt32 :| []))
                      fields
                      Nothing
              pure $
                LetDefinition
                  loc
                  with
                  (With (tr : trs) t)
                  ( EApplication
                      mempty
                      t
                      (dictionaryLambda tr trs expr)
                      recs
                  )
            else -- Check if a trait constraint is on a type variable (not yet resolved)

              pure $
                LetDefinition loc with (With (tr : trs) t) (dictionaryLambda tr trs expr)

-- | Create a lambda that accepts trait dictionaries as parameters

-- | Check if a trait constraint is on a type variable (not yet resolved)
isVariable :: Trait IndexedType -> Bool
isVariable (Trait _ TVariable{}) = True
isVariable _ = False

{- | Expand trait constraints in a let definition without modifying the expression body.
Used during the first pass to collect trait information.
-}

-- | Create a lambda that accepts trait dictionaries as parameters
dictionaryLambda :: (Monoid a, HasType o k (Trait (Type o k))) => Trait (Type o k) -> [Trait (Type o k)] -> Expression a i (Type o k) -> Expression a i (Type o k)
dictionaryLambda tr trs = ELambda mempty (dict <$> (tr :| trs))
 where
  dict t = PTraitInstance mempty (typeOf t) t

{- | Expand trait constraints in a let definition without modifying the expression body.
Used during the first pass to collect trait information.
-}
passiveExpandLetDefinitionTraits :: (Monad m, Monoid a, Data a, Show a) => LetDefinition a Kind IndexedType -> CompilerT a m (LetDefinition a Kind IndexedType)
passiveExpandLetDefinitionTraits =
  -- \| Collect trait information from an expression without transformation (passive pass)
  \case
    LetDefinition loc with (With _ t) e -> do
      (_, traits) <- listenDictionaryTraits (passiveExpandTraitsInExpr e)
      case Set.toList traits of
        [] ->
          pure $ LetDefinition loc with (With [] t) e
        tr : trs -> do
          pure $ LetDefinition loc with (With (tr : trs) t) e

-- | Collect trait information from an expression without transformation (passive pass)
passiveExpandTraitsInExpr :: (Monad m, Monoid a, Data a, Data k, Show a, Show k) => Expression a k IndexedType -> CompilerT a m (Expression a k IndexedType)
passiveExpandTraitsInExpr =
  \case
    ERecursiveLet a p e1 e2 ->
      expandRecursiveLet <$> passiveExpandTraitsInExpr (ELet a (BPattern a p e1 :| []) e2)
    ELet a bs e -> do
      as <- censorDictionaryTraits (const mempty) (traverse transformBindingWithTraits bs)
      let xs = concat (toList (snd <$> as))
      withLocalEnvironment xs $
        ELet a (fst <$> as) <$> passiveExpandTraitsInExpr e
    var@(EVariable _ (Label t name))
      | "$fold" `isPrefixOf` name -> do
          traits <- collectTraits t name
          tellDictionaryTraits traits
          pure var
    EVariable loc (Label t name) -> do
      traits <- collectTraits t name
      passiveApplyTraits loc traits
      pure (EVariable loc (Label t name))
    ECompiledMatch a t e cs ->
      ECompiledMatch a t <$> passiveExpandTraitsInExpr e <*> traverse passiveExpandTraitsInClause cs
    e ->
      descendM passiveExpandTraitsInExpr e

passiveExpandTraitsInClause :: (Monad m, Monoid a, Data a, Data k, Show a, Show k) => CompiledClause a k IndexedType -> CompilerT a m (CompiledClause a k IndexedType)
passiveExpandTraitsInClause =
  \case
    ECompiledClause a lls e ->
      ECompiledClause a lls <$> passiveExpandTraitsInExpr e

passiveApplyTraits :: (Show a, Monoid a, Data a, Monad m) => a -> Set (Trait IndexedType) -> CompilerT a m ()
passiveApplyTraits loc traits = do
  case Set.toList traits of
    [] ->
      pure ()
    x : xs ->
      -- Collect trait constraints from a definition and register its name (first pass)
      traverse_ insert_ (x :| xs)
 where
  insert_ trait = do
    (fields :: Maybe (Dictionary (Expression a Kind IndexedType))) <- lookupTraitInstance loc trait
    case fields of
      Nothing | isVariable trait -> do
        tellDictionaryTraits (Set.singleton trait)
      _ ->
        pure ()

-- | Collect trait constraints from a definition and register its name (first pass)
collectDefinitionTraits :: (Show a, Monad m, Monoid a, Data a) => Definition a Kind IndexedType -> CompilerT a m ()
collectDefinitionTraits =
  \case
    DLet a name def -> do
      d <- passiveExpandLetDefinitionTraits def
      insertName (DLet a name d) name
    DInstance _ InstanceDefinition{..} ->
      forM_ instanceDefinitionImplementations $
        \case
          DLet loc name def -> do
            d <- passiveExpandLetDefinitionTraits def
            insertName (DLet loc name d) instanceName
           where
            instanceName = instanceLabel (Trait instanceDefinitionTraitName instanceDefinitionType) name
          _ ->
            pure ()
    _ ->
      pure ()
