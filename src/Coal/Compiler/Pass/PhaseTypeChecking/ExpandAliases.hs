{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

{- |
Module: Coal.Compiler.Pass.PhaseTypeChecking.ExpandAliases

Expand type aliases to their underlying definitions throughout the AST.

This pass performs inline expansion of type aliases, replacing all occurrences
of aliased types with their full definitions. The expansion is recursive,
handling nested aliases and ensuring that all type aliases are fully resolved
before type inference begins.

For example, a type alias like:

@
type alias Dictionary<a> = Map<string, a>
@

is expanded so that all uses of @Dictionary<a>@ in the code are replaced with
@Map<string, a>@. This simplification ensures that the type checker only needs
to work with concrete types rather than dealing with alias resolution during
type inference.

The pass also updates the build environment to reflect the expanded types in
constructors and name entries.
-}
module Coal.Compiler.Pass.PhaseTypeChecking.ExpandAliases (
  passExpandAliases,
) where

import Coal.Common.Environment (forMEnvironment)
import qualified Coal.Common.Environment as Environment
import Coal.Common.Supply (supplied)
import Coal.Compiler.Build
import Coal.Compiler.Build.NameEntry
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (CompilerT, getCurrentBuildC, updateCurrentBuildC)
import Coal.Language
import Coal.TypeSystem.Parameterized (ToIndexed (toIndexed))
import Coal.TypeSystem.Substitution (applyT)
import qualified Coal.TypeSystem.Substitution as Substitution
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (execStateT, modify)
import Control.Monad.Trans (lift)
import Data.Data (Data, Typeable)
import Data.Generics.Uniplate.Data (descendM)
import Data.List.NonEmpty (NonEmpty (..), toList)
import Extras (Dictionary, Name, forM_)

{- | Type alias expansion pass.

Replace all type alias references with their underlying definitions throughout
the module AST. Perform recursive expansion to handle nested aliases and update
the build environment with expanded types. This simplifies subsequent type
inference by eliminating alias indirection.
-}
passExpandAliases :: (MonadIO m, Data a, Show a, Data k, AliasTransform (Type Parameter k)) => Pass a m (Module a k ()) (Module a k ())
passExpandAliases = Pass{runPass = aliasTransform}

class AliasTransform c where
  aliasTransform :: (MonadIO m, Show a) => c -> CompilerT a m c

instance (AliasTransform c) => AliasTransform [c] where
  aliasTransform = traverse aliasTransform

instance (AliasTransform c) => AliasTransform (Maybe c) where
  aliasTransform = traverse aliasTransform

instance (AliasTransform c) => AliasTransform (Dictionary c) where
  aliasTransform = traverse aliasTransform

instance (AliasTransform c) => AliasTransform (NonEmpty c) where
  aliasTransform = traverse aliasTransform

instance (AliasTransform t) => AliasTransform (Trait t) where
  aliasTransform = traverse aliasTransform

instance (AliasTransform t) => AliasTransform (Qualified t) where
  aliasTransform = traverse aliasTransform

instance (AliasTransform t) => AliasTransform (Row o k t) where
  aliasTransform = traverse aliasTransform

instance (Data e, Data a, Data t, AliasTransform t, AliasTransform (Type Parameter a)) => AliasTransform (Module e a t) where
  aliasTransform =
    \case
      Module{..} -> do
        m <- Module modulePath moduleExportList <$> aliasTransform moduleDefinitions
        updateNames
        updateConstructors
        return m

updateConstructors :: (MonadIO m, Show a) => CompilerT a m ()
updateConstructors =
  updateCurrentBuildC $
    \Build{..} -> do
      newDataConstructors <- forMEnvironment buildDataConstructors aliasTransform
      return
        Build
          { buildDataConstructors = newDataConstructors
          , ..
          }

updateNames :: (MonadIO m, Show a) => CompilerT a m ()
updateNames =
  updateCurrentBuildC $
    \build@Build{buildNames} ->
      flip execStateT build $ do
        forM_ (concat $ Environment.elems buildNames) $
          \case
            NName name s -> do
              newScheme <- lift $ aliasTransform s
              modify (replaceBuildNameEntry (NName name newScheme))
            _ ->
              return ()

instance (Data e, Data a, Data t, AliasTransform t, AliasTransform (Type Parameter a)) => AliasTransform (Definition e a t) where
  aliasTransform =
    \case
      DFunction loc name def ->
        DFunction loc name <$> aliasTransform def
      DLet loc name def ->
        DLet loc name <$> aliasTransform def
      DInstance loc def ->
        DInstance loc <$> aliasTransform def
      DType loc name def ->
        DType loc name <$> aliasTransform def
      DTypeAlias loc name def ->
        DTypeAlias loc name <$> aliasTransform def
      o ->
        return o

instance (Data e, Data a, Data t, AliasTransform t, AliasTransform (Type Parameter a)) => AliasTransform (FunctionDefinition e a t) where
  aliasTransform =
    \case
      FunctionDefinition{..} ->
        FunctionDefinition functionDefinitionMetadata
          <$> aliasTransform functionDefinitionAnnotation
          <*> aliasTransform functionDefinitionConstraints
          <*> aliasTransform functionDefinitionType
          <*> aliasTransform functionDefinitionPatterns
          <*> aliasTransform functionDefinitionExpression

instance (Data e, Data a, Data t, AliasTransform t, AliasTransform (Type Parameter a)) => AliasTransform (LetDefinition e a t) where
  aliasTransform =
    \case
      LetDefinition{..} ->
        LetDefinition letDefinitionMetadata
          <$> aliasTransform letDefinitionAnnotation
          <*> aliasTransform letDefinitionConstraints
          <*> aliasTransform letDefinitionType
          <*> aliasTransform letDefinitionExpression

instance (Data e, Data a, Data t, AliasTransform t, AliasTransform (Type Parameter a)) => AliasTransform (InstanceDefinition e a t) where
  aliasTransform =
    \case
      InstanceDefinition{..} -> do
        newInstanceDefinitionImplementations <- aliasTransform instanceDefinitionImplementations
        return $
          InstanceDefinition
            { instanceDefinitionImplementations = newInstanceDefinitionImplementations
            , ..
            }

instance (AliasTransform (Type Parameter a)) => AliasTransform (TypeDefinition e a t) where
  aliasTransform =
    \case
      TypeDefinition{..} -> do
        newTypeDefinitionConstructors <- aliasTransform typeDefinitionConstructors
        return $
          TypeDefinition
            { typeDefinitionConstructors = newTypeDefinitionConstructors
            , ..
            }

instance (AliasTransform (Type Parameter k)) => AliasTransform (AliasDefinition a k) where
  aliasTransform =
    \case
      AliasDefinition{..} -> do
        newAliasDefinitionType <- aliasTransform aliasDefinitionType
        return $
          AliasDefinition
            { aliasDefinitionType = newAliasDefinitionType
            , ..
            }

instance (Data e, Data a, Data t, AliasTransform (Type Parameter a)) => AliasTransform (Expression e a t) where
  aliasTransform =
    \case
      EAnnotation a t e ->
        EAnnotation a <$> aliasTransform t <*> aliasTransform e
      ELet a bs e ->
        ELet a <$> aliasTransform bs <*> aliasTransform e
      ELambda a ps e ->
        ELambda a <$> aliasTransform ps <*> aliasTransform e
      e ->
        descendM aliasTransform e

instance (Data e, Data a, Data t, AliasTransform (Type Parameter a)) => AliasTransform (Pattern e a t) where
  aliasTransform =
    \case
      PAnnotation a t p ->
        PAnnotation a <$> aliasTransform t <*> aliasTransform p
      p ->
        descendM aliasTransform p

instance (Data e, Data a, Data t, AliasTransform (Type Parameter a)) => AliasTransform (Binding Expression e a t) where
  aliasTransform =
    \case
      BPattern a p e ->
        BPattern a <$> aliasTransform p <*> aliasTransform e
      BFunction a n ps e ->
        BFunction a n <$> aliasTransform ps <*> aliasTransform e

instance (AliasTransform (Type o k)) => AliasTransform (DataConstructor o k (Type o k)) where
  aliasTransform =
    \case
      DataConstructor{..} -> do
        newConstructorScheme <- aliasTransform constructorScheme
        return
          DataConstructor
            { constructorScheme = newConstructorScheme
            , ..
            }

instance (Typeable o, AliasEntryTransform o, Data (o Kind)) => AliasTransform (Type o Kind) where
  aliasTransform =
    \case
      t@(TApplication k _ _) ->
        uncurry (aliasTransformTypeApplication k t) (typeArgs t)
      TArrow t1 t2 ->
        TArrow <$> aliasTransform t1 <*> aliasTransform t2
      TAlias name ts t ->
        TAlias name <$> aliasTransform ts <*> aliasTransform t
      TIntrinsic t ->
        return (TIntrinsic t)
      TRecord t ->
        TRecord <$> aliasTransform t
      TRow row ->
        TRow <$> traverse aliasTransform row
      t@(TConstructor _ name) ->
        lookupAlias t [] name
      t ->
        return t

instance (AliasTransform t) => AliasTransform (Scheme o k t) where
  aliasTransform =
    \case
      Forall{..} ->
        Forall schemeTypeVariables schemeTraits
          <$> aliasTransform schemeTypeBody

instance AliasTransform () where
  aliasTransform _ = return ()

instance AliasTransform (DataConstructorEntry a) where
  aliasTransform =
    \case
      DataConstructorEntry{..} -> do
        newDataConstructorEntryConstructor <- aliasTransform dataConstructorEntryConstructor
        return
          DataConstructorEntry
            { dataConstructorEntryConstructor = newDataConstructorEntryConstructor
            , ..
            }

aliasTransformTypeApplication :: (MonadIO m, AliasEntryTransform o, Typeable o, Data (o Kind), Show a) => Kind -> Type o Kind -> Type o Kind -> NonEmpty (Type o Kind) -> CompilerT a m (Type o Kind)
aliasTransformTypeApplication _ t (TConstructor _ name) ts =
  lookupAlias t (toList ts) name
aliasTransformTypeApplication k _ t ts =
  applyTypeArgs k <$> aliasTransform t <*> aliasTransform ts

lookupAlias :: (MonadIO m, AliasEntryTransform o, Typeable o, Data (o Kind), Show a) => Type o Kind -> [Type o Kind] -> Name -> CompilerT a m (Type o Kind)
lookupAlias t ts name = do
  Build{buildAliases} <- getCurrentBuildC
  case Environment.lookup name buildAliases of
    Nothing ->
      case t of
        TApplication k t1 t2 ->
          TApplication k <$> aliasTransform t1 <*> aliasTransform t2
        _ ->
          return t
    Just aliasEntry -> do
      t1 <- transformAliasEntry ts aliasEntry
      TAlias name ts <$> aliasTransform t1

class AliasEntryTransform o where
  transformAliasEntry :: (MonadIO m, Show a) => [Type o Kind] -> AliasEntry a -> CompilerT a m (Type o Kind)

instance AliasEntryTransform TypeIndex where
  transformAliasEntry ts AliasEntry{..} = do
    ixs <- traverse (\Parameter{parameterKind} -> supplied (TypeIndex parameterKind)) aliasEntryParams
    let env = (parameterName <$> aliasEntryParams) `zip` ixs
        sub = Substitution.fromList ((typeIndexId <$> ixs) `zip` ts)
    t <- runReaderT (toIndexed aliasEntryType) (Environment.fromList env)
    return (applyT sub t)

instance AliasEntryTransform Parameter where
  transformAliasEntry ts AliasEntry{..} = aliasTransform t
   where
    t = foldr (uncurry substituteAlias) aliasEntryType (aliasEntryParams `zip` ts)

substituteAlias :: Parameter k -> Type Parameter k -> Type Parameter k -> Type Parameter k
substituteAlias param s =
  \case
    t@(TVariable (Parameter _ match))
      | parameterName param == match ->
          s
      | otherwise ->
          t
    TApplication k t1 t2 ->
      TApplication k (substituteAlias param s t1) (substituteAlias param s t2)
    TArrow t1 t2 ->
      TArrow (substituteAlias param s t1) (substituteAlias param s t2)
    TRow row ->
      TRow (substituteAlias param s <$> row)
    TRecord t ->
      TRecord (substituteAlias param s t)
    t ->
      t
