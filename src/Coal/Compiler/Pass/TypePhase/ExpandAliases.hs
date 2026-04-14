{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Coal.Compiler.Pass.TypePhase.ExpandAliases (
  passExpandAliases
  ) where

import Coal.Compiler.Pass (Pass (..))
import Coal.Language.Type
import Coal.Common.Environment (forMEnvironment)
import Coal.Common.Supply (supplied)
import Coal.Compiler.Build
import Coal.Compiler.Build.NameEntry
import Coal.Compiler.Stack
import Coal.Language
import Coal.Language.Definition
import Coal.Language.Module (Module (..))
import Coal.Language.Module.Path
import Coal.TypeSystem.Parameterized
import Coal.TypeSystem.Substitution (applyT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (runReaderT)
import Control.Monad.State (execStateT, modify)
import Control.Monad.Trans (lift)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (transformM)
import Data.List.NonEmpty (NonEmpty (..), toList)
import Data.Text.Lazy (toStrict)
import Extras (Dictionary, Name, forM_)
import Text.Pretty.Simple (pShowNoColor)
import qualified Coal.Common.Environment as Environment
import qualified Coal.TypeSystem.Substitution as Substitution
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

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

        updateCurrentBuildC $
          \Build{..} -> do
            newDataConstructors <- forMEnvironment buildDataConstructors aliasTransform
            return
              Build
                { buildDataConstructors = newDataConstructors
                , ..
                }

        Build{..} <- getCurrentBuildC
        liftIO $ Text.writeFile ("tmp/aliases_build_" <> Text.unpack (principalPath modulePath)) (toStrict $ pShowNoColor $ Build{..})
        liftIO $ Text.writeFile ("tmp/aliases_names_" <> Text.unpack (principalPath modulePath)) (toStrict $ pShowNoColor $ buildNames)

        pure m

updateNames :: (MonadIO m, Show a) => CompilerT a m ()
updateNames =
  updateCurrentBuildC $
    \build@Build{..} ->
      flip execStateT build $ do
        forM_ (concat $ Environment.elems buildNames) $
          \case
            NName name s -> do
              newScheme <- lift $ aliasTransform s
              modify (replaceBuildNameEntry (NName name newScheme))
            _ ->
              pure ()

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
        pure o

instance (Data e, Data a, Data t, AliasTransform t, AliasTransform (Type Parameter a)) => AliasTransform (FunctionDefinition e a t) where
  aliasTransform =
    \case
      FunctionDefinition{..} ->
        FunctionDefinition functionDefinitionMetadata
          <$> aliasTransform functionDefinitionAnnotation
          <*> aliasTransform functionDefinitionType
          <*> aliasTransform functionDefinitionPatterns
          <*> aliasTransform functionDefinitionExpression

instance (Data e, Data a, Data t, AliasTransform t, AliasTransform (Type Parameter a)) => AliasTransform (LetDefinition e a t) where
  aliasTransform =
    \case
      LetDefinition{..} ->
        LetDefinition letDefinitionMetadata
          <$> aliasTransform letDefinitionAnnotation
          <*> aliasTransform letDefinitionType
          <*> aliasTransform letDefinitionExpression

instance (Data e, Data a, Data t, AliasTransform t, AliasTransform (Type Parameter a)) => AliasTransform (InstanceDefinition e a t) where
  aliasTransform =
    \case
      InstanceDefinition{..} -> do
        newInstanceDefinitionImplementations <- aliasTransform instanceDefinitionImplementations
        pure $
          InstanceDefinition
            { instanceDefinitionImplementations = newInstanceDefinitionImplementations
            , ..
            }

instance (AliasTransform (Type Parameter a)) => AliasTransform (TypeDefinition e a t) where
  aliasTransform =
    \case
      TypeDefinition{..} -> do
        newTypeDefinitionConstructors <- aliasTransform typeDefinitionConstructors
        pure $
          TypeDefinition
            { typeDefinitionConstructors = newTypeDefinitionConstructors
            , ..
            }

instance (AliasTransform (Type Parameter k)) => AliasTransform (AliasDefinition a k) where
  aliasTransform =
    \case
      AliasDefinition{..} -> do
        newAliasDefinitionType <- aliasTransform aliasDefinitionType
        pure $
          AliasDefinition
            { aliasDefinitionType = newAliasDefinitionType
            , ..
            }

instance (Data e, Data a, Data t, AliasTransform (Type Parameter a)) => AliasTransform (Expression e a t) where
  aliasTransform =
    transformM $
      \case
        EAnnotation a t e ->
          EAnnotation a <$> aliasTransform t <*> aliasTransform e
        ELet a bs e ->
          ELet a <$> aliasTransform bs <*> aliasTransform e
        e ->
          pure e

instance (Data e, Data a, Data t, AliasTransform (Type Parameter a)) => AliasTransform (Pattern e a t) where
  aliasTransform =
    transformM $
      \case
        PAnnotation a t p ->
          PAnnotation a <$> aliasTransform t <*> aliasTransform p
        p ->
          pure p

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
        pure
          DataConstructor
            { constructorScheme = newConstructorScheme
            , ..
            }

instance AliasTransform (Type Parameter Kind) where
  aliasTransform =
    \case
      t@(TApplication k _ _) ->
        uncurry (aliasTransformTypeApplication k t) (typeArgs t)
      TArrow t1 t2 ->
        TArrow <$> aliasTransform t1 <*> aliasTransform t2
      TAlias name ts t ->
        TAlias name <$> aliasTransform ts <*> aliasTransform t
      TIntrinsic t ->
        pure (TIntrinsic t)
      TRecord t ->
        TRecord <$> aliasTransform t
      TRow row ->
        TRow <$> traverse aliasTransform row
      t@(TConstructor _ name) ->
        lookupAlias t [] name
      t ->
        pure t

instance AliasTransform (Type TypeIndex Kind) where
  aliasTransform =
    \case
      t@(TApplication k _ _) ->
        uncurry (aliasTransformTypeApplication2 k t) (typeArgs t)
      TArrow t1 t2 ->
        TArrow <$> aliasTransform t1 <*> aliasTransform t2
      TAlias name ts t ->
        TAlias name <$> aliasTransform ts <*> aliasTransform t
      TIntrinsic t ->
        pure (TIntrinsic t)
      TRecord t ->
        TRecord <$> aliasTransform t
      TRow row ->
        TRow <$> traverse aliasTransform row
      t@(TConstructor _ name) ->
        lookupAlias2 t [] name
      t ->
        pure t

instance (AliasTransform t) => AliasTransform (Scheme o k t) where
  aliasTransform =
    \case
      Forall{..} ->
        Forall schemeTypeVariables schemeTraits
          <$> aliasTransform schemeTypeBody

instance AliasTransform () where
  aliasTransform _ = pure ()

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

aliasTransformTypeApplication :: (MonadIO m, Show a) => Kind -> Type Parameter Kind -> Type Parameter Kind -> NonEmpty (Type Parameter Kind) -> CompilerT a m (Type Parameter Kind)
aliasTransformTypeApplication _ t (TConstructor _ name) ts =
  lookupAlias t (toList ts) name
aliasTransformTypeApplication k _ t ts =
  applyTypeArgs k <$> aliasTransform t <*> aliasTransform ts

aliasTransformTypeApplication2 :: (MonadIO m, Show a) => Kind -> Type TypeIndex Kind -> Type TypeIndex Kind -> NonEmpty (Type TypeIndex Kind) -> CompilerT a m (Type TypeIndex Kind)
aliasTransformTypeApplication2 _ t (TConstructor _ name) ts =
  lookupAlias2 t (toList ts) name
aliasTransformTypeApplication2 k _ t ts =
  applyTypeArgs k <$> aliasTransform t <*> aliasTransform ts

lookupAlias :: (MonadIO m, Show a) => Type Parameter Kind -> [Type Parameter Kind] -> Name -> CompilerT a m (Type Parameter Kind)
lookupAlias t ts name = do
  Build{..} <- getCurrentBuildC
  case Environment.lookup name buildAliases of
    Nothing ->
      case t of
        TApplication k t1 t2 ->
          TApplication k <$> aliasTransform t1 <*> aliasTransform t2
        _ ->
          pure t
    Just AliasEntry{..} -> do
      let t1 = foldr (uncurry substituteAlias) aliasEntryType (aliasEntryParams `zip` ts)
      TAlias name ts <$> aliasTransform t1

lookupAlias2 :: (MonadIO m, Show a) => Type TypeIndex Kind -> [Type TypeIndex Kind] -> Name -> CompilerT a m (Type TypeIndex Kind)
lookupAlias2 t ts name = do
  Build{..} <- getCurrentBuildC
  case Environment.lookup name buildAliases of
    Nothing ->
      case t of
        TApplication k t1 t2 ->
          TApplication k <$> aliasTransform t1 <*> aliasTransform t2
        _ ->
          pure t
    Just AliasEntry{..} -> do
      ixs <- traverse (\Parameter{..} -> supplied (TypeIndex parameterKind)) aliasEntryParams
      let abc = (parameterName <$> aliasEntryParams) `zip` ixs
          sub = Substitution.fromList ((typeIndexId <$> ixs) `zip` ts)
      t1 <- runReaderT (toIndexed aliasEntryType) (Environment.fromList abc)
      TAlias name ts <$> aliasTransform (applyT sub t1)

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
