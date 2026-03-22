{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Coal.Compiler.Aliases (AliasTransform (..)) where

import Coal.Common.Environment (forMEnvironment)
import qualified Coal.Common.Environment as Environment
import Coal.Common.Supply (Supply (..), supplied)
import Coal.Compiler.Environment
import Coal.Compiler.Stack (CompilerT)
import Coal.Graphviz.Dot (Dot (..), generateDot, writeDotFile)
import Coal.Language
import Coal.Language.Module
import Coal.ProtoCompiler.ProtoBuild
import Coal.ProtoCompiler.ProtoBuild.ProtoNameEntry
import Coal.ProtoCompiler.ProtoStack
import Coal.ProtoCompiler.ProtoState
import Coal.ProtoLanguage.ProtoDefinition
import Coal.ProtoLanguage.ProtoModule (ProtoModule (..))
import Coal.ProtoTypeSystem.Parameterized
import Coal.TypeSystem.Substitution (applyT)
import qualified Coal.TypeSystem.Substitution as Substitution
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Reader (asks, runReaderT)
import Control.Monad.State (StateT, execStateT, get, gets, modify, put)
import Control.Monad.Trans (lift)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (transformM)
import Data.List.NonEmpty (NonEmpty (..), toList)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.Text.Lazy (toStrict)
import Extras (Dictionary, Name, forM_)
import Text.Pretty.Simple (pPrint, pShowNoColor)

class AliasTransform c where
  aliasTransform :: (MonadIO m, Show a) => c -> CompilerT a (ProtoCompilerT m a) c

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

instance (AliasTransform t) => AliasTransform (With t) where
  aliasTransform = traverse aliasTransform

instance (AliasTransform t) => AliasTransform (Row o k t) where
  aliasTransform = traverse aliasTransform

instance (Data e, Data a, Data t, AliasTransform t, AliasTransform (Type Parameter a)) => AliasTransform (ProtoModule e a t) where
  aliasTransform =
    \case
      ProtoModule{..} -> do
        m <- ProtoModule protoOmodulePath protoOmoduleExportList <$> aliasTransform protoOmoduleDefinitions
        updateNames

        tempprotoOupdateCurrentBuildC $
          \ProtoBuild{..} -> do
            newDataConstructors <- forMEnvironment protoObuildDataConstructors aliasTransform
            return
              ProtoBuild
                { protoObuildDataConstructors = newDataConstructors
                , ..
                }

        ProtoBuild{..} <- lift $ protoOgetCurrentBuildC
        liftIO $ Text.writeFile ("tmp/aliases_build_" <> Text.unpack (principalPath protoOmodulePath)) (toStrict $ pShowNoColor $ ProtoBuild{..})
        liftIO $ Text.writeFile ("tmp/aliases_names_" <> Text.unpack (principalPath protoOmodulePath)) (toStrict $ pShowNoColor $ protoObuildNames)

        pure m

-- fooz :: ProtoDataConstructorEntry a -> ProtoDataConstructorEntry a
-- fooz ProtoDataConstructorEntry{..} =
--  ProtoDataConstructorEntry{
--    protoOdataConstructorEntryConstructor = protoOdataConstructorEntryConstructor
--  , ..
--  }

updateNames :: (MonadIO m, Show a) => CompilerT a (ProtoCompilerT m a) ()
updateNames =
  tempprotoOupdateCurrentBuildC $
    \build@ProtoBuild{..} ->
      flip execStateT build $ do
        forM_ (concat $ Environment.elems protoObuildNames) $
          \case
            ProtoNName name s -> do
              newScheme <- lift $ aliasTransform s
              modify (replaceBuildNameEntry (ProtoNName name newScheme))
            _ ->
              pure ()

tempprotoOupdateBuildC :: (Monad m) => Path -> (ProtoBuild a -> CompilerT a (ProtoCompilerT m a) (ProtoBuild a)) -> CompilerT a (ProtoCompilerT m a) ()
tempprotoOupdateBuildC path f = do
  maybeBuild <- lift $ protoOgetBuildC path
  case maybeBuild of
    Nothing ->
      error "Implementation error"
    Just build -> do
      newBuild <- f build
      lift $ modify (overProtoCompilerModules (Environment.insert (principalPath path) newBuild))

tempprotoOupdateCurrentBuildC :: (Monad m) => (ProtoBuild a -> CompilerT a (ProtoCompilerT m a) (ProtoBuild a)) -> CompilerT a (ProtoCompilerT m a) ()
tempprotoOupdateCurrentBuildC f = do
  ProtoCompilerState{..} <- lift get
  tempprotoOupdateBuildC protoOcompilerCurrentPath f

instance (Data e, Data a, Data t, AliasTransform t, AliasTransform (Type Parameter a)) => AliasTransform (ProtoDefinition e a t) where
  aliasTransform =
    \case
      ProtoDFunction loc name def ->
        ProtoDFunction loc name <$> aliasTransform def
      ProtoDLet loc name def ->
        ProtoDLet loc name <$> aliasTransform def
      ProtoDInstance loc def ->
        ProtoDInstance loc <$> aliasTransform def
      ProtoDType loc name def ->
        ProtoDType loc name <$> aliasTransform def
      ProtoDTypeAlias loc name def ->
        ProtoDTypeAlias loc name <$> aliasTransform def
      o ->
        pure o

instance (Data e, Data a, Data t, AliasTransform t, AliasTransform (Type Parameter a)) => AliasTransform (ProtoFunctionDefinition e a t) where
  aliasTransform =
    \case
      ProtoFunctionDefinition{..} ->
        ProtoFunctionDefinition protoOfunctionDefinitionMetadata
          <$> aliasTransform protoOfunctionDefinitionAnnotation
          <*> aliasTransform protoOfunctionDefinitionType
          <*> aliasTransform protoOfunctionDefinitionPatterns
          <*> aliasTransform protoOfunctionDefinitionExpression

instance (Data e, Data a, Data t, AliasTransform t, AliasTransform (Type Parameter a)) => AliasTransform (ProtoLetDefinition e a t) where
  aliasTransform =
    \case
      ProtoLetDefinition{..} ->
        ProtoLetDefinition protoOletDefinitionMetadata
          <$> aliasTransform protoOletDefinitionAnnotation
          <*> aliasTransform protoOletDefinitionType
          <*> aliasTransform protoOletDefinitionExpression

instance (Data e, Data a, Data t, AliasTransform t, AliasTransform (Type Parameter a)) => AliasTransform (ProtoInstanceDefinition e a t) where
  aliasTransform =
    \case
      ProtoInstanceDefinition{..} -> do
        newprotoOInstanceDefinitionImplementations <- aliasTransform protoOinstanceDefinitionImplementations
        pure $
          ProtoInstanceDefinition
            { protoOinstanceDefinitionImplementations = newprotoOInstanceDefinitionImplementations
            , ..
            }

instance (AliasTransform (Type Parameter a)) => AliasTransform (ProtoTypeDefinition e a t) where
  aliasTransform =
    \case
      ProtoTypeDefinition{..} -> do
        newprotoOTypeDefinitionConstructors <- aliasTransform protoOtypeDefinitionConstructors
        pure $
          ProtoTypeDefinition
            { protoOtypeDefinitionConstructors = newprotoOTypeDefinitionConstructors
            , ..
            }

instance (AliasTransform (Type Parameter k)) => AliasTransform (ProtoAliasDefinition a k) where
  aliasTransform =
    \case
      ProtoAliasDefinition{..} -> do
        newprotoOAliasDefinitionType <- aliasTransform protoOaliasDefinitionType
        pure $
          ProtoAliasDefinition
            { protoOaliasDefinitionType = newprotoOAliasDefinitionType
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

instance (AliasTransform (Type Parameter a)) => AliasTransform (DataConstructor Parameter a (Type Parameter a)) where
  aliasTransform =
    \case
      DataConstructor{..} -> do
        newConstructorScheme <- aliasTransform constructorScheme
        pure DataConstructor{constructorScheme = newConstructorScheme, ..}

-- TODO: DRY
instance AliasTransform (DataConstructor TypeIndex Kind (Type TypeIndex Kind)) where
  aliasTransform =
    \case
      DataConstructor{..} -> do
        newConstructorScheme <- aliasTransform constructorScheme
        pure DataConstructor{constructorScheme = newConstructorScheme, ..}

instance AliasTransform (Type Parameter Kind) where
  aliasTransform =
    \case
      t@(TApplication k _ _) ->
        uncurry (aliasTransformTypeApplication k t) (listTypeArgs t)
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
        uncurry (aliasTransformTypeApplication2 k t) (listTypeArgs t)
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

instance AliasTransform (ProtoDataConstructorEntry a) where
  aliasTransform =
    \case
      ProtoDataConstructorEntry{..} -> do
        newDataConstructorEntryConstructor <- aliasTransform protoOdataConstructorEntryConstructor
        return
          ProtoDataConstructorEntry
            { protoOdataConstructorEntryConstructor = newDataConstructorEntryConstructor
            , ..
            }

--

-- instance (AliasTransform t, Data a, Data t) => AliasTransform (Pattern a () t) where
--  aliasTransform =
--    transformM $
--      \case
--        PAnnotation a t p ->
--          PAnnotation a <$> aliasTransform t <*> aliasTransform p
--        p ->
--          pure p
--
-- instance (AliasTransform t, Data t, Data a) => AliasTransform (Expression a () t) where
--  aliasTransform =
--    transformM $
--      \case
--        EAnnotation a t e ->
--          EAnnotation a <$> aliasTransform t <*> aliasTransform e
--        ELet a bs e ->
--          ELet a <$> aliasTransform bs <*> aliasTransform e
--        e ->
--          pure e
--
-- instance (AliasTransform t, Data t, Data a) => AliasTransform (Binding Expression a () t) where
--  aliasTransform =
--    \case
--      BPattern a p e ->
--        BPattern a <$> aliasTransform p <*> aliasTransform e
--      BFunction a n ps e ->
--        BFunction a n <$> aliasTransform ps <*> aliasTransform e
--
-- instance (AliasTransform t, Data e, Data t) => AliasTransform (Module e a t) where
--  aliasTransform =
--    \case
--      Module p ns o ->
--        Module p ns <$> aliasTransform o

instance (AliasTransform t, Data a, Data t) => AliasTransform (FunctionDefinition a t) where
  aliasTransform =
    \case
      FunctionDefinition a u w ps e ->
        undefined

--        FunctionDefinition a
--          <$> aliasTransform u
--          <*> aliasTransform w
--          <*> aliasTransform ps
--          <*> aliasTransform e

instance (AliasTransform t, Data a, Data t) => AliasTransform (ConstantDefinition a t) where
  aliasTransform =
    \case
      ConstantDefinition a u w e ->
        undefined

--        ConstantDefinition a
--          <$> aliasTransform u
--          <*> aliasTransform w
--          <*> aliasTransform e

-- instance (AliasTransform t) => AliasTransform (Scheme o k t) where
--  aliasTransform =
--    \case
--      Forall vs ts t ->
--        Forall vs ts <$> aliasTransform t

instance AliasTransform TypeDefinition where
  aliasTransform = pure

--    \case
--      TypeDefinition ps ctors ->
--        TypeDefinition ps <$> traverse aliasTransform ctors

-- instance (AliasTransform t) => AliasTransform (DataConstructor o k t) where
--  aliasTransform =
--    \case
--      DataConstructor{..} -> do
--        s <- aliasTransform constructorScheme
--        pure DataConstructor{constructorScheme = s, ..}

instance (AliasTransform t, Data a, Data t) => AliasTransform (Definition a Kind t) where
  aliasTransform =
    \case
      --      DFunction loc name f fs ->
      --        DFunction loc name <$> aliasTransform f <*> traverse aliasTransform fs
      --      DConstant loc name c fs ->
      --        DConstant loc name <$> aliasTransform c <*> traverse aliasTransform fs
      --      DInstance loc name (InstanceDefinition ts t ds) ->
      --        DInstance loc name . InstanceDefinition ts t <$> traverse aliasTransform ds
      --      DType loc name (TypeDefinition params ctors) ->
      --        DType loc name . TypeDefinition params <$> traverse aliasTransform ctors
      --      DTypeAlias loc name (AliasDefinition params t) ->
      --        DTypeAlias loc name . AliasDefinition params <$> aliasTransform t
      o ->
        pure o

aliasTransformTypeApplication :: (MonadIO m, Show a) => Kind -> Type Parameter Kind -> Type Parameter Kind -> NonEmpty (Type Parameter Kind) -> CompilerT a (ProtoCompilerT m a) (Type Parameter Kind)
aliasTransformTypeApplication _ t (TConstructor _ name) ts =
  lookupAlias t (toList ts) name
aliasTransformTypeApplication k _ t ts =
  applyTypeArgs k <$> aliasTransform t <*> aliasTransform ts

aliasTransformTypeApplication2 :: (MonadIO m, Show a) => Kind -> Type TypeIndex Kind -> Type TypeIndex Kind -> NonEmpty (Type TypeIndex Kind) -> CompilerT a (ProtoCompilerT m a) (Type TypeIndex Kind)
aliasTransformTypeApplication2 _ t (TConstructor _ name) ts =
  lookupAlias2 t (toList ts) name
aliasTransformTypeApplication2 k _ t ts =
  applyTypeArgs k <$> aliasTransform t <*> aliasTransform ts

-- instance AliasTransform ParameterizedType where
--  aliasTransform =
--    \case
--      t@TApplication{} ->
--        uncurry (aliasTransformTypeApplication t) (listTypeArgs t)
--      TArrow t1 t2 ->
--        TArrow <$> aliasTransform t1 <*> aliasTransform t2
--      TAlias name ts t ->
--        TAlias name <$> aliasTransform ts <*> aliasTransform t
--      TIntrinsic t ->
--        pure (TIntrinsic t)
--      TRecord t ->
--        TRecord <$> aliasTransform t
--      TRow row ->
--        TRow <$> traverse aliasTransform row
--      t@(TConstructor _ name) ->
--        lookupAlias t [] name
--      t ->
--        pure t

lookupAlias :: (MonadIO m, Show a) => Type Parameter Kind -> [Type Parameter Kind] -> Name -> CompilerT a (ProtoCompilerT m a) (Type Parameter Kind)
lookupAlias t ts name = do
  ProtoBuild{..} <- lift $ protoOgetCurrentBuildC
  -- env <- asks compilerAliasEnvironment
  case Environment.lookup name protoObuildAliases of
    Nothing ->
      case t of
        TApplication k t1 t2 ->
          TApplication k <$> aliasTransform t1 <*> aliasTransform t2
        _ ->
          pure t
    Just ProtoAliasEntry{..} -> do
      let t1 = foldr (uncurry substituteAlias) protoOaliasEntryType (protoOaliasEntryParams `zip` ts)
      TAlias name ts <$> aliasTransform t1

lookupAlias2 :: (MonadIO m, Show a) => Type TypeIndex Kind -> [Type TypeIndex Kind] -> Name -> CompilerT a (ProtoCompilerT m a) (Type TypeIndex Kind)
lookupAlias2 t ts name = do
  ProtoBuild{..} <- lift $ protoOgetCurrentBuildC
  -- env <- asks compilerAliasEnvironment
  case Environment.lookup name protoObuildAliases of
    Nothing ->
      case t of
        TApplication k t1 t2 ->
          TApplication k <$> aliasTransform t1 <*> aliasTransform t2
        _ ->
          pure t
    Just ProtoAliasEntry{..} -> do
      ixs <- traverse (\Parameter{..} -> supplied (TypeIndex parameterKind)) protoOaliasEntryParams
      let abc = (parameterName <$> protoOaliasEntryParams) `zip` ixs
          sub = Substitution.fromList ((typeIndexId <$> ixs) `zip` ts)
      t1 <- lift $ runReaderT (toIndexed protoOaliasEntryType) (Environment.fromList abc)
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
