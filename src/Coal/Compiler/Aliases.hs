{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE UndecidableInstances #-}

module Coal.Compiler.Aliases (AliasTransform (..)) where

import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Build
import Coal.Compiler.Environment
import Coal.Compiler.Stack (CompilerT)
import Coal.Language
import Coal.Language.Module
import Coal.ProtoCompiler.ProtoBuild.ProtoNameEntry
import Coal.ProtoCompiler.ProtoStack
import Coal.ProtoLanguage.ProtoDefinition
import Coal.ProtoLanguage.ProtoModule (ProtoModule (..))
import Control.Monad.Reader (asks)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (transformM)
import Data.List.NonEmpty (NonEmpty (..), toList)
import Debug.Trace
import Extras (Dictionary, Name)

class AliasTransform c where
  aliasTransform :: (Monad m) => c -> CompilerT a (ProtoCompilerT m a) c

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
      ProtoModule{..} ->
        ProtoModule protoOmodulePath protoOmoduleExportList
          <$> aliasTransform protoOmoduleDefinitions

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

instance (AliasTransform t) => AliasTransform (Scheme o k t) where
  aliasTransform =
    \case
      Forall{..} ->
        Forall schemeTypeVariables schemeTraits
          <$> aliasTransform schemeTypeBody

--

instance AliasTransform () where
  aliasTransform _ = pure ()

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

aliasTransformTypeApplication :: (Monad m) => Kind -> Type Parameter Kind -> Type Parameter Kind -> NonEmpty (Type Parameter Kind) -> CompilerT a (ProtoCompilerT m a) (Type Parameter Kind)
aliasTransformTypeApplication _ t (TConstructor _ name) ts =
  lookupAlias t (toList ts) name
aliasTransformTypeApplication k _ t ts =
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

lookupAlias :: (Monad m, AliasTransform (Type Parameter Kind)) => Type Parameter Kind -> [Type Parameter Kind] -> Name -> CompilerT a (ProtoCompilerT m a) (Type Parameter Kind)
lookupAlias t ts name = do
  env <- asks compilerAliasEnvironment
  case Environment.lookup name env of
    Nothing ->
      case t of
        TApplication k t1 t2 ->
          TApplication k <$> aliasTransform t1 <*> aliasTransform t2
        _ ->
          pure t
    Just ProtoAliasEntry{..} -> do
      let t1 = foldr (uncurry substituteAlias) protoOaliasEntryType (protoOaliasEntryParams `zip` ts)
      TAlias name ts <$> aliasTransform t1

--  traceShowM name
--
--  case Environment.lookup name env of
--    Nothing ->
--      case t of
--        TApplication k t1 t2 ->
--          TApplication k <$> aliasTransform t1 <*> aliasTransform t2
--        _ ->
--          pure t
--    Just AliasEntry{..} -> do
--      let t1 = foldr (uncurry substituteAlias) aliasEntryType (aliasEntryParams `zip` ts)
--      TAlias name ts <$> aliasTransform t1

substituteAlias :: Name -> Type Parameter k -> Type Parameter k -> Type Parameter k
substituteAlias name s =
  \case
    t@(TVariable (Parameter _ match))
      | name == match ->
          s
      | otherwise ->
          t
    TApplication k t1 t2 ->
      TApplication k (substituteAlias name s t1) (substituteAlias name s t2)
    TArrow t1 t2 ->
      TArrow (substituteAlias name s t1) (substituteAlias name s t2)
    TRow row ->
      TRow (substituteAlias name s <$> row)
    TRecord t ->
      TRecord (substituteAlias name s t)
    t ->
      t
