{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.TypePhase.ExpandCotypes (passExpandCotypes) where

import Coal.AST.Type.Parameterized
import qualified Coal.Common.Environment as Environment
import Coal.Common.Supply (supplied)
import Coal.Compiler.Build
import Coal.Compiler.Build.Core (typeConstructorEnv)
import Coal.Compiler.Journal (tellErrors)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Language
import Coal.Language.Module
import Coal.TypeSystem.Substitution (apply)
import qualified Coal.TypeSystem.Substitution as Substitution
import Control.Monad.Except (throwError)
import Control.Monad.State (evalStateT, gets, replicateM)
import Data.List.NonEmpty (NonEmpty (..), toList)
import qualified Data.Map.Strict as Map
import Extras (Dictionary, Name, forM)

passExpandCotypes :: (Monad m, Show a) => Pass a m (Module a Kind IndexedType) (Module a Kind IndexedType)
passExpandCotypes = Pass{runPass = pass}

pass :: (Monad m, Show a) => Module a Kind IndexedType -> CompilerT a m (Module a Kind IndexedType)
pass m@(Module p _ _) = do
  setCompilerCurrentModuleC p
  expandCotypes m

class CotypeContext c where
  expandCotypes :: (Monad m, Show a) => c -> CompilerT a m c

instance CotypeContext () where
  expandCotypes _ = pure ()

instance (CotypeContext c) => CotypeContext [c] where
  expandCotypes = traverse expandCotypes

instance (CotypeContext c) => CotypeContext (Maybe c) where
  expandCotypes = traverse expandCotypes

instance (CotypeContext c) => CotypeContext (Dictionary c) where
  expandCotypes = traverse expandCotypes

instance (CotypeContext c) => CotypeContext (NonEmpty c) where
  expandCotypes = traverse expandCotypes

instance (CotypeContext t) => CotypeContext (Trait t) where
  expandCotypes = traverse expandCotypes

instance (CotypeContext t) => CotypeContext (With t) where
  expandCotypes = traverse expandCotypes

instance (CotypeContext t) => CotypeContext (Row o k t) where
  expandCotypes = traverse expandCotypes

instance (CotypeContext t) => CotypeContext (Pattern a t) where
  expandCotypes = traverse expandCotypes

instance (CotypeContext t) => CotypeContext (Expression a t) where
  expandCotypes = traverse expandCotypes

instance (CotypeContext t) => CotypeContext (Module e a t) where
  expandCotypes = traverse expandCotypes

instance (CotypeContext t) => CotypeContext (FunctionDefinition a t) where
  expandCotypes = traverse expandCotypes

instance (CotypeContext t) => CotypeContext (ConstantDefinition a t) where
  expandCotypes = traverse expandCotypes

instance (CotypeContext t) => CotypeContext (Definition a k t) where
  expandCotypes = traverse expandCotypes

expandCotypesTypeApplication :: (Monad m, Show a) => IndexedType -> IndexedType -> NonEmpty IndexedType -> CompilerT a m IndexedType
expandCotypesTypeApplication t (TConstructor _ name) ts =
  lookupCotype t (toList ts) name
expandCotypesTypeApplication t t1 ts =
  applyTypeArgs (kindOf t) <$> expandCotypes t1 <*> expandCotypes ts

instance CotypeContext IndexedType where
  expandCotypes =
    \case
      t@TApplication{} ->
        uncurry (expandCotypesTypeApplication t) (listTypeArgs t)
      TArrow t1 t2 ->
        TArrow <$> expandCotypes t1 <*> expandCotypes t2
      TAlias name ts t ->
        TAlias name <$> expandCotypes ts <*> expandCotypes t
      TIntrinsic t ->
        pure (TIntrinsic t)
      TRecord t ->
        TRecord <$> expandCotypes t
      TRow row ->
        TRow <$> traverse expandCotypes row
      t@TVariable{} ->
        pure t
      t@(TConstructor _ name) ->
        lookupCotype t [] name

lookupCotype :: (Monad m) => IndexedType -> [IndexedType] -> Name -> CompilerT a m IndexedType
lookupCotype t ts name = do
  ModuleBuild{..} <- getCurrentBuildC
  kinds <- evalStateT typeConstructorEnv ModuleBuild{..}
  case Environment.lookup name moduleCotypeConstructors of
    Nothing ->
      pure t
    Just CotypeConstructorEntry{..} -> do
      let ps = parameterName <$> cotypeConstructorEntryParams
      txs <- replicateM (length ps) (supplied (TypeIndex KType))
      fields <- forM cotypeConstructorEntryDataAccessors $
        \CodataAccessor{accessorScheme = Forall _ _ o, ..} -> do
          r <- instantiateVars (zip ps txs) kinds o
          case r of
            Left err -> do
              path <- gets compilerCurrentModule
              tellErrors [KindError err (ErrorLocation (principalPath path) cotypeConstructorEntryMetadata)]
              throwError PreflightFailure
            Right t1 -> do
              let sub = Substitution.fromList (zip (typeIndexId <$> txs) ts)
              case apply sub t1 of
                TArrow _ t2 ->
                  pure ("$_" <> accessorName, TIntrinsic IUnit `TArrow` t2)
                _ ->
                  error "Implementation error"
      pure (fieldsRecordType (Map.fromList fields) RNil)
