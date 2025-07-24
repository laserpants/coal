{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Noll.Compiler.Kernel.Environment (
  KernelEnvironment (..),
  initialKernelEnvironment,
  qualifyName,
  withLocalName,
  withLocalNames,
  withModuleName,
  insertQualifiedNames,
) where

import Control.Monad.Reader (MonadReader, ask, local)
import Lang.Common.Environment (Environment)
import Lang.Utils (Name, Over, Set)

import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Lang.Common.Environment as Environment

data KernelEnvironment = KernelEnvironment
  { kernelEnvironmentModule :: Name
  , kernelEnvironmentLocalNames :: Set Name
  , kernelEnvironmentQualifiedNames :: Environment Name
  }
  deriving (Show, Eq, Ord)

initialKernelEnvironment :: Environment Name -> KernelEnvironment
initialKernelEnvironment = KernelEnvironment mempty mempty

overKernelEnvironmentModule :: Over KernelEnvironment Name
overKernelEnvironmentModule fn KernelEnvironment{..} =
  KernelEnvironment{kernelEnvironmentModule = fn kernelEnvironmentModule, ..}

overKernelEnvironmentLocalNames :: Over KernelEnvironment (Set Name)
overKernelEnvironmentLocalNames fn KernelEnvironment{..} =
  KernelEnvironment{kernelEnvironmentLocalNames = fn kernelEnvironmentLocalNames, ..}

overKernelEnvironmentQualifiedNames :: Over KernelEnvironment (Environment Name)
overKernelEnvironmentQualifiedNames fn KernelEnvironment{..} =
  KernelEnvironment{kernelEnvironmentQualifiedNames = fn kernelEnvironmentQualifiedNames, ..}

qualifyName :: (MonadReader KernelEnvironment m) => Name -> m Name
qualifyName name = do
  KernelEnvironment{..} <- ask
  if name == "_" || Text.head name == '$' || Text.isPrefixOf "Core$" name || Set.member name kernelEnvironmentLocalNames
    then pure name
    else case Environment.lookup name kernelEnvironmentQualifiedNames of
      Just qname ->
        pure qname
      Nothing ->
        pure (kernelEnvironmentModule <> "." <> name)

withLocalName :: (MonadReader KernelEnvironment m) => Name -> m a -> m a
withLocalName = local . overKernelEnvironmentLocalNames . Set.insert

withLocalNames :: (Foldable f, MonadReader KernelEnvironment m) => f Name -> m a -> m a
withLocalNames = flip (foldr withLocalName)

withModuleName :: (MonadReader KernelEnvironment m) => Name -> m a -> m a
withModuleName = local . overKernelEnvironmentModule . const

insertQualifiedNames :: (MonadReader KernelEnvironment m) => Environment Name -> m a -> m a
insertQualifiedNames names = local (overKernelEnvironmentQualifiedNames (names <>))
