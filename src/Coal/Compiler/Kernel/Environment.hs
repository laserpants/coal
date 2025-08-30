{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Kernel.Environment (
  KernelEnvironment (..),
  initialKernelEnvironment,
  qualifyName,
  withLocalName,
  withLocalNames,
  withModuleName,
  insertQualifiedNames,
) where

import Coal.Common.Environment (Environment)
import Control.Monad.Reader (MonadReader, ask, local)
import Data.Text (isPrefixOf)
import Extra (Name, Over, Set)

import qualified Coal.Common.Environment as Environment
import qualified Data.Set as Set
import qualified Data.Text as Text

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
  if isFinal name kernelEnvironmentLocalNames
    then pure name
    else case Environment.lookup name kernelEnvironmentQualifiedNames of
      Just qname ->
        pure qname
      Nothing ->
        pure (kernelEnvironmentModule <> "." <> name)

isFinal :: Name -> Set Name -> Bool
isFinal name localNames
  | name == "_" = True
  | Text.head name == '$' = True
  | "Core$" `isPrefixOf` name = True
  | name `Set.member` localNames = True
  | otherwise = False

withLocalName :: (MonadReader KernelEnvironment m) => Name -> m a -> m a
withLocalName = local . overKernelEnvironmentLocalNames . Set.insert

withLocalNames :: (Foldable f, MonadReader KernelEnvironment m) => f Name -> m a -> m a
withLocalNames = flip (foldr withLocalName)

withModuleName :: (MonadReader KernelEnvironment m) => Name -> m a -> m a
withModuleName = local . overKernelEnvironmentModule . const

insertQualifiedNames :: (MonadReader KernelEnvironment m) => Environment Name -> m a -> m a
insertQualifiedNames names = local (overKernelEnvironmentQualifiedNames (names <>))
