{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Kernel.Environment (
  KernelEnvironment (..),
  qualifyName,
  withLocalName,
  withLocalNames,
  withModuleName,
  insertQualifiedNames,
) where

import Coal.Common.Environment (Environment)
import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Environment
import Coal.Compiler.Stack
import Control.Monad.Reader (asks, local)
import qualified Data.Set as Set
import Data.Text (isPrefixOf)
import qualified Data.Text as Text
import Extras (Name, Set, (<.>))

qualifyName :: (Monad m) => Name -> CompilerT a m Name
qualifyName name = do
  KernelEnvironment{..} <- asks compilerKernelEnvironment
  if isFinal name kernelEnvironmentLocalNames
    then pure name
    else case Environment.lookup name kernelEnvironmentQualifiedNames of
      Just qname ->
        pure qname
      Nothing ->
        pure (kernelEnvironmentModule <.> name)

isFinal :: Name -> Set Name -> Bool
isFinal name localNames
  | name == "_" = True
  | Text.head name == '$' = True
  | isBuiltin name = True
  | name `Set.member` localNames = True
  | otherwise = False

{-# INLINE isBuiltin #-}
isBuiltin :: Name -> Bool
isBuiltin = ("Builtin$" `isPrefixOf`)

withLocalName :: (Monad m) => Name -> CompilerT a m e -> CompilerT a m e
withLocalName = local . overCompilerKernelEnvironment . overKernelEnvironmentLocalNames . Set.insert

withLocalNames :: (Foldable f, Monad m) => f Name -> CompilerT a m e -> CompilerT a m e
withLocalNames = flip (foldr withLocalName)

withModuleName :: (Monad m) => Name -> CompilerT a m e -> CompilerT a m e
withModuleName = local . overCompilerKernelEnvironment . overKernelEnvironmentModule . const

insertQualifiedNames :: (Monad m) => Environment Name -> CompilerT a m e -> CompilerT a m e
insertQualifiedNames names = local (overCompilerKernelEnvironment $ overKernelEnvironmentQualifiedNames (names <>))
