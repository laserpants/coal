{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- FIXME
module Coal.Compiler.Kernel.TranslateModule (translateModule) where

import Coal.Common.Environment
import Coal.Compiler.Kernel.Environment (KernelEnvironment, insertQualifiedNames, withModuleName)
import Coal.Compiler.Kernel.TranslateDefinition (translateDefinition)
import Coal.Language
import Coal.Language.Module
import Control.Monad.Reader (MonadReader)
import Data.Data (Data)
import Extra (Name)

import qualified Coal.Common.Environment as Environment
import qualified Coal.Kernel.Language as Kernel
import qualified Data.Text as Text

translateModule :: (Show a, MonadReader KernelEnvironment m, Data a) => Module a Kind IndexedType -> m (Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type))
translateModule =
  \case
    Module (Path p) _ defs ->
      insertQualifiedNames env $
        withModuleName name $
          Kernel.Module
            name
            (Environment.elems env <> coreImports)
            . concat
            <$> traverse translateDefinition defs
     where
      name = Text.intercalate "." p
      env = collectImports defs

collectImports :: [Definition a k t] -> Environment Name
collectImports = Environment.fromList . concatMap imports

coreImports :: [Name]
coreImports = []

imports :: Definition a k t -> [(Name, Name)]
imports =
  \case
    DImport _ (Path p) names ->
      flip map names $
        \name ->
          (name, Text.intercalate "." p <> "." <> name)
    _ ->
      []
