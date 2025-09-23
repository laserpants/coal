{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Kernel.TranslateModule (translateModule) where

import Coal.Common.Environment
import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Kernel.Environment (KernelEnvironment, insertQualifiedNames, withModuleName)
import Coal.Compiler.Kernel.TranslateDefinition (translateDefinition)
import Coal.Compiler.Stack
import qualified Coal.Kernel.Language as Kernel
import Coal.Language
import Coal.Language.Module
import Control.Monad.Reader (MonadReader)
import Data.Data (Data)
import qualified Data.Text as Text
import Extra (Name)

translateModule :: (Data a, Monad m) => Module a Kind IndexedType -> CompilerT a m (Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type))
translateModule =
  \case
    Module path _ defs ->
      insertQualifiedNames env $
        withModuleName name $
          Kernel.Module
            name
            (Environment.elems env)
            . concat
            <$> traverse translateDefinition defs
     where
      name = principalPath path
      env = collectImports defs

collectImports :: [Definition a k t] -> Environment Name
collectImports = Environment.fromList . concatMap imports

imports :: Definition a k t -> [(Name, Name)]
imports =
  \case
    DImport _ path ns ->
      flip map ns $
        \name ->
          (name, principalPath path <> "." <> name)
    _ ->
      []
