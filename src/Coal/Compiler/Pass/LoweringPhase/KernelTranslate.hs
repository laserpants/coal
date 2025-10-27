{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Pass.LoweringPhase.KernelTranslate (passKernelTranslate) where

import Coal.Ast.Metadata (Metadata (..))
import Coal.Common.Environment
import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Kernel.Environment (insertQualifiedNames, withModuleName)
import Coal.Compiler.Kernel.TranslateDefinition (translateDefinition)
import Coal.Compiler.Pass
import Coal.Compiler.Stack
import qualified Coal.Kernel.Language as Kernel
import Coal.Language (IndexedType, Kind (..))
import Coal.Language.Module
import Control.Monad.IO.Class
import Extras (Name, for, (<.>))

passKernelTranslate :: (MonadIO m) => Pass Metadata m (Module Metadata Kind IndexedType) (Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type))
passKernelTranslate =
  Pass
    { passName = "KernelTranslate"
    , runPass = pass
    }

pass :: (MonadIO m) => Module Metadata Kind IndexedType -> CompilerT Metadata m (Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type))
pass =
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
      for ns $
        \name ->
          (name, principalPath path <.> name)
    _ ->
      []
