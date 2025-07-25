{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Kernel.TranslateModule (translateModule) where

import Control.Monad.Reader (MonadReader)
import Data.Data (Data)
import Extra (Name)
import Coal.Common.Environment
import Coal.Compiler.Kernel.Environment (KernelEnvironment, insertQualifiedNames, withModuleName)
import Coal.Compiler.Kernel.TranslateDefinition (translateDefinition)
import Coal.Language
import Coal.Language.Module

import qualified Data.Text as Text
import qualified Coal.Common.Environment as Environment
import qualified Coal.Kernel.Language as Kernel

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
coreImports =
  [ "Core$.operator__not"
  , "Core$.not"
  , "Core$.operator__reverse_composition"
  , "Core$.operator__reverse_application"
  , "Core$.always"
  , "Core$.operator__list_concatenation"
  , "Core$.trace_int32"
  , "Core$.trace_string"
  , "Core$.operator__string_concatenation"
  , "Core$.int32_to_string"
  , "Core$.pair_to_string"
  , "Core$.list_to_string"
  , "Core$.trace"
  , "Core$.unpack_nat"
  , "Core$.pack_nat"
  ]

imports :: Definition a k t -> [(Name, Name)]
imports =
  \case
    DImport (Path p) names ->
      flip map names $
        \name ->
          (name, Text.intercalate "." p <> "." <> name)
    _ ->
      []
