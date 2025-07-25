{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Compiler.Kernel.TranslateModule (translateModule) where

import Control.Monad.Reader (MonadReader)
import Data.Data (Data)
import Lang.Common.Environment
import Extra (Name)
import Noll.Compiler.Kernel.Environment (KernelEnvironment, insertQualifiedNames, withModuleName)
import Noll.Compiler.Kernel.TranslateDefinition (translateDefinition)
import Noll.Language
import Noll.Language.Module

import qualified Data.Text as Text
import qualified Lang.Common.Environment as Environment
import qualified Lang.Lowpass.Language as Lowpass

translateModule :: (Show a, MonadReader KernelEnvironment m, Data a) => Module a Kind IndexedType -> m (Lowpass.Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type))
translateModule =
  \case
    Module (Path p) _ defs ->
      insertQualifiedNames env $
        withModuleName name $
          Lowpass.Module
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
