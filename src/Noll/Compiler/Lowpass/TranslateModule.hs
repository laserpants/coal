{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Compiler.Lowpass.TranslateModule (translateModule) where

import Control.Monad.Reader (MonadReader)
import Data.Data (Data)
import Lang.Common.Environment
import Lang.Utils (Name)
import Noll.Compiler.Lowpass.Environment (TranslateEnvironment, insertQualifiedNames, withModuleName)
import Noll.Compiler.Lowpass.TranslateDefinition (translateDefinition)
import Noll.Language
import Noll.Module

import qualified Data.Text as Text
import qualified Lang.Common.Environment as Environment
import qualified Lang.Lowpass.Language as Lowpass

translateModule :: (MonadReader TranslateEnvironment m, Data a) => Module a Kind IndexedType -> m (Lowpass.Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type))
translateModule =
  \case
    Module (Path p) _ defs ->
      insertQualifiedNames env $
        withModuleName name $
          Lowpass.Module
            name
            (Environment.elems env)
            . concat
            <$> traverse translateDefinition defs
     where
      name = Text.intercalate "." p
      env = collectImports defs

collectImports :: [Definition a k t] -> Environment Name
collectImports = Environment.fromList . concatMap imports

imports :: Definition a k t -> [(Name, Name)]
imports =
  \case
    DImport (Path p) names ->
      flip map names $
        \name ->
          (name, Text.intercalate "." p <> "." <> name)
    _ ->
      []
