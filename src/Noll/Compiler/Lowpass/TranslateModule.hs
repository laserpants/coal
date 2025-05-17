{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Compiler.Lowpass.TranslateModule (translateModule) where

import Control.Monad.Reader (MonadReader)
import Data.Data (Data)
import Lang.Utils (Name, Set)
import Noll.Compiler.Lowpass.Environment (TranslateEnvironment, withModuleName)
import Noll.Compiler.Lowpass.TranslateDefinition (translateDefinition)
import Noll.Language
import Noll.Module
import Noll.Module.Definition

import qualified Data.Text as Text
import qualified Lang.Lowpass.Language as Lowpass

translateModule :: (MonadReader TranslateEnvironment m, Data a) => Module a Kind IndexedType -> m (Lowpass.Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type))
translateModule =
  \case
    Module (Path p) _ ds ->
      withModuleName name $
        Lowpass.Module name [] . concat <$> traverse translateDefinition ds
     where
      name = Text.intercalate "." p
