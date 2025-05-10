{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Compiler.Lowpass.TranslateModule (translateModule) where

import Data.Data (Data)
import Lang.Utils (Name)
import Noll.Compiler.Lowpass.TranslateDefinition (translateDefinition)
import Noll.Language
import Noll.Module
import Noll.Module.Definition

import qualified Data.Text as Text
import qualified Lang.Lowpass.Language as Lowpass

translateModule :: (Data a) => Module a Kind IndexedType -> Lowpass.Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type)
translateModule =
  \case
    Module (Path p) _ ds ->
      Lowpass.Module (Text.intercalate "." p) [] (concatMap translateDefinition ds)
