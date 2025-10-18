{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.PreflightPhase.TypeDefinitions where

import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Pass
import Coal.Compiler.Stack
import Coal.Language
import Coal.Language.Module
import Coal.Language.Module.Definition (isDType)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (gets)
import qualified Data.Text as Text
import Extra (forM, fromMaybe, isConstructor)

passTypeDefinitions :: (MonadIO m) => Pass a m (Module a Kind ()) (Module a Kind ())
passTypeDefinitions =
  Pass
    { passName = "TypeDefinitions"
    , runPass = pass
    }

pass :: (Monad m) => Module a Kind () -> CompilerT a m (Module a Kind ())
pass = insertTypeDefinitions

insertTypeDefinitions :: (Monad m) => Module a Kind () -> CompilerT a m (Module a Kind ())
insertTypeDefinitions m@(Module (Path path) _ defs) = do
  insertTypeDefinitionsC (Text.intercalate "." path) (filter isDType defs)
  pure m
