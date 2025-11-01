{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.PreflightPhase.TypeDefinitions (passTypeDefinitions) where

import Coal.Compiler.Pass
import Coal.Compiler.Stack
import Coal.Language
import Coal.Language.Module
import Coal.Language.Module.Definition (isDType)
import Control.Monad.IO.Class (MonadIO)

passTypeDefinitions :: (MonadIO m) => Pass a m (Module a Kind ()) (Module a Kind ())
passTypeDefinitions =
  Pass
    { passName = "TypeDefinitions"
    , runPass = pass
    }

pass :: (Monad m) => Module a Kind () -> CompilerT a m (Module a Kind ())
pass = insertTypeDefinitions

insertTypeDefinitions :: (Monad m) => Module a Kind () -> CompilerT a m (Module a Kind ())
insertTypeDefinitions m@(Module path _ defs) = do
  insertTypeDefinitionsC (principalPath path) (filter isDType defs)
  pure m
