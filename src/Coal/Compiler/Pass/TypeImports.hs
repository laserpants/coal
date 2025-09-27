{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.TypeImports where

import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Pass
import Coal.Compiler.Stack
import Coal.Language
import Coal.Language.Module
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State (gets)
import Extra (forM, fromMaybe, isConstructor)

passTypeImports :: (MonadIO m) => Pass a m [Module a Kind ()] [Module a Kind ()]
passTypeImports =
  Pass
    { passName = "TypeImports"
    , runPass = pass
    }

pass :: (Monad m) => [Module a Kind ()] -> CompilerT a m [Module a Kind ()]
pass = traverse (overModuleDefinitionsM insertTypes)

insertTypes :: (Monad m) => [Definition a Kind ()] -> CompilerT a m [Definition a Kind ()]
insertTypes defs = do
  defss <-
    forM defs $
      \case
        DImport _ path ns -> do
          env <- gets compilerTypeDefinitions
          let ds = fromMaybe mempty (Environment.lookup (principalPath path) env)
          pure [t | t@(DType _ ctor _) <- ds, ctor `elem` constructors]
         where
          constructors = filter isConstructor ns
        _ ->
          pure []
  pure (concat defss <> defs)
