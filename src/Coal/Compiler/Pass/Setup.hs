{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.Setup (passSetup) where

import Coal.Ast.Metadata (Metadata (..))
import Coal.Compiler.Pass
import Coal.Compiler.Stack (CompilerT, insertNamesC)
import Coal.Language
import Coal.Language.Module
import Extra (Name)

passSetup :: (Monad m) => Pass a m [Module Metadata Kind ()] [Module Metadata Kind ()]
passSetup =
  Pass
    { passName = "Setup"
    , runPass = pass
    }

pass :: (Monad m) => [Module Metadata Kind ()] -> CompilerT a m [Module Metadata Kind ()]
pass modules = do
  insertNamesC names
  pure (overModuleDefinitions insertBuiltInDefinitions <$> modules)

insertBuiltInDefinitions :: [Definition a k ()] -> [Definition a k ()]
insertBuiltInDefinitions = (builtInDefinitions <>)

builtInDefinitions :: [Definition a k ()]
builtInDefinitions = []

-- TODO
names :: [(Name, IndexedScheme)]
names =
  []
