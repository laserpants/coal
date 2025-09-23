{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.Setup (setupPass) where

import Coal.Ast.Metadata (Metadata (..))
import Coal.Compiler.Pass
import Coal.Compiler.Stack (CompilerT)
import Coal.Language
import Coal.Language.Module

setupPass :: (Monad m) => Pass a m [Module Metadata Kind ()] [Module Metadata Kind ()]
setupPass =
  Pass
    { passName = "Setup"
    , runPass = pass
    }

pass :: (Monad m) => [Module Metadata Kind ()] -> CompilerT a m [Module Metadata Kind ()]
pass modules = pure (overModuleDefinitions insertBuiltInDefinitions <$> modules)

insertBuiltInDefinitions :: [Definition a k ()] -> [Definition a k ()]
insertBuiltInDefinitions = (builtInDefinitions <>)

builtInDefinitions :: [Definition a k ()]
builtInDefinitions = []
