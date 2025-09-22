{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.Setup (setupPass) where

import Coal.Compiler.Pass
import Coal.Compiler.Stack (CompilerT)
import Coal.Language.Module
import Extra ((<$$>))

setupPass :: (Monad m) => Pass a m [ModuleBundle] [ModuleBundle]
setupPass =
  Pass
    { passName = "Setup"
    , runPass = pass
    }

pass :: (Monad m) => [ModuleBundle] -> CompilerT a m [ModuleBundle]
pass modules = pure (overModuleDefinitions insertBuiltInDefinitions <$$> modules)

insertBuiltInDefinitions :: [Definition a k ()] -> [Definition a k ()]
insertBuiltInDefinitions = (builtInDefinitions <>)

builtInDefinitions :: [Definition a k ()]
builtInDefinitions = []
