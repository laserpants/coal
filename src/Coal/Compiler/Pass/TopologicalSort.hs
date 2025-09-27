{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass.TopologicalSort (passTopologicalSort) where

import Coal.Ast.Metadata (Metadata (..))
import Coal.Compiler.Pass
import Coal.Compiler.Stack (CompilerT, insertNamesC)
import Coal.Language
import Coal.Language.Module
import Extra (Name)

passTopologicalSort :: (Monad m) => Pass a m [Module Metadata Kind ()] [Module Metadata Kind ()]
passTopologicalSort =
  Pass
    { passName = "TopologicalSort"
    , runPass = pass
    }

pass :: (Monad m) => [Module Metadata Kind ()] -> CompilerT a m [Module Metadata Kind ()]
pass modules = do
  undefined

