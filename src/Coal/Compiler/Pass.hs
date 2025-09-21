{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass (Pass (..), (>->), ModuleBundle) where

import Coal.Ast.Metadata (Metadata (..))
import Coal.Compiler.Stack (CompilerT)
import Coal.Language
import Coal.Language.Module
import Control.Monad ((>=>))
import Data.Text (Text)
import Extra (Name)

type ModuleBundle = (Text, Module Metadata Kind ())

data Pass a m i o = Pass
  { passName :: Name
  , runPass :: i -> CompilerT a m o
  }

(>->) :: (Monad m) => Pass a m p q -> Pass a m q r -> Pass a m p r
p1 >-> p2 =
  Pass
    { passName = passName p1 <> " > " <> passName p2
    , runPass = runPass p1 >=> runPass p2
    }
