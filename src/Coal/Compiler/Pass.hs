{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass (Pass (..), (>->)) where

import Coal.Compiler.Stack (CompilerT)
import Control.Monad ((>=>))
import Extra (Name)

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
