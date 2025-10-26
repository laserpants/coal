{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.Pass (Pass (..), (>->), mapPass, localPass) where

import Coal.Ast.Metadata (Metadata (..))
import Coal.Compiler.Environment
import Coal.Compiler.Stack (CompilerT)
import Coal.Language (IndexedType, Kind)
import Coal.Language.Module (Module (..))
import Control.Monad ((>=>))
import Control.Monad.Reader (local)
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

mapPass :: (Monad m) => Pass a m i o -> Pass a m [i] [o]
mapPass p =
  Pass
    { passName = "map<" <> passName p <> ">"
    , runPass = traverse (runPass p)
    }

localPass :: (Monad m) => (Module Metadata Kind t -> CompilerEnvironment -> CompilerEnvironment) -> Pass Metadata m (Module Metadata Kind t) (Module Metadata Kind IndexedType) -> Pass Metadata m (Module Metadata Kind t) (Module Metadata Kind IndexedType)
localPass f p =
  Pass
    { passName = "local<" <> passName p <> ">"
    , runPass = \m -> local (f m) (runPass p m)
    }
