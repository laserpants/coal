module Coal.Compiler.Pass.TypePhase.ExpandAliases (passExpandAliases) where

import Coal.Compiler.Aliases
import Coal.Compiler.Pass (Pass (..))
import Coal.Language.Module
import Data.Data (Data)

passExpandAliases :: (Monad m, Data a) => Pass a m (Module a k ()) (Module a k ())
passExpandAliases = Pass{runPass = expandAliases}
