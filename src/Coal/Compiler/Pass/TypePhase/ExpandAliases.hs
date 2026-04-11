{-# LANGUAGE FlexibleContexts #-}

module Coal.Compiler.Pass.TypePhase.ExpandAliases (passExpandAliases) where

import Coal.Compiler.Aliases
import Coal.Compiler.Pass (Pass (..))
import Coal.Language.Module (Module (..))
import Coal.Language.Type
import Control.Monad.IO.Class (MonadIO)
import Data.Data (Data)

passExpandAliases :: (MonadIO m, Data a, Show a, Data k, AliasTransform (Type Parameter k)) => Pass a m (Module a k ()) (Module a k ())
passExpandAliases = Pass{runPass = aliasTransform}
