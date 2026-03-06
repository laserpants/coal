{-# LANGUAGE FlexibleContexts #-}

module Coal.Compiler.Pass.TypePhase.ExpandAliases (passExpandAliases) where

import Coal.Compiler.Aliases
import Coal.Compiler.Pass (Pass (..))
import Coal.Language.Type
import Coal.ProtoLanguage.ProtoModule (ProtoModule (..))
import Data.Data (Data)

passExpandAliases :: (Monad m, Data a, Data k, AliasTransform (Type Parameter k)) => Pass a m (ProtoModule a k ()) (ProtoModule a k ())
passExpandAliases = Pass{runPass = aliasTransform}
