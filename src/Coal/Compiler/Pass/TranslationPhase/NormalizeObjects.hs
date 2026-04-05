{-# LANGUAGE FlexibleContexts #-}

module Coal.Compiler.Pass.TranslationPhase.NormalizeObjects (passNormalizeObjects) where

import Coal.AST.Normalization (NormalizationContext (normalizeObject))
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (CompilerT)
import Coal.Language
import Coal.Language.Module
import Coal.Language.Type (Type (..))
import Coal.ProtoCompiler.ProtoStack (ProtoCompilerT (..))
import Coal.ProtoLanguage.ProtoModule
import Data.Data (Data, Typeable)

passNormalizeObjects :: (Monad m, Monoid a, Data a) => Pass a m (ProtoModule a Kind (Type TypeIndex Kind)) (Module a Kind (Type TypeIndex Kind))
passNormalizeObjects = Pass{runPass = bork}

bork :: (Monad m, Monoid a, Data a) => ProtoModule a Kind IndexedType -> CompilerT a (ProtoCompilerT m a) (Module a Kind IndexedType)
bork xx = do
  let a = normalizeObject (fromProtoModule xx)
  return a
