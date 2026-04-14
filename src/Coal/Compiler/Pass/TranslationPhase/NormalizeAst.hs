{-# LANGUAGE FlexibleContexts #-}

module Coal.Compiler.Pass.TranslationPhase.NormalizeAst (
  passNormalizeAst,
) where

import Coal.AST.Normalization (NormalizationContext (normalizeObject))
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (CompilerT)
import Coal.Language
import Coal.Language.Module
import Data.Data (Data)

passNormalizeAst :: (Monad m, Monoid a, Data a) => Pass a m (Module a Kind (Type TypeIndex Kind)) (Module a Kind (Type TypeIndex Kind))
passNormalizeAst = Pass{runPass = passImpl}

passImpl :: (Monad m, Monoid a, Data a) => Module a Kind IndexedType -> CompilerT a m (Module a Kind IndexedType)
passImpl = return . normalizeObject
