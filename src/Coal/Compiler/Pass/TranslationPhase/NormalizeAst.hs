{-# LANGUAGE FlexibleContexts #-}

module Coal.Compiler.Pass.TranslationPhase.NormalizeAST (
  passNormalizeAST,
) where

import Coal.AST.Normalization (NormalizationContext (normalizeObject))
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (CompilerT)
import Coal.Language (IndexedType, Kind, Type, TypeIndex)
import Coal.Language.Module (Module)
import Data.Data (Data)

passNormalizeAST :: (Monad m, Monoid a, Data a) => Pass a m (Module a Kind (Type TypeIndex Kind)) (Module a Kind (Type TypeIndex Kind))
passNormalizeAST = Pass{runPass = passImpl}

passImpl :: (Monad m, Monoid a, Data a) => Module a Kind IndexedType -> CompilerT a m (Module a Kind IndexedType)
passImpl = return . normalizeObject
