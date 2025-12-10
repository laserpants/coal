module Coal.Compiler.Pass.TranslationPhase.MatchExpressions (passMatchExpressions) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.PatternMatching
import Coal.Compiler.Stack (CompilerT)
import Coal.Language (IndexedType, Kind (..))
import Coal.Language.Module (Module)

passMatchExpressions :: (Monad m) => Pass Metadata m (Module Metadata Kind IndexedType) (Module Metadata Kind IndexedType)
passMatchExpressions = Pass{runPass = pass}

pass :: (Monad m) => Module Metadata Kind IndexedType -> CompilerT Metadata m (Module Metadata Kind IndexedType)
pass = compileMatchExprs
