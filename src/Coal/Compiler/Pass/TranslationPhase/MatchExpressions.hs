module Coal.Compiler.Pass.TranslationPhase.MatchExpressions (passMatchExpressions) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.PatternMatching
import Coal.Compiler.Stack
import Coal.Language (IndexedType, Kind (..))
import Coal.ProtoLanguage.ProtoModule

passMatchExpressions :: (Monad m) => Pass Metadata m (ProtoModule Metadata Kind IndexedType) (ProtoModule Metadata Kind IndexedType)
passMatchExpressions = Pass{runPass = pass}

pass :: (Monad m) => ProtoModule Metadata Kind IndexedType -> CompilerT Metadata m (ProtoModule Metadata Kind IndexedType)
pass = compileMatchExprs
