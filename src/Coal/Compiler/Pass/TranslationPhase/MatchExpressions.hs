{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Pass.TranslationPhase.MatchExpressions (passMatchExpressions) where

import Coal.Ast.Metadata (Metadata (..))
import Coal.Compiler.Pass
import Coal.Compiler.PatternMatching
import Coal.Compiler.Stack
import Coal.Language (IndexedType, Kind (..))
import Coal.Language.Module

passMatchExpressions :: (Monad m) => Pass Metadata m (Module Metadata Kind IndexedType) (Module Metadata Kind IndexedType)
passMatchExpressions =
  Pass
    { passName = "MatchExpressions"
    , runPass = pass
    }

pass :: (Monad m) => Module Metadata Kind IndexedType -> CompilerT Metadata m (Module Metadata Kind IndexedType)
pass = compileMatchExprs
