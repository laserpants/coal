module Coal.Compiler.Pass.TranslationPhase.MatchExpressions (passMatchExpressions) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.PatternMatching
import Coal.Compiler.Stack (CompilerT)
import Coal.Language (IndexedType, Kind (..))
import Coal.Language.Module (Module, fromProtoModule)
import Coal.ProtoCompiler.ProtoStack (ProtoCompilerT (..), protoOgetCurrentBuildC, setCurrentPathC)
import Coal.ProtoLanguage.ProtoModule

passMatchExpressions :: (Monad m) => Pass Metadata m (ProtoModule Metadata Kind IndexedType) (ProtoModule Metadata Kind IndexedType)
passMatchExpressions = Pass{runPass = pass}

pass :: (Monad m) => ProtoModule Metadata Kind IndexedType -> CompilerT Metadata (ProtoCompilerT m Metadata) (ProtoModule Metadata Kind IndexedType)
pass = compileMatchExprs
