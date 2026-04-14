module Coal.Compiler.Pass.TranslationPhase.DenormalizeAst (
  passDenormalizeAst,
) where

import Coal.AST.Normalization (NormalizationContext (denormalizeObject))
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (CompilerT)
import Coal.Language.Module (Module (..), ModuleExportList (..))
import Coal.Language.Type (Type (..))
import Data.Data (Data, Typeable)

passDenormalizeAst :: (Monad m, Monoid a, Data a, Data k, Data (o k), Typeable o, Ord k) => Pass a m (Module a k (Type o k)) (Module a k (Type o k))
passDenormalizeAst = Pass{runPass = passImpl}

passImpl :: (Monad m, Monoid a, Data a, Data k, Data (o k), Typeable o, Ord k) => Module a k (Type o k) -> CompilerT a m (Module a k (Type o k))
passImpl = return . denormalizeObject
