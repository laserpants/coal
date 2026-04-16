module Coal.Compiler.Pass.TranslationPhase.DenormalizeAST (
  passDenormalizeAST,
) where

import Coal.AST.Normalization (NormalizationContext (denormalizeObject))
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (CompilerT)
import Coal.Language.Module (Module (..))
import Coal.Language.Type (Type (..))
import Data.Data (Data, Typeable)

passDenormalizeAST :: (Monad m, Monoid a, Data a, Data k, Data (o k), Typeable o, Ord k) => Pass a m (Module a k (Type o k)) (Module a k (Type o k))
passDenormalizeAST = Pass{runPass = passImpl}

passImpl :: (Monad m, Monoid a, Data a, Data k, Data (o k), Typeable o, Ord k) => Module a k (Type o k) -> CompilerT a m (Module a k (Type o k))
passImpl = return . denormalizeObject
