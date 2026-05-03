module Coal.Compiler.Pass.PhaseTranslation.DenormalizeAST (
  passDenormalizeAST,
) where

import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack (CompilerT)
import Coal.Language (Kind, Module (..), Type (..))
import Coal.Language.AST.Normalization (NormalizationContext (denormalizeObject))
import Data.Data (Data, Typeable)

passDenormalizeAST :: (Monad m, Monoid a, Data a, Data (o Kind), Typeable o) => Pass a m (Module a Kind (Type o Kind)) (Module a Kind (Type o Kind))
passDenormalizeAST = Pass{runPass = passImpl}

passImpl :: (Monad m, Monoid a, Data a, Data (o Kind), Typeable o) => Module a Kind (Type o Kind) -> CompilerT a m (Module a Kind (Type o Kind))
passImpl = return . denormalizeObject
