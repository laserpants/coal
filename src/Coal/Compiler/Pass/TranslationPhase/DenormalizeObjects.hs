module Coal.Compiler.Pass.TranslationPhase.DenormalizeObjects (passDenormalizeObjects) where

import Coal.AST.Normalization (NormalizationContext (denormalizeObject))
import Coal.Compiler.Pass (Pass (..))
import Coal.Language.Module (Module)
import Coal.Language.Type (Type (..))
import Data.Data (Data, Typeable)

passDenormalizeObjects :: (Monad m, Monoid a, Data a, Data k, Data (o k), Typeable o) => Pass a m (Module a k (Type o k)) (Module a k (Type o k))
passDenormalizeObjects = Pass{runPass = pure . denormalizeObject}
