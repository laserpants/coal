module Coal.Compiler.Pass.TranslationPhase.DenormalizeObjects (passDenormalizeObjects) where

import Coal.AST.Normalization (NormalizationContext (denormalizeObject))
import Coal.Compiler.Pass (Pass (..))
import Coal.Language.Type (Type (..))
import Coal.ProtoLanguage.ProtoModule (ModuleExportList (..), ProtoModule (..))
import Data.Data (Data, Typeable)

passDenormalizeObjects :: (Monad m, Monoid a, Data a, Data k, Data (o k), Typeable o) => Pass a m (ProtoModule a k (Type o k)) (ProtoModule a k (Type o k))
passDenormalizeObjects = Pass{runPass = pure . denormalizeObject}
