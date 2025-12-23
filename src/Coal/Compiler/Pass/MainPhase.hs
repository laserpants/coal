module Coal.Compiler.Pass.MainPhase (mainPhase) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Pass (BuildUnit, Pass (..), liftPass, mapPass, overlayEnvironment, (>->))
import Coal.Compiler.Pass.TranslationPhase (translationPhasePasses)
import Coal.Compiler.Pass.TypePhase (typePhasePasses)
import Coal.Language (IndexedType, Kind)
import Coal.Language.Module (Module)
import Control.Monad.IO.Class (MonadIO)

mainPhasePasses :: (MonadIO m) => Pass Metadata m (BuildUnit (Module Metadata Kind ())) (BuildUnit (Module Metadata Kind IndexedType))
mainPhasePasses = liftPass (typePhasePasses >-> overlayEnvironment translationPhasePasses)

mainPhase :: (MonadIO m) => Pass Metadata m [BuildUnit (Module Metadata Kind ())] [BuildUnit (Module Metadata Kind IndexedType)]
mainPhase = mapPass mainPhasePasses
