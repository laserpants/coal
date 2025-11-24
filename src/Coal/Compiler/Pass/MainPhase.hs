module Coal.Compiler.Pass.MainPhase (mainPhase) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Pass (Pass (..), mapPass, overlayEnvironment, (>->))
import Coal.Compiler.Pass.TranslationPhase (translationPhasePasses)
import Coal.Compiler.Pass.TypePhase (typePhasePasses)
import Coal.Language (IndexedType, Kind)
import Coal.Language.Module (Module)
import Control.Monad.IO.Class (MonadIO)

mainPhasePasses :: (MonadIO m) => Pass Metadata m (Module Metadata Kind ()) (Module Metadata Kind IndexedType)
mainPhasePasses = typePhasePasses >-> overlayEnvironment translationPhasePasses

mainPhase :: (MonadIO m) => Pass Metadata m [Module Metadata Kind ()] [Module Metadata Kind IndexedType]
mainPhase = mapPass mainPhasePasses
