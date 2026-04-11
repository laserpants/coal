module Coal.Compiler.Pass.MainPhase (mainPhase) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Build.Envelope (BuildEnvelope (..))
import Coal.Compiler.Pass (Pass (..), liftPass, mapPass, overlayEnvironment, (>->))
import Coal.Compiler.Pass.TranslationPhase (translationPhasePasses)
import Coal.Compiler.Pass.TypePhase (typePhasePasses)
import Coal.Language (IndexedType, Kind)
import Coal.ProtoLanguage.ProtoModule
import Control.Monad.IO.Class (MonadIO)

mainPhasePasses :: (MonadIO m) => Pass Metadata m (BuildEnvelope (ProtoModule Metadata () ())) (BuildEnvelope (ProtoModule Metadata Kind IndexedType))
mainPhasePasses = liftPass (typePhasePasses >-> overlayEnvironment translationPhasePasses)

mainPhase :: (MonadIO m) => Pass Metadata m [BuildEnvelope (ProtoModule Metadata () ())] [BuildEnvelope (ProtoModule Metadata Kind IndexedType)]
mainPhase = mapPass mainPhasePasses
