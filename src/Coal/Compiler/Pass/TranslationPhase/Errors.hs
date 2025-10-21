{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Pass.TranslationPhase.Errors (passTranslationPhaseErrors) where

import Coal.Ast.Metadata (Metadata (..))
import Coal.Compiler.Journal
import Coal.Compiler.Pass
import Coal.Compiler.Stack
import Coal.Language
import Coal.Language.Module
import Coal.TypeSystem.Constraint.Assumption (Assumption (..))
import Control.Monad
import Control.Monad.Except
import Control.Monad.State (gets)
import Data.List (nub)
import qualified Data.Text as Text

passTranslationPhaseErrors :: (MonadIO m) => Pass Metadata m (Module Metadata Kind IndexedType) (Module Metadata Kind IndexedType)
passTranslationPhaseErrors =
  Pass
    { passName = "TranslationPhaseErrors"
    , runPass = pass
    }

pass :: (Monad m) => Module Metadata Kind IndexedType -> CompilerT Metadata m (Module Metadata Kind IndexedType)
pass m@(Module path _ _) = do
  assumptions <- gets compilerAssumptions
  forM_ (nub assumptions) $
    \Assumption{..} ->
      -- TODO: Maybe look up these in environment and add additional constraints
      unless ("!" `Text.isPrefixOf` assumptionName) $
        tellErrors [NameNotInScope assumptionName (ErrorLocation (principalPath path) assumptionMetadata)]
  pure m
