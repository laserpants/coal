{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Pass.PhaseLowering (phaseLowering) where

import Coal.Compiler.Build.Envelope (BuildEnvelope (..))
import Coal.Compiler.Metadata (Metadata (..))
import Coal.Compiler.Pass (Pass (..), mapPass, (>->))
import Coal.Compiler.Pass.PhaseLowering.KernelCodegen (passKernelCodegen)
import Coal.Compiler.Pass.PhaseLowering.KernelTranslateNew (passKernelTranslateNew)
import Coal.Language (IndexedType, Kind)
import Coal.Language.Module
import Control.Monad.IO.Class (MonadIO)
import Data.ByteString (ByteString)
import Extras (Name)

phaseLowering :: (MonadIO m) => Pass Metadata m [BuildEnvelope (Module Metadata Kind IndexedType)] [(Name, ByteString)]
phaseLowering =
  mapPass passKernelTranslateNew
    >-> passKernelCodegen
