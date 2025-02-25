{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Noll.Core.LLVM.IRInstruction.TH where

import Control.Monad.Free (MonadFree, liftF)
import Control.Monad.Free.TH (makeFree)
import Noll.Core.LLVM.IRInstruction (IRInstrOpF (..))

makeFree ''IRInstrOpF
