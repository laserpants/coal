{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.LLVM.IRInstruction.Eval.Comment (
  irCommentBlock,
  irComments,
) where

import Control.Monad.Free (MonadFree)
import Data.Text (Text)
import Noll.Core.LLVM.IRInstruction (IRInstrOp)
import Noll.Core.LLVM.IRInstruction.TH
import Noll.Utils (forM_)

import qualified Data.Text as Text

irCommentBlock :: (MonadFree (IRInstrOp) m) => Text -> m a -> m a
irCommentBlock text instrs = do
  s <- iIndex
  iComment (Text.pack (replicate 75 '='))
  iComment ("[" <> s <> "] " <> text)
  iComment ""
  v <- instrs
  iComment ""
  iComment ("End: [" <> s <> "] " <> text)
  iComment "---- ^"
  pure v

irComments :: (MonadFree (IRInstrOp) m) => [Text] -> m ()
irComments txts = do
  iComment ""
  forM_ txts iComment
  iComment ""
