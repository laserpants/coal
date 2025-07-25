{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Kernel.LLVM.IREval.Comment (
  irCommentBlock,
  irComment,
) where

import Control.Monad.Free (MonadFree)
import Data.Text (Text)
import Extra (forM_)
import Coal.Kernel.LLVM.IRInstruction (IRInstrOp)
import Coal.Kernel.LLVM.IRInstruction.TH

import qualified Data.Text as Text

irCommentBlock :: (MonadFree (IRInstrOp) m) => Text -> m a -> m a
irCommentBlock text instrs = do
  s <- makeIndex
  comment (Text.pack (replicate 75 '='))
  comment ("[" <> s <> "] " <> text)
  comment ""
  v <- instrs
  comment ""
  comment ("End: [" <> s <> "] " <> text)
  comment "---- ^"
  pure v

irComment :: (MonadFree (IRInstrOp) m) => [Text] -> m ()
irComment txts = do
  comment ""
  forM_ txts comment
  comment ""
