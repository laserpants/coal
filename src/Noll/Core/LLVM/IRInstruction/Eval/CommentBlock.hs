{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.LLVM.IRInstruction.Eval.CommentBlock (irCommentBlock) where

import Data.Text (Text)
import qualified Data.Text as Text
import Noll.Core.LLVM.IRInstruction (IRInstr)
import Noll.Core.LLVM.IRInstruction.TH

irCommentBlock :: Text -> IRInstr a -> IRInstr a
irCommentBlock text block = do
  s <- iIndex
  iComment (Text.pack (replicate 75 '='))
  iComment ("[" <> s <> "] " <> text)
  iComment ""
  v <- block
  iComment ""
  iComment ("End: [" <> s <> "] " <> text)
  iComment "---- ^"
  pure v
