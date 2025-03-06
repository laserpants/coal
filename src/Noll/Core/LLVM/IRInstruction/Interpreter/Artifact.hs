{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.LLVM.IRInstruction.Interpreter.Artifact (artifactInterpreter) where

import Control.Monad.Writer (listen)
import Noll.Core.LLVM.IRConstruct (IRConstruct (..))
import Noll.Core.LLVM.IREval.Closure.Extend (irClosureExtend)
import Noll.Core.LLVM.IREval.Closure.Finalize (irClosureFinalize)
import Noll.Core.LLVM.IRInstruction.Interpreter (
  IRInterpreter (..),
  IRInterpreterArtifact (..),
  IRLine,
  interpret,
 )
import Noll.Core.LLVM.IRType.Syntax (i32, i8Ptr, i8PtrPtr, struct)
import Noll.Label (Label (..))
import TextShow (showt)

foo :: Int -> IRInterpreter (IRConstruct [IRLine])
foo n = do
  (_, w) <- listen (interpret (irClosureFinalize n undefined undefined undefined)) -- (interpret (irClosureExtend applied))
  let zz =
        CDefine
          undefined
          i8Ptr
          Nothing
          [ Label i8Ptr undefined
          , Label i32 undefined
          , Label i8PtrPtr undefined
          ]
          w
  pure zz

foo2 :: Int -> IRInterpreter (IRConstruct [IRLine])
foo2 n = do
  (_, w) <- listen (interpret (irClosureExtend n undefined undefined undefined)) -- (interpret (irClosureExtend applied))
  let zz =
        CDefine
          undefined
          i8Ptr
          Nothing
          [ Label i8Ptr undefined
          , Label i32 undefined
          , Label i8PtrPtr undefined
          ]
          w
  pure zz

artifactInterpreter :: IRInterpreterArtifact -> IRInterpreter [IRConstruct [IRLine]]
artifactInterpreter =
  \case
    InterpreterArtifactClosure applied -> do
      finalize <- foo applied
      extender <- foo2 applied
      pure (finalize : extender : [CType ("closure" <> showt applied) t])
     where
      t = struct (i32 : replicate (3 + applied) i8Ptr)
    _ ->
      error "TODO"
