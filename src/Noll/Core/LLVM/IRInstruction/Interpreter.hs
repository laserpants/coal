{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.LLVM.IRInstruction.Interpreter (
  module Noll.Core.LLVM.IRInstruction.Interpreter.Environment,
  module Noll.Core.LLVM.IRInstruction.Interpreter.Types,
  module Noll.Core.LLVM.IRInstruction.Interpreter.State,
  interpret,
  offset,
) where

import Control.Monad.Free (iterM)
import Control.Monad.RWS (asks, gets, local, tell)
import Data.Text (Text, intercalate)
import Noll.Core.LLVM.IREncodable (
  IRAnnotated (..),
  IREncodable (..),
  annotated,
  encodeLabel,
  irGlobalName,
  irLocalName,
 )
import Noll.Core.LLVM.IRInstruction (IRInstr, IRInstrOp, IRInstrOpF (..))
import Noll.Core.LLVM.IRInstruction.Interpreter.Environment (
  IRInterpreterEnv (..),
  inConstructorEnv,
  inValueEnv,
  insertBoundVars,
 )
import Noll.Core.LLVM.IRInstruction.Interpreter.Instruction (
  instruction,
  instruction1,
 )
import Noll.Core.LLVM.IRInstruction.Interpreter.State (
  IRInterpreterArtifact (..),
  IRInterpreterState (..),
  addArtifact,
  setLabel,
 )
import Noll.Core.LLVM.IRInstruction.Interpreter.Types (
  IRInterpreter (..),
  IRLine (..),
  evalInterpreter,
  nextLabelIndex,
  runInterpreter,
 )
import Noll.Core.LLVM.IRType (IRType (..))
import Noll.Core.LLVM.IRType.Syntax (fun, i8Ptr, ptr, stringLiteralType)
import Noll.Core.LLVM.IRValue (IRValue (..))
import Noll.Utils (Name)
import TextShow (showt)

import qualified Data.Text as Text
import qualified Noll.Common.Environment as Environment

{-# INLINE interpret #-}
interpret :: IRInstr a -> IRInterpreter a
interpret = iterM interpreter

interpreter :: IRInstrOp (IRInterpreter a) -> IRInterpreter a
interpreter =
  \case
    IAdd t v1 v2 next ->
      instruction t next ["add", irEncode t, withCommas [v1, v2]]
    ISub t v1 v2 next ->
      instruction t next ["sub", irEncode t, withCommas [v1, v2]]
    IMul t v1 v2 next ->
      instruction t next ["mul", irEncode t, withCommas [v1, v2]]
    ICmpEq t v1 v2 next ->
      instruction t next ["icmp", "eq", withCommas [annotated v1, irEncode v2]]
    ICmpSLt t v1 v2 next ->
      instruction t next ["icmp", "slt", withCommas [annotated v1, irEncode v2]]
    ICmpSGt t v1 v2 next ->
      instruction t next ["icmp", "sgt", withCommas [annotated v1, irEncode v2]]
    IXOr t v1 v2 next ->
      instruction t next ["xor", irEncode t, withCommas [v1, v2]]
    IOr t v1 v2 next ->
      instruction t next ["or", irEncode t, withCommas [v1, v2]]
    IAnd t v1 v2 next ->
      instruction t next ["and", irEncode t, withCommas [v1, v2]]
    IInttoptr v t next ->
      instruction t next ["inttoptr", annotated v, "to", irEncode t]
    IPtrtoint v t next ->
      instruction t next ["ptrtoint", annotated v, "to", irEncode t]
    IBitcast v t next ->
      instruction t next ["bitcast", annotated v, "to", irEncode t]
    IAlloca t v next ->
      instruction (ptr t) next ["alloca", withCommas [irEncode t, annotated v]]
    IComment text next -> do
      tell [LComment text]
      next
    IRet t v next ->
      instruction1 next ["ret", irEncode t, irEncode v]
    ICall TVoid v vs next ->
      error "TODO"
    ICall t v vs next ->
      instruction t next ["call", irEncode t, irEncode v <> "(" <> withCommas (annotated <$> vs) <> ")"]
    ICallGlobal t name vs next ->
      instruction t next ["call", irEncode t, irGlobalName name <> "(" <> withCommas (annotated <$> vs) <> ")"]
    IBr v names next ->
      instruction1 next ["br", withCommas (annotated v : (encodeLabel <$> names))]
    IBr1 name next ->
      instruction1 next ["br", encodeLabel name]
    IPhi t vs next ->
      instruction t next ["phi", irEncode t, withCommas (uncurry phiBranches <$> vs)]
    IGep t v1 v2 v3 next ->
      instruction (ptr (offset t [v2, v3])) next ["getelementptr", withCommas (irEncode t : (irEncode . IRAnnotated <$> [v1, v2, v3]))]
    IGep1 t v1 v2 next ->
      instruction (ptr t) next ["getelementptr", withCommas (irEncode t : (irEncode . IRAnnotated <$> [v1, v2]))]
    IGepNull t v1 next ->
      instruction t next ["getelementptr", withCommas [irEncode t, irEncode t <> " null", irEncode (IRAnnotated v1)]]
    ILoad t v1 next ->
      instruction t next ["load", withCommas [irEncode t, irEncode (IRAnnotated v1)]]
    IStore v1 v2 next ->
      instruction1 next ["store", withCommas [irEncode (annotated v1), irEncode (annotated v2)]]
    ISwitch v n cs next ->
      instruction1 next ["switch", withCommas [annotated v, encodeLabel n], "[" <> switchBranches cs <> "]"]
    ILookup var next -> do
      env <- asks irInterpreterValueEnv
      case Environment.lookup var env of
        Nothing ->
          error ("Name not in scope: " <> show var)
        Just val ->
          next val
    IBind bound instr next -> do
      v <- local (insertBoundVars bound) (interpret instr)
      next v
    IIndex next -> do
      d <- nextLabelIndex
      next (showt d)
    ILabel name next -> do
      d <- nextLabelIndex
      next (name <> "." <> showt d)
    IBlock name instr next -> do
      setLabel name
      tell [LLabel name]
      r <- interpret instr
      l <- gets irInterpreterStateLabel
      next (l, r)
    IBlock1 name instr next -> do
      setLabel name
      tell [LLabel name]
      interpret instr
      next
    IApply arity next -> do
      addArtifact (InterpreterArtifactFunctionApply arity)
      next ("apply" <> showt arity)
    IClosure fn applied remain next -> do
      let name = "closure" <> showt applied
          signature n = fun i8Ptr (replicate n i8Ptr)
      addArtifact (InterpreterArtifactClosure applied)
      next
        ( name
        , Global (signature (remain + 1)) (name <> "_finalize")
        , Global (signature (remain + 1)) (name <> "_extend")
        , Global (signature (applied + remain)) fn
        )
    IHashMapKey name next -> do
      let label = "label." <> name
      addArtifact (InterpreterArtifactHashMapKey name)
      next (Global (ptr (stringLiteralType name)) label)
    IMemoize next -> do
      d <- nextLabelIndex
      let name = "ptr." <> showt d
      addArtifact (InterpreterArtifactMemoizedConstant name)
      next (Global i8Ptr name)
    IDataConstr t name next -> do
      addArtifact (InterpreterArtifactDataConstructor name t)
      env <- asks irInterpreterConstructorEnv
      case Environment.lookup name env of
        Nothing ->
          error ("No constructor " <> Text.unpack name)
        Just n -> do
          next (n, TNamed name t)
    _ ->
      error "TODO"

{-# INLINE withCommas #-}
withCommas :: (IREncodable a) => [a] -> Text
withCommas vs = intercalate ", " (irEncode <$> vs)

phiBranches :: Name -> IRValue -> Text
phiBranches n v = "[" <> withCommas [irEncode v, irLocalName n] <> "]"

switchBranch :: Name -> IRValue -> Text
switchBranch n v = withCommas [annotated v, encodeLabel n]

switchBranches :: [(Name, IRValue)] -> Text
switchBranches bs = Text.unwords (uncurry switchBranch <$> bs)

offset :: IRType -> [IRValue] -> IRType
offset (TPtr t) (I32 0 : ixs) = offset t ixs
offset (TNamed _ t) ixs = offset t ixs
offset (TStruct ts) (I32 0 : I32 n : _) = ts !! fromIntegral n
offset (TArray _ t) (I32 _ : _) = ptr t
offset t [] = t
offset _ _ = error "Implementation error"
