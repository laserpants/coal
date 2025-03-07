{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Noll.Core.LLVM.IRInstruction.Interpreter (
  module Noll.Core.LLVM.IRInstruction.Interpreter.Environment,
  interpret,
  IRInterpreter (..),
  IRInterpreterArtifact (..),
  IRInterpreterState (..),
  IRLine (..),
  offset,
  runInterpreter,
  evalInterpreter,
) where

import Control.Monad.Free (iterM)
import Control.Monad.RWS (
  MonadReader,
  MonadState,
  MonadWriter,
  RWS,
  asks,
  evalRWS,
  gets,
  local,
  runRWS,
  tell,
 )
import Data.Text (Text, intercalate)
import Noll.Core.LLVM.IREncodable (
  IRAnnotated (..),
  IREncodable (..),
  annotated,
  encodeLabel,
  enquote,
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
import Noll.Core.LLVM.IRInstruction.Interpreter.State (
  IRInterpreterArtifact (..),
  IRInterpreterState (..),
  addArtifact,
  incrementLabelIndex,
  incrementRegisterIndex,
  initialIRInterpreterState,
  setLabel,
 )
import Noll.Core.LLVM.IRType (IRType (..))
import Noll.Core.LLVM.IRType.Syntax (fun, i8Ptr, ptr, stringLiteralType)
import Noll.Core.LLVM.IRValue (IRValue (..))
import Noll.Utils (Name)
import TextShow (showt)

import qualified Data.Text as Text
import qualified Noll.Common.Environment as Environment

data IRLine
  = LInstruction [Text]
  | LComment Text
  | LLabel Text
  deriving (Show, Eq, Ord)

instance IREncodable IRLine where
  irEncode =
    \case
      LInstruction tokens ->
        "  " <> Text.unwords tokens
      LComment text ->
        "  ; " <> text
      LLabel text ->
        enquote text <> ":"

newtype IRInterpreter a = IRInterpreter {getIRInterpreter :: RWS IRInterpreterEnv [IRLine] IRInterpreterState a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState IRInterpreterState
    , MonadWriter [IRLine]
    , MonadReader IRInterpreterEnv
    )

evalInterpreter :: IRInterpreterEnv -> IRInterpreter a -> (a, [IRLine])
evalInterpreter env ri = evalRWS (getIRInterpreter ri) env initialIRInterpreterState

runInterpreter :: IRInterpreterEnv -> IRInterpreter a -> (a, IRInterpreterState, [IRLine])
runInterpreter env ri = runRWS (getIRInterpreter ri) env initialIRInterpreterState

nextRegister :: IRType -> IRInterpreter IRValue
nextRegister t = do
  n <- gets irInterpreterStateRegisterIndex
  incrementRegisterIndex
  pure (Local t (showt n))

nextLabelIndex :: IRInterpreter Int
nextLabelIndex = do
  incrementLabelIndex
  gets irInterpreterStateLabelIndex

{-# INLINE withCommas #-}
withCommas :: (IREncodable a) => [a] -> Text
withCommas vs = intercalate ", " (irEncode <$> vs)

phiBranches :: Name -> IRValue -> Text
phiBranches n v = "[" <> withCommas [irEncode v, irLocalName n] <> "]"

switchBranch :: Name -> IRValue -> Text
switchBranch n v = withCommas [annotated v, encodeLabel n]

switchBranches :: [(Name, IRValue)] -> Text
switchBranches bs = Text.unwords (uncurry switchBranch <$> bs)

instruction :: IRType -> (IRValue -> IRInterpreter a) -> [Text] -> IRInterpreter a
instruction t next tokens = do
  r <- nextRegister t
  tell [LInstruction ([irEncode r, "="] <> tokens)]
  next r

instruction1 :: IRInterpreter a -> [Text] -> IRInterpreter a
instruction1 next tokens = tell [LInstruction tokens] >> next

offset :: IRType -> [IRValue] -> IRType
offset (TPtr t) (I32 0 : ixs) = offset t ixs
offset (TNamed _ t) ixs = offset t ixs
offset (TStruct ts) (I32 0 : I32 n : _) = ts !! fromIntegral n
offset (TArray _ t) (I32 _ : _) = ptr t
offset t [] = t
offset _ _ = error "Implementation error"

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
      let label = "label_" <> name
      addArtifact (InterpreterArtifactHashMapKey label)
      next (Global (ptr (stringLiteralType name)) label)
    IMemoized next -> do
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

{-# INLINE interpret #-}
interpret :: IRInstr a -> IRInterpreter a
interpret = iterM interpreter
