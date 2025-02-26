{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Noll.Core.LLVM.IRInstruction.Interpreter (
  interpret,
  IRInterpreter (..),
  IRInterpreterEnv (..),
  IRLine (..),
  evalInterpreter,
  runInterpreter,
  inValueEnv,
) where

import Control.Monad.Free (iterM)
import Control.Monad.RWS (MonadReader, MonadState, MonadWriter, RWS, asks, gets, local, modify, runRWS, tell)
import Data.Text (Text, intercalate)
import Noll.Common.Environment (Environment (..))
import Noll.Core.LLVM.IREncodable (IRAnnotated (..), IREncodable (..), enquote, irAnnotate)
import Noll.Core.LLVM.IRInstruction (IRInstr, IRInstrOp, IRInstrOpF (..))
import Noll.Core.LLVM.IRType (IRType (..), fun, i8Ptr, irTypeOf, pointee, ptr)
import Noll.Core.LLVM.IRValue (IRValue (..))
import Noll.Utils (Name, Over)
import TextShow (showt)

import qualified Data.Text as Text
import qualified Noll.Common.Environment as Environment

data IRInterpreterArtifact
  = InterpreterArtifactFunctionApply Int
  | InterpreterArtifactClosure Int
  deriving (Show, Eq, Ord)

data IRInterpreterState = IRInterpreterState
  { irInterpreterStateRegisterIndex :: Int
  , irInterpreterStateLabelIndex :: Int
  , irInterpreterStateLabel :: Text
  , irInterpreterStateArtifacts :: [IRInterpreterArtifact]
  }
  deriving (Show, Eq, Ord)

{-# INLINE initialIRInterpreterState #-}
initialIRInterpreterState :: IRInterpreterState
initialIRInterpreterState =
  IRInterpreterState
    { irInterpreterStateRegisterIndex = 1
    , irInterpreterStateLabelIndex = 1
    , irInterpreterStateLabel = ""
    , irInterpreterStateArtifacts = []
    }

{-# INLINE overIRInterpreterStateRegisterIndex #-}
overIRInterpreterStateRegisterIndex :: Over IRInterpreterState Int
overIRInterpreterStateRegisterIndex f IRInterpreterState{..} = IRInterpreterState{irInterpreterStateRegisterIndex = f irInterpreterStateRegisterIndex, ..}

{-# INLINE overIRInterpreterStateLabelIndex #-}
overIRInterpreterStateLabelIndex :: Over IRInterpreterState Int
overIRInterpreterStateLabelIndex f IRInterpreterState{..} = IRInterpreterState{irInterpreterStateLabelIndex = f irInterpreterStateLabelIndex, ..}

{-# INLINE overIRInterpreterStateLabel #-}
overIRInterpreterStateLabel :: Over IRInterpreterState Text
overIRInterpreterStateLabel f IRInterpreterState{..} = IRInterpreterState{irInterpreterStateLabel = f irInterpreterStateLabel, ..}

{-# INLINE overIRInterpreterStateArtifacts #-}
overIRInterpreterStateArtifacts :: Over IRInterpreterState [IRInterpreterArtifact]
overIRInterpreterStateArtifacts f IRInterpreterState{..} = IRInterpreterState{irInterpreterStateArtifacts = f irInterpreterStateArtifacts, ..}

{-# INLINE incrementRegisterIndex #-}
incrementRegisterIndex :: (MonadState IRInterpreterState m) => m ()
incrementRegisterIndex = modify (overIRInterpreterStateRegisterIndex succ)

{-# INLINE incrementLabelIndex #-}
incrementLabelIndex :: (MonadState IRInterpreterState m) => m ()
incrementLabelIndex = modify (overIRInterpreterStateLabelIndex succ)

{-# INLINE setLabel #-}
setLabel :: (MonadState IRInterpreterState m) => Text -> m ()
setLabel label = modify (overIRInterpreterStateLabel (const label))

{-# INLINE addArtifact #-}
addArtifact :: (MonadState IRInterpreterState m) => IRInterpreterArtifact -> m ()
addArtifact af = modify (overIRInterpreterStateArtifacts (af :))

data IRInterpreterEnv = IRInterpreterEnv
  { irInterpreterValueEnv :: Environment IRValue
  , irInterpreterConstructorEnv :: Environment Int
  }
  deriving (Show, Eq, Ord, Read)

{-# INLINE inValueEnv #-}
inValueEnv :: (Environment IRValue -> Environment IRValue) -> IRInterpreterEnv -> IRInterpreterEnv
inValueEnv f IRInterpreterEnv{..} = IRInterpreterEnv{irInterpreterValueEnv = f irInterpreterValueEnv, ..}

{-# INLINE insertBoundVars #-}
insertBoundVars :: [(Name, IRValue)] -> IRInterpreterEnv -> IRInterpreterEnv
insertBoundVars = inValueEnv . Environment.insertMultiple

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

{-# INLINE irAnnotated #-}
irAnnotated :: IRValue -> Text
irAnnotated = irEncode . irAnnotate

{-# INLINE irEncodeName #-}
irEncodeName :: Text -> Text
irEncodeName v = "%" <> enquote v

{-# INLINE irEncodeLabel #-}
irEncodeLabel :: Text -> Text
irEncodeLabel v = "label" <> " " <> irEncodeName v

irEncodePhiBranches :: Name -> IRValue -> Text
irEncodePhiBranches n v = "[" <> withCommas [irEncode v, irEncodeName n] <> "]"

irEncodeSwitchBranch :: Name -> IRValue -> Text
irEncodeSwitchBranch ll v = withCommas [irAnnotated v, irEncodeLabel ll]

irEncodeSwitchBranches :: [(Name, IRValue)] -> Text
irEncodeSwitchBranches bs = Text.intercalate " " (uncurry irEncodeSwitchBranch <$> bs)

instruction :: IRType -> (IRValue -> IRInterpreter a) -> [Text] -> IRInterpreter a
instruction t next tokens = do
  r <- nextRegister t
  tell [LInstruction ([irEncode r, "="] <> tokens)]
  next r

instruction0 :: IRInterpreter a -> [Text] -> IRInterpreter a
instruction0 next tokens = tell [LInstruction tokens] >> next

offset :: IRType -> IRValue -> IRType
offset (TStruct ts) (I32 n) = ts !! fromIntegral n
offset (TNamed _ t) n = offset t n
offset (TArray _ t) _ = t
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
      instruction t next ["icmp", "eq", withCommas [irAnnotated v1, irEncode v2]]
    ICmpSLt t v1 v2 next ->
      instruction t next ["icmp", "slt", withCommas [irAnnotated v1, irEncode v2]]
    ICmpSGt t v1 v2 next ->
      instruction t next ["icmp", "sgt", withCommas [irAnnotated v1, irEncode v2]]
    IXOr t v1 v2 next ->
      instruction t next ["xor", irEncode t, withCommas [v1, v2]]
    IOr t v1 v2 next ->
      instruction t next ["or", irEncode t, withCommas [v1, v2]]
    IAnd t v1 v2 next ->
      instruction t next ["and", irEncode t, withCommas [v1, v2]]
    IInttoptr v t next ->
      instruction t next ["inttoptr", irAnnotated v, "to", irEncode t]
    IPtrtoint v t next ->
      instruction t next ["ptrtoint", irAnnotated v, "to", irEncode t]
    IBCast v t next ->
      instruction t next ["bitcast", irAnnotated v, "to", irEncode t]
    IAlloca t next ->
      instruction (ptr t) next ["alloca", irEncode t]
    IComment text next -> do
      tell [LComment text]
      next
    IRet t v next ->
      instruction0 next ["ret", irEncode t, irEncode v]
    ICall TVoid v vs next ->
      error "TODO"
    ICall t v vs next ->
      instruction t next ["call", irEncode t, irEncode v <> "(" <> withCommas (irAnnotated <$> vs) <> ")"]
    ICallGlobal t name vs next ->
      instruction t next ["call", irEncode t, "@" <> enquote name <> "(" <> withCommas (irAnnotated <$> vs) <> ")"]
    IBr v names next ->
      instruction0 next ["br", withCommas (irAnnotated v : (irEncodeLabel <$> names))]
    IBr1 name next ->
      instruction0 next ["br", irEncodeLabel name]
    IPhi t vs next ->
      instruction t next ["phi", irEncode t, withCommas (uncurry irEncodePhiBranches <$> vs)]
    IGep t v1 v2 v3 next ->
      instruction (ptr (offset t v3)) next ["getelementptr", withCommas (irEncode (pointee (irTypeOf v1)) : (irEncode . IRAnnotated <$> [v1, v2, v3]))]
    IGepNull t v1 next ->
      instruction t next ["getelementptr", withCommas [irEncode t, irEncode t <> " null", irEncode (IRAnnotated v1)]]
    ILoad t v1 next ->
      instruction t next ["load", withCommas [irEncode t, irEncode (IRAnnotated v1)]]
    IStore v1 v2 next ->
      instruction0 next ["store", withCommas [irEncode (irAnnotated v1), irEncode (irAnnotated v2)]]
    ISwitch v n cs next ->
      instruction0 next ["switch", withCommas [irAnnotated v, irEncodeLabel n], "[" <> irEncodeSwitchBranches cs <> "]"]
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
    IRuntimeApply arity next -> do
      addArtifact (InterpreterArtifactFunctionApply arity)
      next ("apply" <> showt arity)
    IRuntimeClosure fn applied remain next -> do
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
      error "IHashMapKey"
    IDataConstr t name next -> do
      error "IDataConstr"
    _ ->
      error "TODO"

{-# INLINE interpret #-}
interpret :: IRInstr a -> IRInterpreter a
interpret = iterM interpreter

evalInterpreter :: IRInterpreterEnv -> IRInterpreter a -> [IRLine]
evalInterpreter env ipt = undefined -- evalState (execWriterT (runReaderT (getIRInterpreter ipt) env)) initialIRInterpreterState

runInterpreter :: IRInterpreterEnv -> IRInterpreter a -> (a, IRInterpreterState, [IRLine])
runInterpreter env ipt = runRWS (getIRInterpreter ipt) env initialIRInterpreterState
