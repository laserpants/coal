{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Noll.Core.LLVM.IRInstruction.Interpreter (
  interpret,
  IRInterpreterEnv (..),
  evalInterpreter,
  runInterpreter,
) where

import Control.Monad.Free (iterM)
import Control.Monad.RWS (MonadReader, MonadState, MonadWriter, RWS, asks, gets, local, modify, runRWS, tell)
import Data.Text (Text, intercalate)
import Noll.Common.Environment (Environment (..))
import Noll.Core.LLVM.IREncodable (IREncodable (..), enquote, irAnnotate)
import Noll.Core.LLVM.IRInstruction (IRInstr, IRInstrOp, IRInstrOpF (..))
import Noll.Core.LLVM.IRType (IRType (..), ptr)
import Noll.Core.LLVM.IRValue (IRValue (..))
import Noll.Utils (Name, Over)
import TextShow (showt)

import qualified Data.Text as Text
import qualified Noll.Common.Environment as Environment

data IRInterpreterArtifact
  = InterpreterArtifactFunctionApply Int
  | InterpreterArtifactTODO
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

{-# INLINE overIRInterpreterStateArtifacts #-}
overIRInterpreterStateArtifacts :: Over IRInterpreterState [IRInterpreterArtifact]
overIRInterpreterStateArtifacts f IRInterpreterState{..} = IRInterpreterState{irInterpreterStateArtifacts = f irInterpreterStateArtifacts, ..}

{-# INLINE incrementRegisterIndex #-}
incrementRegisterIndex :: (MonadState IRInterpreterState m) => m ()
incrementRegisterIndex = modify (overIRInterpreterStateRegisterIndex succ)

{-# INLINE incrementLabelIndex #-}
incrementLabelIndex :: (MonadState IRInterpreterState m) => m ()
incrementLabelIndex = modify (overIRInterpreterStateLabelIndex succ)

addArtifact :: (MonadState IRInterpreterState m) => IRInterpreterArtifact -> m ()
addArtifact art = modify (overIRInterpreterStateArtifacts (art :))

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
      error "IGep"
    IGepNull t v1 next ->
      error "IGepNull"
    ILoad t v1 next ->
      error "ILoad"
    IStore v1 v2 next ->
      error "IStore"
    ISwitch v n cs next ->
      error "ISwitch"
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
      error "ILabel"
    IBlock name instr next -> do
      error "IBlock"
    IRuntimeApply arity next -> do
      addArtifact (InterpreterArtifactFunctionApply arity)
      next ("apply" <> showt arity)
    IRuntimeClosure fn applied remain next -> do
      error "IRuntimeClosure"
    IHashMapKey name next -> do
      error "IHashMapKey"
    IDataConstructor t name next -> do
      error "IDataConstructor"
    _ ->
      error "TODO"

{-# INLINE interpret #-}
interpret :: IRInstr a -> IRInterpreter a
interpret = iterM interpreter

evalInterpreter :: IRInterpreterEnv -> IRInterpreter a -> [IRLine]
evalInterpreter env ipt = undefined -- evalState (execWriterT (runReaderT (getIRInterpreter ipt) env)) initialIRInterpreterState

runInterpreter :: IRInterpreterEnv -> IRInterpreter a -> (a, IRInterpreterState, [IRLine])
runInterpreter env ipt = runRWS (getIRInterpreter ipt) env initialIRInterpreterState
