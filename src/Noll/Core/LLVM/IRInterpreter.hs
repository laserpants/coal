{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Noll.Core.LLVM.IRInterpreter where

import Control.Monad.Free (iterM)
import Control.Monad.RWS (MonadReader, MonadState, MonadWriter, RWS, evalRWS, gets, runRWS)
import Control.Monad.Reader (asks, local)
import Control.Monad.State (modify)
import Control.Monad.Writer (tell)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Debug.Trace
import Noll.AST.FreeVars (FreeVars (..), boundIn, exceptNames)
import Noll.Common.Environment (Environment (..))
import Noll.Core.LLVM.IRConstruct (IRConstruct (..), IRLinkage (..))
import Noll.Core.LLVM.IREncodable (IRAnnotated (..), IREncodable (..), annotated, commaSep, encodeLabel, enquote, irGlobalName, irLocalName)
import Noll.Core.LLVM.IREval (irEvalFun)
import Noll.Core.LLVM.IREval.Expr (irEvalExpr)
import Noll.Core.LLVM.IRInstruction
import Noll.Core.LLVM.IRInterpreter.Artifact
import Noll.Core.LLVM.IRInterpreter.Environment
import Noll.Core.LLVM.IRInterpreter.Instruction
import Noll.Core.LLVM.IRInterpreter.Monad
import Noll.Core.LLVM.IRInterpreter.State
import Noll.Core.LLVM.IRType (IRType (..), IRTyped (..))
import Noll.Core.LLVM.IRType.Syntax
import Noll.Core.LLVM.IRValue (IRValue (..))
import Noll.Core.Language.Expr (Expr (..))
import Noll.Core.Language.Object
import Noll.Core.Language.Type (Type)
import Noll.Label (Label (..))
import Noll.Utils (Name, Over, listenOnly)
import TextShow (showt)

import qualified Data.Text as Text
import qualified Noll.Common.Environment as Environment
import qualified Noll.Core.Language as Core

interpretFunction :: Name -> IRInstr a -> [Label IRType] -> IRInterpreter (IRConstruct [IRLine])
interpretFunction name f args = CDefine name i8Ptr Nothing args <$> listenOnly (interpret f)

interpretObject :: Object Core.Type (Core.Expr Core.Type) -> IRInterpreter (IRConstruct [IRLine])
interpretObject =
  \case
    OFunction name lls e -> do
      local
        (flip (foldr insertLocal) lls)
        (interpretFunction name (irEvalFun e) [Label i8Ptr n | Label _ n <- lls])
    OConstant name e -> do
      w <- listenOnly (interpret (irEvalExpr e))
      error "TODO"
    _ ->
      error "TODO"
 where
  insertLocal (Label _ name) =
    inValueEnv (Environment.insert name (Local i8Ptr name))

interpretArtifact :: IRInterpreterArtifact -> IRInterpreter [IRConstruct [IRLine]]
interpretArtifact =
  \case
    InterpreterArtifactHashMapKey name ->
      pure [CString ("label." <> name) (encodeUtf8 name)]
    InterpreterArtifactDataConstructor name t ->
      pure [CType name t]
    InterpreterArtifactMemoizedConstant name ->
      pure [CGlobal name i8Ptr (Just LPrivate) Null]
    _ ->
      pure []

interpreter :: IRInstrOp (IRInterpreter a) -> IRInterpreter a
interpreter =
  \case
    IAdd t v1 v2 next ->
      instruction t next ["add", irEncode t, commaSep [v1, v2]]
    ISub t v1 v2 next ->
      instruction t next ["sub", irEncode t, commaSep [v1, v2]]
    IMul t v1 v2 next ->
      instruction t next ["mul", irEncode t, commaSep [v1, v2]]
    IDiv{} ->
      error "TODO"
    ICmpEq t v1 v2 next ->
      instruction t next ["icmp", "eq", commaSep [annotated v1, irEncode v2]]
    ICmpNe t v1 v2 next ->
      instruction t next ["icmp", "ne", commaSep [annotated v1, irEncode v2]]
    ICmpSLt t v1 v2 next ->
      instruction t next ["icmp", "slt", commaSep [annotated v1, irEncode v2]]
    ICmpSLE{} ->
      error "TODO"
    ICmpSGE{} ->
      error "TODO"
    ICmpSGt t v1 v2 next ->
      instruction t next ["icmp", "sgt", commaSep [annotated v1, irEncode v2]]
    IXOr t v1 v2 next ->
      instruction t next ["xor", irEncode t, commaSep [v1, v2]]
    IOr t v1 v2 next ->
      instruction t next ["or", irEncode t, commaSep [v1, v2]]
    IAnd t v1 v2 next ->
      instruction t next ["and", irEncode t, commaSep [v1, v2]]
    IInttoptr v t next ->
      instruction t next ["inttoptr", annotated v, "to", irEncode t]
    IPtrtoint v t next ->
      instruction t next ["ptrtoint", annotated v, "to", irEncode t]
    IBitcast v t next ->
      instruction t next ["bitcast", annotated v, "to", irEncode t]
    IAlloca t v next ->
      instruction (ptr t) next ["alloca", commaSep [irEncode t, annotated v]]
    IComment text next -> do
      tell [LComment text]
      next
    IRet t v next ->
      instruction1 next ["ret", irEncode t, irEncode v]
    ICall TVoid v vs next ->
      error "TODO"
    ICall t v vs next ->
      instruction t next ["call", irEncode t, irEncode v <> "(" <> commaSep (annotated <$> vs) <> ")"]
    ICallGlobal t name vs next ->
      instruction t next ["call", irEncode t, irGlobalName name <> "(" <> commaSep (annotated <$> vs) <> ")"]
    IBr v names next ->
      instruction1 next ["br", commaSep (annotated v : (encodeLabel <$> names))]
    IBr1 name next ->
      instruction1 next ["br", encodeLabel name]
    IPhi t vs next ->
      instruction t next ["phi", irEncode t, commaSep (uncurry phiBranches <$> vs)]
    IGep t v1 v2 v3 next ->
      instruction (ptr (offset t [v2, v3])) next ["getelementptr", commaSep (irEncode t : (irEncode . IRAnnotated <$> [v1, v2, v3]))]
    IGep1 t v1 v2 next ->
      instruction (ptr t) next ["getelementptr", commaSep (irEncode t : (irEncode . IRAnnotated <$> [v1, v2]))]
    IGepNull t v1 next ->
      instruction t next ["getelementptr", commaSep [irEncode t, irEncode t <> " null", irEncode (IRAnnotated v1)]]
    ILoad t v1 next ->
      instruction t next ["load", commaSep [irEncode t, irEncode (IRAnnotated v1)]]
    IStore v1 v2 next ->
      instruction1 next ["store", commaSep [irEncode (annotated v1), irEncode (annotated v2)]]
    ISwitch v n cs next ->
      instruction1 next ["switch", commaSep [annotated v, encodeLabel n], "[" <> switchBranches cs <> "]"]
    MetaLookup var next -> do
      env <- asks irInterpreterValueEnv
      case Environment.lookup var env of
        Nothing ->
          error ("Name not in scope: " <> show var)
        Just val ->
          next val
    MetaBind bound instr next -> do
      v <- local (insertBoundVars bound) (interpret instr)
      next v
    MetaIndex next -> do
      d <- nextLabelIndex
      next (showt d)
    MetaLabel name next -> do
      d <- nextLabelIndex
      next (name <> "." <> showt d)
    MetaBlock name instr next -> do
      setLabel name
      tell [LLabel name]
      r <- interpret instr
      l <- gets irInterpreterStateLabel
      next (l, r)
    MetaBlock1 name instr next -> do
      setLabel name
      tell [LLabel name]
      interpret instr
      next
    MetaApply argc next -> do
      addArtifact (InterpreterArtifactFunctionApply argc)
      next ("apply" <> showt argc)
    MetaClosure fn applied remain next -> do
      let name = "closure" <> showt applied
          signature n = fun i8Ptr (replicate n i8Ptr)
      addArtifact (InterpreterArtifactClosure applied)
      next
        ( IRClosure
            name
            (Global (signature (remain + 1)) (name <> "_finalize"))
            (Global (signature (remain + 1)) (name <> "_extend"))
            (Global (signature (applied + remain)) fn)
        )
    MetaKey name next -> do
      let label = "label." <> name
      addArtifact (InterpreterArtifactHashMapKey name)
      next (Global (ptr (stringLiteral (Text.length name + 1))) label)
    MetaMemoize next -> do
      d <- nextLabelIndex
      let name = "ptr." <> showt d
      addArtifact (InterpreterArtifactMemoizedConstant name)
      next (Global i8Ptr name)
    MetaConstructor t name next -> do
      addArtifact (InterpreterArtifactDataConstructor name t)
      env <- asks irInterpreterConstructorEnv
      case Environment.lookup name env of
        Nothing ->
          error ("No constructor " <> Text.unpack name)
        Just n -> do
          next (IRConstructor n (TNamed name t))

phiBranches :: Name -> IRValue -> Text
phiBranches n v = "[" <> commaSep [irEncode v, irLocalName n] <> "]"

switchBranches :: [(Name, IRValue)] -> Text
switchBranches bs = Text.unwords (uncurry branch <$> bs)
 where
  branch n v = commaSep [annotated v, encodeLabel n]

{-# INLINE interpret #-}
interpret :: IRInstr a -> IRInterpreter a
interpret = iterM interpreter

-- TODO: clean up
offset :: IRType -> [IRValue] -> IRType
offset (TPtr t) (I32 0 : ixs) = offset t ixs
offset (TNamed _ t) ixs = offset t ixs
offset (TStruct ts) (I32 0 : I32 n : _) = ts !! fromIntegral n
offset (TArray _ t) (I32 _ : _) = ptr t
offset t [] = t
offset _ _ = error "Implementation error"
