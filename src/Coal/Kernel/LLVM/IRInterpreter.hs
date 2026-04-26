{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Coal.Kernel.LLVM.IRInterpreter (
  interpret,
  interpreter,
  interpretArtifact,
  interpretObject,
  interpretFunction,
  support,
  closureSupport,
) where

import qualified Coal.Common.Environment as Environment
import Coal.Common.Label (Label (..))
import Coal.Kernel.LLVM.IRConstruct (IRConstruct (..), IRLinkage (..))
import Coal.Kernel.LLVM.IREncodable
import Coal.Kernel.LLVM.IREval.Closure (closureStructType)
import Coal.Kernel.LLVM.IREval.Comment (irComment)
import Coal.Kernel.LLVM.IREval.Conceal (irConceal, irReveal)
import Coal.Kernel.LLVM.IREval.Expr (IREval (..))
import Coal.Kernel.LLVM.IRInstruction
import Coal.Kernel.LLVM.IRInstruction.TH (bind, ret)
import Coal.Kernel.LLVM.IRInterpreter.Artifact (IRInterpreterArtifact (..))
import Coal.Kernel.LLVM.IRInterpreter.Environment (IRInterpreterEnv (..), insertBoundVars)
import Coal.Kernel.LLVM.IRInterpreter.Instruction (instruction, instruction1)
import Coal.Kernel.LLVM.IRInterpreter.Monad
import Coal.Kernel.LLVM.IRInterpreter.State
import Coal.Kernel.LLVM.IRType (IRType (..), IRTyped (..))
import Coal.Kernel.LLVM.IRType.Syntax
import Coal.Kernel.LLVM.IRValue (IRValue (..))
import qualified Coal.Kernel.Language as Syntax
import Coal.Kernel.Language.Object (Object (..))
import Control.Monad (unless, void)
import Control.Monad.Free (iterM)
import Control.Monad.RWS (asks, gets, local, tell)
import qualified Data.ByteString as ByteString
import Data.Fix (Fix (..))
import Data.Text (Text, isPrefixOf)
import qualified Data.Text as Text
import Data.Text.Encoding (encodeUtf8)
import Extras (Name, forM, listenOnly, (<.>))
import TextShow (showt)

closureSupport :: [IRConstruct [IRLine]]
closureSupport =
  [ CDeclare "closure_finalize" i8Ptr [i8Ptr, i8PtrPtr]
  , CDeclare "closure_extend" i8Ptr [i8Ptr, i32, i8PtrPtr]
  , CDeclare "apply" i8Ptr [i8Ptr, i32, i8PtrPtr]
  ]

support :: [IRConstruct [IRLine]]
support =
  [ CType "closure" (closureStructType 0)
  , CDeclare "init" TVoid []
  , CDeclare "gc_init" TVoid []
  , CDeclare "gc_malloc" i8Ptr [i64]
  , CDeclare "hashmap_init" i8Ptr []
  , CDeclare "hashmap_insert" i8Ptr [i8Ptr, i8Ptr, i8Ptr]
  , CDeclare "hashmap_lookup" i8Ptr [i8Ptr, i8Ptr]
  , CDeclare "bignum_init" i8Ptr [i8Ptr]
  ]

interpretFunction :: Name -> IRInstr a -> [Label IRType] -> IRInterpreter (IRConstruct [IRLine])
interpretFunction name f args = do
  refreshInterpreterState
  CDefine name i8Ptr Nothing args <$> listenOnly (interpret f)

irEvalFun :: (IREval e) => [Label Syntax.Type] -> e -> IRInstr ()
irEvalFun lls e = do
  bound <- forM lls $
    \(Label t name) -> do
      let v = Local i8Ptr name
      r <- irReveal v (irTypeOf t)
      unless (r == v) (irComment ["^ Reveal arg. " <> name])
      pure (name, r)
  r1 <- bind bound (irEval e)
  r2 <- irConceal r1
  unless (r1 == r2) (irComment ["^ Conceal return value"])
  ret r2

interpretObject :: Object Syntax.Type (Syntax.Expr Syntax.Type) -> IRInterpreter [IRConstruct [IRLine]]
interpretObject =
  \case
    OFunction name lls e -> do
      ir <- interpretFunction name (irEvalFun lls e) [Label i8Ptr n | Label _ n <- lls]
      pure [ir]
    OConstant name (Fix (Syntax.ELit (Syntax.PInt32 n))) ->
      pure [CGlobal name i32 Nothing (I32 n)]
    OConstant name (Fix (Syntax.ELit (Syntax.PInt64 n))) ->
      pure [CGlobal name i64 Nothing (I64 n)]
    OConstant name (Fix (Syntax.ELit (Syntax.PFloat f))) ->
      pure [CGlobal name TFloat Nothing (Float f)]
    OConstant name (Fix (Syntax.ELit (Syntax.PDouble d))) ->
      pure [CGlobal name TDouble Nothing (Double d)]
    OConstant name (Fix (Syntax.ELit (Syntax.PBool b))) ->
      pure [CGlobal name TInt1 Nothing (I1 b)]
    OConstant name (Fix (Syntax.ELit Syntax.PUnit)) ->
      pure [CGlobal name TInt1 Nothing (I1 True)]
    OConstant name (Fix (Syntax.ELit (Syntax.PChar c))) ->
      pure [CGlobal name TInt32 Nothing (I32 c)]
    OConstant name (Fix (Syntax.ELit (Syntax.PString str))) ->
      pure [CString name str]
    OConstant name e -> do
      ir <- interpretFunction name (irEvalFun [] e) []
      pure [ir]
    OExternal name it _ ->
      case it of
        TFun t ts ->
          pure [CDeclare name t ts]
        _ ->
          error "Implementation error"
    OData{} ->
      pure []

interpretArtifact :: IRInterpreterArtifact -> IRInterpreter [IRConstruct [IRLine]]
interpretArtifact =
  \case
    AHashMapKey name ->
      pure [CString ("label." <> name) (encodeUtf8 name)]
    ADataConstructor name t ->
      pure [CType name t]
    AMemoizedConstant name ->
      pure [CGlobal name i8Ptr (Just LPrivate) Null]
    ACFunctionCall "bignum_init" _ _ ->
      pure []
    ACFunctionCall name t ts ->
      pure [CDeclare name t ts]
    AStringLiteral name str ->
      pure [CString name str]
    ABignum name n ->
      pure [CString name (encodeUtf8 (showt n))]

interpreter :: IRInstrOp (IRInterpreter a) -> IRInterpreter a
interpreter =
  \case
    IAdd t v1 v2 next ->
      instruction t next ["add", irEncode t, commaSep [v1, v2]]
    ISub t v1 v2 next ->
      instruction t next ["sub", irEncode t, commaSep [v1, v2]]
    IMul t v1 v2 next ->
      instruction t next ["mul", irEncode t, commaSep [v1, v2]]
    IFAdd t v1 v2 next ->
      instruction t next ["fadd", irEncode t, commaSep [v1, v2]]
    IFSub t v1 v2 next ->
      instruction t next ["fsub", irEncode t, commaSep [v1, v2]]
    IFMul t v1 v2 next ->
      instruction t next ["fmul", irEncode t, commaSep [v1, v2]]
    IUDiv t v1 v2 next ->
      instruction t next ["udiv", irEncode t, commaSep [v1, v2]]
    IFDiv t v1 v2 next ->
      instruction t next ["fdiv", irEncode t, commaSep [v1, v2]]
    IFNeg t v next ->
      instruction t next ["fneg", irEncode t, irEncode v]
    IICmp Eq t v1 v2 next ->
      instruction t next ["icmp", "eq", commaSep [annotated v1, irEncode v2]]
    IFCmp OEq t v1 v2 next ->
      instruction t next ["fcmp", "oeq", commaSep [annotated v1, irEncode v2]]
    IICmp Ne t v1 v2 next ->
      instruction t next ["icmp", "ne", commaSep [annotated v1, irEncode v2]]
    IFCmp ONe t v1 v2 next ->
      instruction t next ["fcmp", "one", commaSep [annotated v1, irEncode v2]]
    IICmp SLt t v1 v2 next ->
      instruction t next ["icmp", "slt", commaSep [annotated v1, irEncode v2]]
    IICmp SLte t v1 v2 next ->
      instruction t next ["icmp", "sle", commaSep [annotated v1, irEncode v2]]
    IICmp SGt t v1 v2 next ->
      instruction t next ["icmp", "sgt", commaSep [annotated v1, irEncode v2]]
    IICmp SGte t v1 v2 next ->
      instruction t next ["icmp", "sge", commaSep [annotated v1, irEncode v2]]
    IFCmp OLt t v1 v2 next ->
      instruction t next ["fcmp", "olt", commaSep [annotated v1, irEncode v2]]
    IFCmp OLte t v1 v2 next ->
      instruction t next ["fcmp", "ole", commaSep [annotated v1, irEncode v2]]
    IFCmp OGt t v1 v2 next ->
      instruction t next ["fcmp", "ogt", commaSep [annotated v1, irEncode v2]]
    IFCmp OGte t v1 v2 next ->
      instruction t next ["fcmp", "oge", commaSep [annotated v1, irEncode v2]]
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
    IAlloca1 t next ->
      instruction (ptr t) next ["alloca", irEncode t]
    IComment text next -> do
      tell [LComment text]
      next
    IRet v next ->
      instruction1 next ["ret", irEncode (annotated v)]
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
      instruction (ptr (offset t (v2, v3))) next ["getelementptr", commaSep (irEncode t : (irEncode . IRAnnotated <$> [v1, v2, v3]))]
    IGep1 t v1 v2 next ->
      instruction (ptr t) next ["getelementptr", commaSep (irEncode t : (irEncode . IRAnnotated <$> [v1, v2]))]
    IGepsize t v1 next ->
      instruction (ptr t) next ["getelementptr", commaSep [irEncode t, irEncode (ptr t) <> " null", irEncode (IRAnnotated v1)]]
    ILoad t v1 next ->
      instruction t next ["load", commaSep [irEncode t, irEncode (IRAnnotated v1)]]
    IStore v1 v2 next ->
      instruction1 next ["store", commaSep [irEncode (annotated v1), irEncode (annotated v2)]]
    ISwitch v n cs next ->
      instruction1 next ["switch", commaSep [annotated v, encodeLabel n], "[" <> switchBranches cs <> "]"]
    MakeLabel name next -> do
      d <- nextLabelIndex
      next (name <.> showt d)
    MakeIndex next -> do
      d <- nextLabelIndex
      next (showt d)
    MakeConstructor t name next -> do
      addArtifact (ADataConstructor name t)
      ix <- constructorIndex name
      case ix of
        Nothing ->
          error ("No constructor '" <> Text.unpack name <> "'")
        Just n ->
          next (IRConstructor n t1)
     where
      t1 = TNamed name t
    MakeKey name next -> do
      let label = "label." <> name
      addArtifact (AHashMapKey name)
      next (Global (ptr (stringLiteral (Text.length name + 1))) label)
    MakeString str next -> do
      d <- nextLabelIndex
      let name = "str." <> showt d
      addArtifact (AStringLiteral name str)
      next (Global (ptr (TArray (ByteString.length str + 1) i8)) name)
    MakeBignum n next -> do
      d <- nextLabelIndex
      let name = "bignum." <> showt d
      addArtifact (ABignum name n)
      next (Global (ptr (TArray (Text.length (showt n) + 1) i8)) name)
    CCall t name vs next -> do
      addArtifact (ACFunctionCall name t [irTypeOf v | v <- vs])
      instruction t next ["call", irEncode t, irGlobalName name <> "(" <> commaSep (annotated <$> vs) <> ")"]
    NameLookup var next -> do
      env <- asks irInterpreterValueEnv
      case Environment.lookup var env of
        Nothing ->
          error ("Name not in scope: '" <> show var <> "'")
        Just val ->
          next val
    ConstructorLookup name next -> do
      ix <- constructorIndex name
      next ix
    Bind bound instr next -> do
      v <- local (insertBoundVars bound) (interpret instr)
      next v
    Block name instr next -> do
      r <- makeBlock name instr
      l <- gets irInterpreterStateLabel
      next (l, r)
    Block1 name instr next -> do
      void (makeBlock name instr)
      next
    Memoize next -> do
      d <- nextLabelIndex
      let name = "ptr." <> showt d
      addArtifact (AMemoizedConstant name)
      next (Global i8Ptr name)

constructorIndex :: Name -> IRInterpreter (Maybe Int)
constructorIndex name = do
  env <- asks irInterpreterConstructorEnv
  if name == "$Record" || "$Tuple" `isPrefixOf` name
    then pure (Just 0)
    else case Environment.lookup name env of
      Nothing ->
        pure Nothing
      Just n -> do
        pure (Just n)

makeBlock :: Name -> IRInstr a -> IRInterpreter a
makeBlock name instr = do
  setLabel name
  tell [LLabel name]
  interpret instr

phiBranches :: Name -> IRValue -> Text
phiBranches n v = "[" <> commaSep [irEncode v, irLocalName n] <> "]"

switchBranches :: [(Name, IRValue)] -> Text
switchBranches bs = Text.unwords (uncurry branch <$> bs)
 where
  branch n v = commaSep [annotated v, encodeLabel n]

{-# INLINE interpret #-}
interpret :: IRInstr a -> IRInterpreter a
interpret = iterM interpreter

offset :: IRType -> (IRValue, IRValue) -> IRType
offset (TArray _ t) _ = t
offset (TNamed _ t) p = offset t p
offset (TStruct ts) (_, I32 n) = ts !! fromIntegral n
offset _ _ = error "Implementation error"
