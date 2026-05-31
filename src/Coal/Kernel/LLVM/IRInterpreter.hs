{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
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
import Coal.Kernel.LLVM.IRError
import Coal.Kernel.LLVM.IREval (IRTailContext (..))
import Coal.Kernel.LLVM.IREval.Closure (closureStructType)
import Coal.Kernel.LLVM.IREval.Comment (irComment)
import Coal.Kernel.LLVM.IREval.Conceal (irConceal, irReveal)
import Coal.Kernel.LLVM.IREval.Expr (IREval (..))
import Coal.Kernel.LLVM.IREval.Malloc (irMalloc, irMallocN)
import Coal.Kernel.LLVM.IRInstruction
import Coal.Kernel.LLVM.IRInstruction.Builders
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
  , CDeclare "exit_failure" i8Ptr []
  , CDeclare "debug_call_n_bounds" i8Ptr [i32]
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
  setCurrentFunction (Just name)
  CDefine name i8Ptr Nothing args <$> listenOnly (interpret f)

irEvalFun :: (IREval e) => [Label Syntax.Type] -> e -> IRInstr ()
irEvalFun lls e = do
  bound <- forM lls $
    \(Label t name) -> do
      let v = Local i8Ptr name
      r <- irReveal v (irTypeOf t)
      unless (r == v) (irComment ["^ Reveal arg. " <> name])
      pure (name, r)
  r1 <- bind bound (irEval InTail e) -- Function body is in tail position
  r2 <- irConceal r1
  unless (r1 == r2) (irComment ["^ Conceal return value"])
  ret r2

-- ---------------------------------------------------------------------------
-- cofix helpers: direct LLVM IR emission for machine$_cofix
-- ---------------------------------------------------------------------------

-- | Build a closure struct for a global function with 0 captured args.
cofixBuildClosure :: Name -> Int -> IRInstr IRValue
cofixBuildClosure name remaining = do
  let t = closureStructType 0
  r <- irMalloc t
  capSlot <- getelementptr t r (I32 0) (I32 0)
  store (I32 0) capSlot
  remSlot <- getelementptr t r (I32 0) (I32 1)
  store (I32 (fromIntegral remaining)) remSlot
  fnSlot <- getelementptr t r (I32 0) (I32 2)
  fnCast <- bitcast (Global (opaqueFunction remaining) name) i8Ptr
  store fnCast fnSlot
  bitcast r i8Ptr

-- | Navigate from a Machine i8* to its row hashmap i8*.
cofixExtractMachineHashmap :: IRValue -> IRInstr IRValue
cofixExtractMachineHashmap realMachine = do
  IRConstructor _ machineType <- makeConstructor (struct [i32, i8Ptr]) "Machine"
  machineTyped <- bitcast realMachine (ptr machineType)
  recordPtrSlot <- getelementptr machineType machineTyped (I32 0) (I32 1)
  recordNodePtr <- load i8Ptr recordPtrSlot
  IRConstructor _ recordType <- makeConstructor (struct [i32, i8Ptr]) "$Record"
  recordTyped <- bitcast recordNodePtr (ptr recordType)
  hashmapSlot <- getelementptr recordType recordTyped (I32 0) (I32 1)
  load i8Ptr hashmapSlot

-- | Lookup a named field in a Machine's row hashmap.
cofixLookupField :: IRValue -> Text -> IRInstr IRValue
cofixLookupField hashmap field = do
  keyGlobal <- makeKey field
  keyPtr <- getelementptr (stringLiteral (Text.length field + 1)) keyGlobal (I32 0) (I32 0)
  callg i8Ptr "hashmap_lookup" [hashmap, keyPtr]

{- | Body of @Builtin$.machine$_cofix$view(cell_ptr: i8*) -> i8*@
  Loads the real Machine from the cell and calls its view.
-}
cofixViewBody :: IRInstr ()
cofixViewBody = do
  let cellPtrArg = Local i8Ptr "cell_ptr"
  cell <- bitcast cellPtrArg (ptr i8Ptr)
  realMachine <- load i8Ptr cell
  hashmap <- cofixExtractMachineHashmap realMachine
  stateVal <- cofixLookupField hashmap "state"
  viewClosure <- cofixLookupField hashmap "view"
  argsArr <- irMallocN i8Ptr (I32 1)
  slot0 <- getelementptr1 i8Ptr argsArr (I32 0)
  store stateVal slot0
  result <- callg i8Ptr "apply" [viewClosure, I32 1, argsArr]
  ret result

{- | Body of @Builtin$.machine$_cofix$step(input: i8*, cell_ptr: i8*) -> i8*@
  Loads the real Machine from the cell and calls its step with input.
-}
cofixStepBody :: IRInstr ()
cofixStepBody = do
  let inputArg = Local i8Ptr "input"
  let cellPtrArg = Local i8Ptr "cell_ptr"
  cell <- bitcast cellPtrArg (ptr i8Ptr)
  realMachine <- load i8Ptr cell
  hashmap <- cofixExtractMachineHashmap realMachine
  stateVal <- cofixLookupField hashmap "state"
  stepClosure <- cofixLookupField hashmap "step"
  argsArr <- irMallocN i8Ptr (I32 2)
  slot0 <- getelementptr1 i8Ptr argsArr (I32 0)
  store inputArg slot0
  slot1 <- getelementptr1 i8Ptr argsArr (I32 1)
  store stateVal slot1
  result <- callg i8Ptr "apply" [stepClosure, I32 2, argsArr]
  ret result

{- | Body of @Builtin$.machine$_cofix(f: i8*) -> i8*@
  Allocates a mutable indirection cell, builds a proxy Machine whose
  step\/view load from the cell at call-time, calls f(proxy) to obtain
  the real Machine, stores it in the cell, then returns the proxy.
-}
cofixMainBody :: IRInstr ()
cofixMainBody = do
  let fArg = Local i8Ptr "f"
  -- Allocate the indirection cell (i8**) and initialise to null
  cell <- irMalloc i8Ptr
  store Null cell
  cellI8 <- bitcast cell i8Ptr
  -- Build helper closures (0 captured args each)
  stepClosure <- cofixBuildClosure "Builtin$.machine$_cofix$step" 2
  viewClosure <- cofixBuildClosure "Builtin$.machine$_cofix$view" 1
  -- Build the row hashmap: { state = cellI8, step = stepClosure, view = viewClosure }
  hashmap <- callg i8Ptr "hashmap_init" []
  stateKey <- makeKey "state"
  stateKeyPtr <- getelementptr (stringLiteral (Text.length "state" + 1)) stateKey (I32 0) (I32 0)
  void $ callg i8Ptr "hashmap_insert" [hashmap, stateKeyPtr, cellI8]
  stepKey <- makeKey "step"
  stepKeyPtr <- getelementptr (stringLiteral (Text.length "step" + 1)) stepKey (I32 0) (I32 0)
  void $ callg i8Ptr "hashmap_insert" [hashmap, stepKeyPtr, stepClosure]
  viewKey <- makeKey "view"
  viewKeyPtr <- getelementptr (stringLiteral (Text.length "view" + 1)) viewKey (I32 0) (I32 0)
  void $ callg i8Ptr "hashmap_insert" [hashmap, viewKeyPtr, viewClosure]
  -- Build the $Record node: { i32 tag=0, i8* hashmap }
  IRConstructor _ recordType <- makeConstructor (struct [i32, i8Ptr]) "$Record"
  recPtr <- irMalloc recordType
  recTagSlot <- getelementptr recordType recPtr (I32 0) (I32 0)
  store (I32 0) recTagSlot
  recMapSlot <- getelementptr recordType recPtr (I32 0) (I32 1)
  store hashmap recMapSlot
  recI8 <- bitcast recPtr i8Ptr
  -- Build the Machine node: { i32 tag=0, i8* $Record }
  IRConstructor _ machineType <- makeConstructor (struct [i32, i8Ptr]) "Machine"
  machPtr <- irMalloc machineType
  machTagSlot <- getelementptr machineType machPtr (I32 0) (I32 0)
  store (I32 0) machTagSlot
  machRecSlot <- getelementptr machineType machPtr (I32 0) (I32 1)
  store recI8 machRecSlot
  proxy <- bitcast machPtr i8Ptr
  -- Call f(proxy) to obtain the real Machine
  argsArr <- irMallocN i8Ptr (I32 1)
  slot0 <- getelementptr1 i8Ptr argsArr (I32 0)
  store proxy slot0
  realMachine <- callg i8Ptr "apply" [fArg, I32 1, argsArr]
  -- Store the real Machine into the cell so the proxy closures can find it
  store realMachine cell
  ret proxy

-- ---------------------------------------------------------------------------

interpretObject :: Object Syntax.Type (Syntax.Expr Syntax.Type) -> IRInterpreter [IRConstruct [IRLine]]
interpretObject =
  \case
    OFunction "Builtin$.machine$_cofix" _ _ -> do
      viewIR <- interpretFunction "Builtin$.machine$_cofix$view" cofixViewBody [Label i8Ptr "cell_ptr"]
      stepIR <- interpretFunction "Builtin$.machine$_cofix$step" cofixStepBody [Label i8Ptr "input", Label i8Ptr "cell_ptr"]
      mainIR <- interpretFunction "Builtin$.machine$_cofix" cofixMainBody [Label i8Ptr "f"]
      pure [viewIR, stepIR, mainIR]
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
      pure [CString name str Nothing]
    OConstant name e -> do
      ir <- interpretFunction name (irEvalFun [] e) []
      pure [ir]
    OExternal name _ -> do
      irTypes <- asks irInterpreterIRTypes
      case Environment.lookup name irTypes of
        Just (TFun t ts) ->
          pure [CDeclare name t ts]
        Just it ->
          pure [CExternGlobal name it]
        Nothing ->
          error $ "IRType not found for external: " <> Text.unpack name
    OData{} ->
      pure []

interpretArtifact :: IRInterpreterArtifact -> IRInterpreter [IRConstruct [IRLine]]
interpretArtifact =
  \case
    AHashMapKey name ->
      pure [CString ("label." <> name) (encodeUtf8 name) (Just LPrivate)]
    ADataConstructor name t ->
      pure [CType name t]
    AMemoizedConstant name ->
      pure [CGlobal name i8Ptr (Just LPrivate) Null]
    ACFunctionCall "bignum_init" _ _ ->
      pure []
    ACFunctionCall name t ts ->
      pure [CDeclare name t ts]
    AStringLiteral name str ->
      pure [CString name str (Just LPrivate)]
    ABignum name n ->
      pure [CString name (encodeUtf8 (showt n)) (Just LPrivate)]

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
    IZext v t next ->
      instruction t next ["zext", annotated v, "to", irEncode t]
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
    ICall marker t v vs next ->
      let prefix = case marker of
            NoTail -> []
            Tail -> ["tail"]
            MustTail -> ["musttail"]
       in instruction t next (prefix <> ["call", irEncode t, irEncode v <> "(" <> commaSep (annotated <$> vs) <> ")"])
    ICallGlobal marker t name vs next ->
      let prefix = case marker of
            NoTail -> []
            Tail -> ["tail"]
            MustTail -> ["musttail"]
       in instruction t next (prefix <> ["call", irEncode t, irGlobalName name <> "(" <> commaSep (annotated <$> vs) <> ")"])
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
    IMeta metaOp ->
      interpretMeta metaOp

interpretMeta :: MetaOpF IRValue IRType (IRInterpreter a) -> IRInterpreter a
interpretMeta = \case
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
        throwIRError (UnboundConstructor name)
      Just n ->
        next (IRConstructor n t1)
   where
    t1 = TNamed name t
  MakeKey name next -> do
    let labelName = "label." <> name
    addArtifact (AHashMapKey name)
    next (Global (ptr (stringLiteral (Text.length name + 1))) labelName)
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
        throwIRError (UnboundVariable var)
      Just val ->
        next val
  CurrentFunction next -> do
    currentFn <- gets irInterpreterStateCurrentFunction
    next currentFn
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
offset (TStruct ts) (_, I32 n)
  | fromIntegral n < length ts = ts !! fromIntegral n
  | otherwise = error ("Struct field index " <> show n <> " out of bounds for " <> show (length ts) <> " fields")
offset ty _ = error ("Invalid getelementptr on type: " <> show ty)
