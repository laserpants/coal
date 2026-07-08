{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}

{- |
Main LLVM IR code orchestrator.

This module implements the primary entry point for converting normalized
kernel language modules into LLVM IR. It delegates to specialized submodules
for constructors ('Coal.Kernel.LLVM.Constructor'), functions
('Coal.Kernel.LLVM.Function'), and primitives ('Coal.Kernel.LLVM.Prim'), while
handling the overall module structure, global environment, and expression
traversal.

= Expression evaluation strategy

Expressions are compiled in two modes:

  * __Value context__: produces an 'IROperand' for use in subsequent
    computations
  * __Tail context__: terminates the current basic block with a return or
    branch

The tail-call optimization strategy ensures that functions always end with a
proper terminator instruction, eliminating unnecessary temporary variables.
-}
module Coal.Kernel.LLVM.Codegen (irModule, irMainModule) where

import Control.Monad.Except (forM_, throwError, when)
import Control.Monad.Reader (asks, local)
import Control.Monad.State (gets, modify)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

import LLVM.IR
import qualified LLVM.IROperand.Constructors as O

import Coal.Kernel.LLVM.Boxing (irBox, irTypeRep, irUnbox)
import qualified Coal.Kernel.LLVM.Boxing as Boxing
import qualified Coal.Kernel.LLVM.Constructor as Constructor
import qualified Coal.Kernel.LLVM.Function as Function
import qualified Coal.Kernel.LLVM.Module as MA (collectImportedConstants, collectImportedDData, collectImportedFunctions, objectExprVarRefs, objectGlobalBinding)
import Coal.Kernel.LLVM.Monad
import Coal.Kernel.LLVM.Prim (irPrim)
import qualified Coal.Kernel.LLVM.Prim as Prim
import Coal.Kernel.LLVM.Runtime (callRuntime)
import Coal.Kernel.LLVM.RuntimeDefs
import Coal.Kernel.Language.Expr (Binding (..), Clause (..), Expr (..), Label (..))
import Coal.Kernel.Language.Module (Module (..))
import Coal.Kernel.Language.Object (FunctionScope (..), Object (..))
import Coal.Kernel.Language.Op (Op (..))
import Coal.Kernel.Language.Prim (Prim (..))
import Coal.Kernel.Language.Type (Type (..))
import Coal.Kernel.Language.Type.Function (arity)
import Coal.Kernel.Language.Type.HasType (HasType (typeOf), returnTypeOf, unfoldType)
import Common (Name, unsnoc)
import qualified Common.Environment as Environment

{-# INLINE irOp #-}
irOp :: Op (Expr Type) -> IRCodegen IROperand
irOp = Prim.irOp irValue

{-# INLINE irCase #-}
irCase :: Expr Type -> NonEmpty (Clause Type) -> IRCodegen ()
irCase = Constructor.irCase irValue irTail

nameLookup :: Type -> Name -> IRCodegen IROperand
nameLookup t name = do
  env <- asks codegenVarEnv
  case Environment.lookup name env of
    Nothing ->
      case irTypeRep t of
        TFun rty ts -> do
          declare name rty ts
          return (OGlobal (TFun rty ts) name)
        _ ->
          throwError (UnboundVariable name)
    Just op ->
      return op

{- | Automatically force a thunk (nullary function) if needed, and load
global constants.

When an operand refers to a global nullary function, it represents a lazy
thunk that must be called to obtain the actual value. When an operand refers
to a global constant (e.g. @i32@, @i64@), a 'load' is emitted because global
references always have pointer type in LLVM IR.
-}
forceThunk :: IROperand -> IRCodegen IROperand
forceThunk op =
  case op of
    OGlobal (TFun t []) _ ->
      call NoTail t op []
    OGlobal t name
      | t /= TPtr ->
          load t (OGlobal TPtr name)
    _ ->
      return op

irLet :: (Expr Type -> IRCodegen a) -> NonEmpty (Binding Type) -> Expr Type -> IRCodegen a
irLet eval vs e = foldr step (eval e) vs
 where
  step (Binding (Label _ name) e1) cont = do
    r1 <- irValue e1
    local (\env -> env{codegenVarEnv = Environment.insert name r1 (codegenVarEnv env)}) cont

irTail :: Expr Type -> IRCodegen ()
irTail =
  \case
    EVar (Label t name) -> do
      r1 <- nameLookup t name
      r2 <- case r1 of
        OGlobal (TFun _ (_ : _)) fname -> do
          let n = arity t
          callRuntime rtClosureNew [OGlobal TPtr (fname <> "$apply"), O.i32 n]
        _ ->
          forceThunk r1
      ret r2
    EIf e1 e2 e3 -> mdo
      r1 <- irValue e1
      condbr r1 thenL elseL
      thenL <- block "then"
      irTail e2
      elseL <- block "else"
      irTail e3
    ELet vs e ->
      irLet irTail vs e
    ECase _ e1 cs ->
      irCase e1 cs
    expr@(EApp t (EVar (Label t1 name)) es) -> do
      r1 <- nameLookup t1 name
      r2 <- case r1 of
        OGlobal (TFun _ ts) _ | length ts == length es -> do
          -- Fully saturated call
          rs <- traverse irValue es
          call Tail (irTypeRep t) r1 (NonEmpty.toList rs)
        _ -> do
          irValue expr
      ret r2
    ECall (Label t name) args k -> do
      -- External C call in tail position: evaluate as value, return
      r1 <- irValue (ECall (Label t name) args k)
      ret r1
    expr -> do
      r1 <- irValue expr
      ret r1

irApplyConstructor :: Label Type -> [Expr Type] -> IRCodegen IROperand
irApplyConstructor = Constructor.irApplyConstructor irValue

irLabel :: Name -> IRCodegen IROperand
irLabel name = do
  emitGlobal (IRString LPrivate label (Text.encodeUtf8 (name <> "\0")))
  return (OGlobal TPtr label)
 where
  label = ".label_" <> name

irPackArgs :: NonEmpty (Expr Type) -> IRCodegenT IRBuilder (IROperand, IROperand)
irPackArgs es = do
  vals <- traverse (Boxing.irBoxed irValue) es
  let argc = O.i32 (length vals)
  args <- alloca TPtr (O.i32 (length vals))
  forM_ (zip [0 ..] (NonEmpty.toList vals)) $
    \(i, v) -> do
      slot <- gep TPtr args [O.i32 @Int i]
      store v slot
  return (argc, args)

irValue :: Expr Type -> IRCodegen IROperand
irValue =
  \case
    ECon con ->
      irApplyConstructor con []
    EVar (Label t name) -> do
      op <- nameLookup t name
      case op of
        OGlobal (TFun _ (_ : _)) fname -> do
          let n = arity t
          callRuntime rtClosureNew [OGlobal TPtr (fname <> "$apply"), O.i32 n]
        _ ->
          forceThunk op
    EOp op ->
      irOp op
    ELit prim ->
      irPrim prim
    EApp _ (ECon con) es ->
      irApplyConstructor con (NonEmpty.toList es)
    expr@(EApp t (EVar (Label t1 name)) es) -> do
      o1 <- nameLookup t1 name
      vs <- traverse irValue es
      case o1 of
        OGlobal (TFun _ ts) _
          | length ts == length es -> do
              -- Fully saturated call
              call NoTail (irTypeRep t) o1 (NonEmpty.toList vs)
          | otherwise -> do
              let fnPtr = OGlobal TPtr (name <> "$apply")
              o2 <- callRuntime rtClosureNew [fnPtr, O.i32 (length ts)]
              (argc, args) <- irPackArgs es
              o3 <- callRuntime rtApply [o2, argc, args]
              irUnbox t o3
        op@(OLocal TPtr _) -> do
          (argc, args) <- irPackArgs es
          o3 <- callRuntime rtApply [op, argc, args]
          irUnbox t o3
        _ ->
          throwError (UnsupportedExpression expr)
    ELet vs e ->
      irLet irValue vs e
    EExt name e1 e2 -> do
      val <- Boxing.irBoxed irValue e1
      row <- irValue e2
      field <- irLabel name
      callRuntime rtRecordExtend [row, field, val]
    ENil ->
      callRuntime rtRecordEmpty []
    EGet (Label t fieldName) e1 -> do
      row <- irValue e1
      field <- irLabel fieldName
      r1 <- callRuntime rtRecordLookup [row, field]
      irUnbox t r1
    ECall (Label t name) args k -> do
      -- External C call via #{…} syntax.
      -- Arguments are boxed to TPtr. The function is declared with its
      -- actual return type; we box the raw result ourselves.
      boxedArgs <- traverse (Boxing.irBoxed irValue) args
      let resultType = returnTypeOf t
          resultIrType = irTypeRep resultType
      _ <- declare name resultIrType (replicate (length args) TPtr)
      rawResult <- call NoTail resultIrType (OGlobal (TFun resultIrType (replicate (length args) TPtr)) name) boxedArgs
      boxedResult <- Boxing.irBox resultType rawResult
      -- Apply the continuation to the boxed result.
      kv <- irValue k
      argSlot <- alloca TPtr (O.i32 @Int 1)
      gepSlot <- gep TPtr argSlot [O.i32 @Int 0]
      store boxedResult gepSlot
      applied <- callRuntime rtApply [kv, O.i32 @Int 1, argSlot]
      irUnbox resultType applied
    e ->
      throwError (UnsupportedExpression e)

-- | Convert a primitive literal to an IR constant, if representable directly.
primToIRConstant :: Prim -> Maybe (IRType, IRConstant)
primToIRConstant = Prim.primToIRConstant

{- | Compute the global environment binding for an object, if any.
DData constructors are not bound here; they are referenced via make_% in irApplyConstructor.
-}
objectGlobalBinding :: Object Type -> Maybe (Name, IROperand)
objectGlobalBinding = MA.objectGlobalBinding

{- | Search all modules for 'DConstant' objects whose name appears in the given
import list, returning (name, operand) pairs.
-}
collectImportedConstants :: [Module Type] -> [Name] -> [(Name, IROperand)]
collectImportedConstants = MA.collectImportedConstants

{- | Search all modules for 'DData' objects whose name appears in the given
import list, returning (constructorName, fieldCount) pairs.
-}
collectImportedDData :: [Module Type] -> [Name] -> [(Name, Int)]
collectImportedDData = MA.collectImportedDData

{- | Search all modules for 'DFunction' objects whose name appears in the given
import list, returning (functionName, arity) pairs.
-}
collectImportedFunctions :: [Module Type] -> [Name] -> [(Name, Int)]
collectImportedFunctions = MA.collectImportedFunctions

{- | Collect free variable references from the body of an object as (name, type) pairs.
Delegates to 'freeVars' from "Coal.Kernel.FreeVars"; parameters of a 'DFunction'
are excluded from the result.
-}
objectExprVarRefs :: Object Type -> [(Name, Type)]
objectExprVarRefs = MA.objectExprVarRefs

{- | Emit a @$apply@ trampoline for an inline C external, at most once per
module.  Uses the trampoline-name set in 'IRCodegenT' state for deduplication.
Must be called at module level (outside any active function definition).
-}
irInlineExternalTrampoline :: Name -> Type -> IRCodegen ()
irInlineExternalTrampoline name t = do
  let trampolineName = name <> "$apply"
  emitted <- gets (\(s, _, _) -> Set.member trampolineName s)
  if emitted
    then return ()
    else do
      modify (\(s, i, m) -> (Set.insert trampolineName s, i, m))
      let (argTypes, retType) = unsnoc (unfoldType t)
          labels = zipWith (\i at -> Label at ("p" <> Text.pack (show (i :: Int)))) [1 ..] argTypes
      irTrampoline LInternal name labels retType

irImportedDataConstructor :: Name -> Int -> IRCodegen ()
irImportedDataConstructor name fieldCount = do
  emitTypeDecl name (TStruct $ [i32] <> replicate fieldCount TPtr)
  declare ("make_%" <> name) TPtr (replicate fieldCount TPtr)

{- | Declare the $apply trampoline for a function defined in another module.
The importing module needs this declaration when the function is used through
partial application; the actual trampoline will be supplied by the defining
module at link time.
-}
irImportedFunctionTrampoline :: Name -> Int -> IRCodegen ()
irImportedFunctionTrampoline name arity_ =
  declare (name <> "$apply") TPtr (replicate arity_ TPtr)

irModule :: [Module Type] -> Module Type -> IRCodegen () -> IRCodegen IRModule
irModule allModules Module{moduleName, moduleObjects, moduleImports} k =
  buildModuleM moduleName $ do
    -- Emit struct types and external declarations for constructors defined in
    -- other modules so that 'irClause' can use sized getelementptr.
    forM_ (collectImportedDData allModules moduleImports) $
      uncurry irImportedDataConstructor
    -- Declare $apply trampolines for imported functions.
    forM_ (collectImportedFunctions allModules moduleImports) $
      uncurry irImportedFunctionTrampoline
    let importedConstantBindings = collectImportedConstants allModules moduleImports
    forM_ importedConstantBindings $ \(_, op) ->
      case op of
        OGlobal (TFun rty []) thunkName ->
          declare thunkName rty []
        _ ->
          return ()
    let bindings = mapMaybe objectGlobalBinding moduleObjects ++ importedConstantBindings
        allTagBindings =
          Map.fromList
            [ (ctorName, idx)
            | Module{moduleObjects = objs} <- allModules
            , DData _ ctors <- objs
            , (idx, (ctorName, _)) <- zip [0 ..] ctors
            ]
        boundNames = Set.fromList (map fst bindings)
        importedNames = Set.fromList [n | (n, _) <- collectImportedFunctions allModules moduleImports]
        excludedNames = Set.union boundNames importedNames
    -- Define $apply trampolines for inline C externals: names that appear in
    -- expression bodies with function types but are neither module-local nor
    -- imported Coal functions. Must be emitted here, at module level, before
    -- any function body is generated (irTrampoline calls beginFunction).
    forM_
      [ (name, t)
      | obj <- moduleObjects
      , (name, t) <- objectExprVarRefs obj
      , arity t > 0
      , not (Set.member name excludedNames)
      ]
      (uncurry irInlineExternalTrampoline)
    local
      ( \e ->
          e
            { codegenVarEnv = Environment.insertMultiple bindings (codegenVarEnv e)
            , codegenTagEnv = allTagBindings <> codegenTagEnv e
            }
      )
      $ forM_ moduleObjects
      $ \case
        DData _ ctors ->
          forM_ (zip [0 ..] ctors) $ \(index, (ctorName, ctorType)) ->
            irDataConstructor ctorName $
              Constructor.ConstructorDefinition
                { Constructor.constructorIndex = index
                , Constructor.constructorFieldCount = arity ctorType
                }
        DFunction scope name lls expr -> do
          let lnk = toIRLinkage scope
          irFunction lnk name lls expr
          irTrampoline lnk name lls (typeOf expr)
        DConstant name (ELit prim)
          | Just (irt, irc) <- primToIRConstant prim ->
              emitGlobal (IRConstant LExternal name irt irc)
        DConstant name expr ->
          irThunk name expr
        DExternal name t ->
          case irTypeRep t of
            TFun rty ts -> do
              declare name rty ts
              let (argTypes, retType) = unsnoc (unfoldType t)
                  labels = zipWith (\i at -> Label at ("p" <> Text.pack (show (i :: Int)))) [1 ..] argTypes
              irTrampoline LInternal name labels retType
            _ ->
              return ()
    k
irFunction :: IRLinkage -> Name -> [Label Type] -> Expr Type -> IRCodegen ()
irFunction lnk = Function.irFunction lnk irTail

irTrampoline :: IRLinkage -> Name -> [Label Type] -> Type -> IRCodegen ()
irTrampoline = Function.irTrampoline

irDataConstructor :: Name -> Constructor.ConstructorDefinition -> IRCodegen ()
irDataConstructor = Constructor.irDataConstructor

irThunk :: Name -> Expr Type -> IRCodegen ()
irThunk = Function.irThunk irValue

-- | Map a kernel-language 'FunctionScope' to the corresponding LLVM linkage.
toIRLinkage :: FunctionScope -> IRLinkage
toIRLinkage Exported = LExternal
toIRLinkage Local = LInternal

{- | Emit the C main entry point for a Main module.

This must be called after 'irModule' on the Main module to add the
program entry point that initializes the runtime and calls Main.main.
-}
irMainModule :: IRCodegen ()
irMainModule = do
  declare "rt_runtime_init" TVoid []
  define i32 "main" [] LExternal [] $ do
    callVoid NoTail TVoid (OGlobal (TFun TVoid []) "rt_runtime_init") []
    callVoid NoTail TPtr (OGlobal (TFun TPtr [TPtr]) "Main.main") [O.nullPtr TPtr]
    ret (O.i32 @Int 0)
