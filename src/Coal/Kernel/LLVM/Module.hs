{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module-level analysis and global environment construction.

Provides utilities for analyzing Coal kernel language modules to extract
global bindings, imported symbols, and variable references. Used by the main
code generator to build the IR environment before compiling function bodies.
-}
module Coal.Kernel.LLVM.Module (
  buildConstructorFieldCounts,
  buildConstructorTagEnv,
  buildModuleObjectIndex,
  collectImportedBindings,
  objectGlobalBinding,
  objectExprVarRefs,
) where

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import LLVM.IR (IROperand (OGlobal), IRType (TFun, TPtr))

import Coal.Common.Name (Name)
import Coal.Kernel.FreeVars (freeVars)
import Coal.Kernel.LLVM.Boxing (irTypeRep, irValueTypeRep)
import Coal.Kernel.LLVM.Prim (primToIRConstant)
import Coal.Kernel.Language.Expr (Expr (..), Label (..))
import Coal.Kernel.Language.Interface (ObjectInterface (..), moduleInterface)
import Coal.Kernel.Language.Module (Module (..))
import Coal.Kernel.Language.Object (Object (..))
import Coal.Kernel.Language.Type (Type)
import Coal.Kernel.Language.Type.Function (arity)
import Coal.Kernel.Language.Type.HasType (HasType (typeOf))

{- | Compute the global environment binding for an object, if any.

'DData' constructors are not bound here; they are referenced via @make_%@ in
@irApplyConstructor@.
-}
objectGlobalBinding :: Object Type -> Maybe (Name, IROperand)
objectGlobalBinding =
  \case
    DFunction _ name lls expr ->
      let tfun = TFun (irTypeRep (typeOf expr)) ((irValueTypeRep . typeOf) <$> lls)
       in Just (name, OGlobal tfun name)
    DConstant name (ELit prim)
      | Just (irt, _) <- primToIRConstant prim ->
          Just (name, OGlobal irt name)
    DConstant name _ ->
      Just (name, OGlobal (TFun TPtr []) ("force#_" <> name))
    DExternal name t ->
      case irTypeRep t of
        tfun@TFun{} ->
          Just (name, OGlobal tfun name)
        _ ->
          Nothing
    DData{} ->
      Nothing

{- | Build the constructor tag map (constructor name → tag index) across all
modules. Used once per compilation to seed the code generator's tag
environment, rather than being recomputed for every module (which would be
quadratic in the number of modules).
-}
buildConstructorTagEnv :: [Module Type] -> Map Name Int
buildConstructorTagEnv allModules =
  Map.fromList
    [ (ctorName, idx)
    | Module{moduleObjects = objs} <- allModules
    , DData _ ctors <- objs
    , (idx, (ctorName, _)) <- zip [0 ..] ctors
    ]

{- | Build the constructor field-count map (constructor name → field count)
across all modules. Used to emit sized struct declarations for constructors
imported from other modules.
-}
buildConstructorFieldCounts :: [Module Type] -> Map Name Int
buildConstructorFieldCounts allModules =
  Map.fromList
    [ (ctorName, arity ctorType)
    | Module{moduleObjects = objs} <- allModules
    , DData _ ctors <- objs
    , (ctorName, ctorType) <- ctors
    ]

{- | Build the full object interface index (object name → 'ObjectInterface')
across all modules. Used to resolve imported functions and constants in
constant time per import, rather than scanning every module.
-}
buildModuleObjectIndex :: [Module Type] -> Map Name ObjectInterface
buildModuleObjectIndex allModules =
  Map.fromList (concatMap (Map.toList . moduleInterface) allModules)

{- | Collect free variable references from the body of an object as (name,
type) pairs.

Delegates to 'freeVars' from "Coal.Kernel.FreeVars"; parameters of a
'DFunction' are excluded from the result.
-}
objectExprVarRefs :: Object Type -> [(Name, Type)]
objectExprVarRefs = \case
  DFunction _ _ lls expr ->
    let paramNames = Set.fromList [n | Label _ n <- lls]
     in [(n, t) | Label t n <- Set.toList (freeVars expr), Set.notMember n paramNames]
  DConstant _ expr ->
    [(n, t) | Label t n <- Set.toList (freeVars expr)]
  _ -> []

{- | Resolve the imported function/constant bindings for the given import list
using a precomputed object interface index (built once per compilation by
'buildModuleObjectIndex'), returning
@(constantBindings, functionBindings, functionArities)@.

The reconstruction matches 'objectGlobalBinding' exactly: functions bind to
@OGlobal (TFun resultIRType paramIRTypes) name@; directly-representable literal
constants bind to their global IR type; every other constant is a thunk bound to
@force#_name@.
-}
collectImportedBindings ::
  Map Name ObjectInterface ->
  [Name] ->
  ([(Name, IROperand)], [(Name, IROperand)], [(Name, Int)])
collectImportedBindings objs = foldr step ([], [], [])
 where
  step name acc@(consts, fns, arities) =
    case Map.lookup name objs of
      Nothing ->
        acc
      Just (IFunction params rty) ->
        let op = OGlobal (TFun (irTypeRep rty) (irValueTypeRep <$> params)) name
         in (consts, (name, op) : fns, (name, length params) : arities)
      Just (IConstant (Just prim)) ->
        case primToIRConstant prim of
          Just (irt, _) ->
            ((name, OGlobal irt name) : consts, fns, arities)
          Nothing ->
            ((name, thunk name) : consts, fns, arities)
      Just (IConstant Nothing) ->
        ((name, thunk name) : consts, fns, arities)

  thunk n = OGlobal (TFun TPtr []) ("force#_" <> n)
