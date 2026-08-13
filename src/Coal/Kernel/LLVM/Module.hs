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
  collectCachedImports,
  collectImportedConstants,
  collectImportedDData,
  collectImportedFunctionBindings,
  collectImportedFunctions,
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
import Coal.Kernel.Language.Interface (ObjectInterface (..))
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

{- | Search all modules for 'DData' objects whose name appears in the given
import list, returning (constructorName, fieldCount) pairs.
-}
collectImportedDData :: [Module Type] -> [Name] -> [(Name, Int)]
collectImportedDData allModules importNames =
  [ (ctorName, arity ctorType)
  | importName <- importNames
  , Module{moduleObjects} <- allModules
  , DData _ ctors <- moduleObjects
  , (ctorName, ctorType) <- ctors
  , ctorName == importName
  ]

{- | Search all modules for 'DFunction' objects whose name appears in the given
import list, returning (functionName, arity) pairs.
-}
collectImportedFunctions :: [Module Type] -> [Name] -> [(Name, Int)]
collectImportedFunctions allModules importNames =
  [ (name, length params)
  | importName <- importNames
  , Module{moduleObjects} <- allModules
  , DFunction _ name params _ <- moduleObjects
  , name == importName
  ]

{- | Search all modules for 'DFunction' objects whose name appears in the given
import list, returning @(name, operand)@ pairs with the exact IR type derived
from the function's definition — the same way 'objectGlobalBinding' handles
module-local functions.

Used to pre-populate the codegen environment so that 'nameLookup' uses the
declared parameter count rather than the (potentially wrong) arity derived
from usage-site type annotations.
-}
collectImportedFunctionBindings :: [Module Type] -> [Name] -> [(Name, IROperand)]
collectImportedFunctionBindings allModules importNames =
  [ (name, op)
  | importName <- importNames
  , Module{moduleObjects} <- allModules
  , obj@(DFunction _ name _ _) <- moduleObjects
  , name == importName
  , Just (_, op) <- [objectGlobalBinding obj]
  ]

{- | Search all modules for 'DConstant' objects whose name appears in the given
import list, returning (name, operand) pairs suitable for insertion into the
codegen variable environment.

String and bignum constants are thunks (@force#_\<name\>@; arity 0 functions);
directly representable constants (int32, bool, etc.) are global references.
-}
collectImportedConstants :: [Module Type] -> [Name] -> [(Name, IROperand)]
collectImportedConstants allModules importNames =
  [ (name, op)
  | importName <- importNames
  , Module{moduleObjects} <- allModules
  , obj@(DConstant name _) <- moduleObjects
  , name == importName
  , Just (_, op) <- [objectGlobalBinding obj]
  ]

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

{- | Reconstruct the imported function/constant bindings for names that live in
cached ('BCached') modules rather than in @allModules@.

Returns @(constantBindings, functionBindings, functionArities)@, mirroring what
'collectImportedConstants', 'collectImportedFunctionBindings', and
'collectImportedFunctions' produce for source modules, so 'irModule' can treat
cached and source imports uniformly.

The reconstruction matches 'objectGlobalBinding' exactly: functions bind to
@OGlobal (TFun resultIRType paramIRTypes) name@; directly-representable literal
constants bind to their global IR type; every other constant is a thunk bound to
@force#_name@.
-}
collectCachedImports ::
  Map Name ObjectInterface ->
  [Name] ->
  ([(Name, IROperand)], [(Name, IROperand)], [(Name, Int)])
collectCachedImports objs = foldr step ([], [], [])
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
