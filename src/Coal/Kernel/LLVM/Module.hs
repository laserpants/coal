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
  collectImportedDData,
  collectImportedFunctions,
  objectGlobalBinding,
  objectExprVarRefs,
) where

import qualified Data.Set as Set

import LLVM.IR

import Coal.Kernel.FreeVars (freeVars)
import Coal.Kernel.LLVM.Boxing (irTypeRep, irValueTypeRep)
import Coal.Kernel.LLVM.Prim (primToIRConstant)
import Coal.Kernel.Language.Expr (Expr (..), Label (..))
import Coal.Kernel.Language.Module (Module (..))
import Coal.Kernel.Language.Object (Object (..))
import Coal.Kernel.Language.Type (Type)
import Coal.Kernel.Language.Type.Function (arity)
import Coal.Kernel.Language.Type.HasType (HasType (typeOf))
import Common (Name)

{- | Compute the global environment binding for an object, if any.

'DData' constructors are not bound here; they are referenced via @make_%@ in
@irApplyConstructor@.
-}
objectGlobalBinding :: Object Type -> Maybe (Name, IROperand)
objectGlobalBinding =
  \case
    DFunction name lls expr ->
      let tfun = TFun (irTypeRep (typeOf expr)) (map (irValueTypeRep . typeOf) lls)
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
  , DFunction name params _ <- moduleObjects
  , name == importName
  ]

{- | Collect free variable references from the body of an object as (name,
type) pairs.

Delegates to 'freeVars' from "Coal.Kernel.FreeVars"; parameters of a
'DFunction' are excluded from the result.
-}
objectExprVarRefs :: Object Type -> [(Name, Type)]
objectExprVarRefs = \case
  DFunction _ lls expr ->
    let paramNames = Set.fromList [n | Label _ n <- lls]
     in [(n, t) | Label t n <- Set.toList (freeVars expr), Set.notMember n paramNames]
  DConstant _ expr ->
    [(n, t) | Label t n <- Set.toList (freeVars expr)]
  _ -> []
