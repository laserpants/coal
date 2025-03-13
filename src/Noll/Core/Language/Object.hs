{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Noll.Core.Language.Object (Object (..), ObjectList, objectName) where

import Noll.AST.FreeVars (FreeVars (..), boundIn, exceptNames)
import Noll.Core.LLVM.IRType (IRTyped (..))
import Noll.Core.LLVM.IRType.Syntax (opaqueFunction)
import Noll.Core.Language.Expr (Expr)
import Noll.Core.Language.Type (Type (..))
import Noll.Label (Label (..))
import Noll.Utils (Name)

data Object t e
  = OFunction Name [Label t] e
  | OConstant Name e
  | OExternal Name t
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance (Ord t, FreeVars e t) => FreeVars (Object t e) t where
  freeIn =
    \case
      OFunction _ lls e ->
        freeIn e `exceptNames` boundIn lls
      OConstant _ e ->
        freeIn e
      OExternal{} ->
        mempty

instance (IRTyped e) => IRTyped (Object t e) where
  irTypeOf =
    \case
      OFunction _ lls _ ->
        opaqueFunction (length lls)
      OConstant _ e ->
        irTypeOf e
      OExternal{} ->
        error "TODO"

objectName :: Object t e -> Name
objectName =
  \case
    OFunction name _ _ ->
      name
    OConstant name _ ->
      name
    OExternal name _ ->
      name

type ObjectList = [Object Type (Expr Type)]
