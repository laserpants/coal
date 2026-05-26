{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Coal.Kernel.Language.Object (
  Object (..),
  ObjectList,
  KernelObject,
  objectName,
  fromBinding,
  objectIsFunction,
  objectIsConstant,
  objectConstructorInfo,
) where

import Coal.Common.FreeVars (FreeVars (..), boundIn, exceptNames)
import Coal.Common.Label (Label (..))
import Coal.Kernel.LLVM.IRType (IRTyped (..))
import Coal.Kernel.Language.Expr (Binding (..), Expr, ExprF (..))
import Coal.Kernel.Language.Type (Type (..))
import Coal.Kernel.Language.Type.Arrow (functionTypeOf)
import Coal.Kernel.Language.Typed (Typed (..))
import Control.Arrow ((>>>))
import Data.Functor.Foldable (embed, project)
import Data.List.NonEmpty (toList)
import Extras (Name)

data Object t e
  = OFunction Name [Label t] e
  | OConstant Name e
  | OExternal Name t
  | OData Name Int t
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

type KernelObject = Object Type (Expr Type)

instance (Ord t, FreeVars e t) => FreeVars (Object t e) t where
  freeIn =
    \case
      OFunction _ lls e ->
        freeIn e `exceptNames` boundIn lls
      OConstant _ e ->
        freeIn e
      OExternal{} ->
        mempty
      OData{} ->
        mempty

instance (Typed t) => Typed (Object t (Expr t)) where
  typeOf =
    \case
      OFunction _ lls e ->
        functionTypeOf e lls
      OConstant _ e ->
        typeOf e
      OExternal _ t ->
        typeOf t
      OData _ _ t ->
        typeOf t

objectName :: Object t e -> Name
objectName =
  \case
    OFunction name _ _ ->
      name
    OConstant name _ ->
      name
    OExternal name _ ->
      name
    OData name _ _ ->
      name

type ObjectList = [Object Type (Expr Type)]

fromBinding :: Binding Type (Expr Type) -> Object Type (Expr Type)
fromBinding (Binding (Label _ name) expr) = go expr
 where
  go =
    project
      >>> \case
        ELam vs e ->
          OFunction name (toList vs) e
        e ->
          OConstant name (embed e)

objectIsFunction :: Object t e -> Bool
objectIsFunction =
  \case
    OFunction{} ->
      True
    _ ->
      False

objectIsConstant :: Object t e -> Bool
objectIsConstant =
  \case
    OConstant{} ->
      True
    _ ->
      False

objectConstructorInfo :: Object t e -> [(Name, Int)]
objectConstructorInfo =
  \case
    OData name i _ ->
      [(name, i)]
    _ ->
      []
