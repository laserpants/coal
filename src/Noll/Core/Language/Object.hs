{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Noll.Core.Language.Object (Object (..), ObjectList, objectName) where

import Noll.AST.HasFree (HasFree (..), boundIn, exceptNames)
import Noll.Core.Language.Expr (Expr)
import Noll.Core.Language.Type (Type (..))
import Noll.Label (Label (..))
import Noll.Utils (Name)

data Object t e
  = OFunction Name [Label t] e
  | OConstant Name e
  | OExternal Name t
  deriving (Show, Eq, Ord, Functor, Foldable, Traversable)

instance (Ord t, HasFree e t) => HasFree (Object t e) t where
  freeIn =
    \case
      OFunction _ lls e ->
        freeIn e `exceptNames` boundIn lls
      OConstant _ e ->
        freeIn e
      OExternal{} ->
        mempty

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
