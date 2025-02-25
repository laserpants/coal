{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Noll.Core.Language.Expr (
  ExprF (..),
  Expr,
  Focus (..),
  Clause (..),
  Binding (..),
  bindingLabel,
  bindingExpr,
  overBindingLabel,
  overBindingExpr,
  unzipBindings,
) where

import Data.Eq.Deriving (deriveEq1)
import Data.Fix (Fix (..))
import Data.Functor.Foldable (cata)
import Data.Set (singleton)
import Noll.AST.HasFree (HasBound (..), HasFree (..), exceptNames)
import Noll.Common.List1 (List1, NonEmpty (..))
import Noll.Core.Language.Op (Op (..))
import Noll.Core.Language.Prim (Prim (..))
import Noll.Label (Label (..))
import Noll.Utils (Name, Over)
import Text.Show.Deriving (deriveShow1)

import qualified Data.Set as Set
import qualified Noll.Common.List1 as List1

-- | Pattern matching clause
data Clause t a = Clause (List1 (Label t)) a
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

deriveShow1 ''Clause
deriveEq1 ''Clause

instance (HasFree a t) => HasFree (Clause t a) t where
  freeIn (Clause (_ :| lls) e1) = freeIn e1 `exceptNames` boundIn lls

-- | Field selector
data Focus t = Focus Name (Label t) (Label t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

instance (HasBound (Focus t)) where
  boundIn (Focus _ ll1 ll2) = boundIn ll1 <> boundIn ll2

data Binding t a = Binding (Label t) a
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

{-# INLINE bindingLabel #-}
bindingLabel :: Binding t a -> Label t
bindingLabel (Binding label _) = label

{-# INLINE bindingExpr #-}
bindingExpr :: Binding t a -> a
bindingExpr (Binding _ e) = e

{-# INLINE overBindingLabel #-}
overBindingLabel :: (Label s -> Label t) -> Binding s a -> Binding t a
overBindingLabel f (Binding ll e) = Binding (f ll) e

{-# INLINE overBindingExpr #-}
overBindingExpr :: Over (Binding t a) a
overBindingExpr f (Binding ll e) = Binding ll (f e)

{-# INLINE unpackBinding #-}
unpackBinding :: Binding t a -> (Label t, a)
unpackBinding (Binding ll e) = (ll, e)

{-# INLINE unzipBindings #-}
unzipBindings :: List1 (Binding t a) -> (List1 (Label t), List1 a)
unzipBindings = List1.unzip . fmap unpackBinding

deriveShow1 ''Binding
deriveEq1 ''Binding

instance (HasFree a t) => HasFree (Binding t a) t where
  freeIn (Binding _ e) = freeIn e

instance HasBound (Binding t a) where
  boundIn (Binding ll _) = boundIn ll

-- | Parameterized (non-recursive) expression grammar
data ExprF t a
  = -- | Variable
    EVar (Label t)
  | -- | Let-binding
    ELet (List1 (Binding t a)) a
  | -- | Literal value
    ELit Prim
  | -- | Lambda abstraction
    ELam (List1 (Label t)) a
  | -- | Function application
    EApp t a (List1 a)
  | -- | If-statement
    EIf a a a
  | -- | Operator
    EOp (Op a)
  | -- | Pattern matching expression
    EMat t a (List1 (Clause t a))
  | -- | Record field extension
    EExt (Label t) a a
  | -- | Empty record
    ENil
  | -- | Field selection operator
    ESel (Focus t) a a
  | -- | External C function call
    ECall (Label t) [a] a
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

-- | Main expression tree grammar
type Expr t = Fix (ExprF t)

deriveShow1 ''ExprF
deriveEq1 ''ExprF

instance (Ord t) => HasFree (Expr t) t where
  freeIn =
    cata $
      \case
        EVar ll ->
          singleton ll
        EApp _ e es ->
          e <> Set.unions es
        ELit{} ->
          mempty
        EIf e1 e2 e3 ->
          e1 <> e2 <> e3
        ECall _ es e ->
          Set.unions es <> e
        EOp op ->
          freeIn op
        EMat _ e cs ->
          e <> freeIn cs
        EExt _ e1 e2 ->
          e1 <> e2
        ESel f e1 e2 ->
          e1 <> (e2 `exceptNames` boundIn f)
        ELet vs e ->
          (freeIn vs <> e) `exceptNames` boundIn vs
        ENil ->
          mempty
        ELam vs e ->
          e `exceptNames` boundIn vs
