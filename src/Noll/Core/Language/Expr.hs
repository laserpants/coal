{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Noll.Core.Language.Expr (ExprF (..), Expr, Focus (..), Clause (..)) where

import Data.Eq.Deriving (deriveEq1)
import Data.Fix (Fix (..))
import Data.Functor.Foldable (cata)
import Data.Set (singleton)
import Noll.AST.HasFree (HasBound (..), HasFree (..), exceptNames)
import Noll.Common.List1 (List1, NonEmpty (..))
import Noll.Core.Language.Op (Op (..))
import Noll.Core.Language.Prim (Prim (..))
import Noll.Label (Label (..))
import Noll.Utils (Name)
import Text.Show.Deriving (deriveShow1)

import qualified Data.Set as Set

-- | Pattern matching clause
data Clause t a = Clause (List1 (Label t)) a
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

deriveShow1 ''Clause
deriveEq1 ''Clause

instance (HasFree a t) => HasFree (Clause t a) t where
  freeIn (Clause (_ :| lls) e1) = freeIn e1 `exceptNames` boundIn lls

instance (HasFree (Label t) t) where
  freeIn = Set.singleton

-- | Field selector
data Focus t = Focus Name (Label t) (Label t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

instance (HasBound (Focus t)) where
  boundIn (Focus _ (Label _ name) _) = Set.singleton name

-- | Parameterized (non-recursive) expression grammar
data ExprF t a
  = -- | Variable
    EVar (Label t)
  | -- | Let-binding
    ELet (List1 (Label t, a)) a
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
  | -- | Pattern match statement
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
        ELet lls e ->
          error "ELet"
        ENil ->
          mempty
