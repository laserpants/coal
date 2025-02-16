{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Core.Language.Expr (ExprF (..), Expr, Clause (..)) where

import Data.Fix (Fix (..))
import Noll.Common.List1 (List1)
import Noll.Core.Language.Op (Op (..))
import Noll.Core.Language.Prim (Prim (..))
import Noll.Label (Label (..))
import Noll.Utils (Name)

-- | Pattern matching clause
data Clause t a = Clause (List1 (Label t)) a
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

-- | Field selector
data Focus t = Focus Name (Label t) (Label t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

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
    EIf t a a a
  | -- | Operator
    EOp t (Op a)
  | -- | Pattern match statement
    EPat t a (List1 (Clause t a))
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
