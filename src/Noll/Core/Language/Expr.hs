{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Noll.Core.Language.Expr (ExprF (..), Expr, Focus (..), Clause (..)) where

import Data.Eq.Deriving (deriveEq1)
import Data.Fix (Fix (..))
import Noll.Common.List1 (List1)
import Noll.Core.Language.Op (Op (..))
import Noll.Core.Language.Prim (Prim (..))
import Noll.Label (Label (..))
import Noll.Utils (Name)
import Text.Show.Deriving (deriveShow1)

-- | Pattern matching clause
data Clause t a = Clause (List1 (Label t)) a
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

deriveShow1 ''Clause
deriveEq1 ''Clause

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
