{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE StrictData #-}

module Noll.Core.Language.Expr where

import Data.Fix (Fix (..))
import Noll.Common.List1 (List1)
import Noll.Label (Label (..))
import Noll.Utils (Name)

-- | Parameterized (non-recursive) expression grammar
data ExprF e t a
  = -- | Variable
    EVar (Label t)
  | -- | Let-binding
    ELet (List1 (Label t, a)) a
  | --  | -- | Literal value
    --    ELit Prim

    -- | Lambda abstraction
    ELam (List1 (Label t)) a
  | -- | If-statement
    EIf a a a
  | -- | Function application
    EApp t a (List1 a)
  | --  | -- | Unary operator
    --    EOp1 (t, Op1) a
    --  | -- | Binary operator
    --    EOp2 (t, Op2) a a
    --  | -- | Pattern match statement
    --    EPat a (List' (Clause e t a))

    -- | Record field extension
    EExt Name a a
  | -- | Empty record
    ENil
  | --  | -- | Field selection operator
    --    ESel (Focus e t) a a

    -- | External C function call
    ECall (Label t) [a] a
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

-- | Main expression tree grammar
type Expr e t = Fix (ExprF e t)
