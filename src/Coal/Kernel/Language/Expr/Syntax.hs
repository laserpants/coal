module Coal.Kernel.Language.Expr.Syntax (
  var,
  let_,
  lam,
  app,
  match,
  if_,
  op,
  lit,
  sel,
  ext,
  nil,
  call,
  mem,
) where

import Coal.Common.Label (Label (..))
import Coal.Common.List1 (List1)
import Coal.Kernel.Language.Expr (
  Binding (..),
  Clause (..),
  Expr,
  ExprF (..),
  Focus (..),
 )
import Coal.Kernel.Language.Op (Op)
import Coal.Kernel.Language.Prim (Prim)
import Extra (Name)
import Extra.Data.Functor.Foldable (embed, embed1, embed2, embed3)

{-# INLINE var #-}
var :: Label t -> Expr t
var = embed1 EVar

{-# INLINE let_ #-}
let_ :: List1 (Binding t (Expr t)) -> Expr t -> Expr t
let_ = embed2 ELet

{-# INLINE lit #-}
lit :: Prim -> Expr t
lit = embed1 ELit

{-# INLINE if_ #-}
if_ :: Expr t -> Expr t -> Expr t -> Expr t
if_ = embed3 EIf

{-# INLINE match #-}
match :: t -> Expr t -> List1 (Clause t (Expr t)) -> Expr t
match = embed3 EMat

{-# INLINE ext #-}
ext :: Name -> Expr t -> Expr t -> Expr t
ext = embed3 EExt

{-# INLINE nil #-}
nil :: Expr t
nil = embed ENil

{-# INLINE sel #-}
sel :: Focus t -> Expr t -> Expr t -> Expr t
sel = embed3 ESel

{-# INLINE call #-}
call :: Label t -> [Expr t] -> Expr t -> Expr t
call = embed3 ECall

{-# INLINE op #-}
op :: Op (Expr t) -> Expr t
op = embed1 EOp

{-# INLINE lam #-}
lam :: List1 (Label t) -> Expr t -> Expr t
lam = embed2 ELam

{-# INLINE app #-}
app :: t -> Expr t -> List1 (Expr t) -> Expr t
app = embed3 EApp

{-# INLINE mem #-}
mem :: Expr t -> Expr t
mem = embed1 EMem
