module Noll.Core.Language.Expr.Syntax (
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
) where

import Noll.Common.List1 (List1)
import Noll.Core.Language.Expr (
  Binding (..),
  Clause (..),
  Expr,
  ExprF (..),
  Focus (..),
 )
import Noll.Core.Language.Op (Op)
import Noll.Core.Language.Prim (Prim)
import Noll.Label (Label (..))
import Noll.Utils.Embed

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
ext :: Label t -> Expr t -> Expr t -> Expr t
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
