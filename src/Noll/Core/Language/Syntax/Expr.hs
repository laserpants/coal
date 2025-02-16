module Noll.Core.Language.Syntax.Expr (
  var,
  let_,
  lam,
  app,
  match,
  if_,
  op,
  lit,
) where

import Noll.Common.List1 (List1)
import Noll.Core.Language.Expr (Clause (..), Expr, ExprF (..))
import Noll.Core.Language.Op (Op)
import Noll.Core.Language.Prim (Prim)
import Noll.Label (Label (..))
import Noll.Utils.Embed

{-# INLINE var #-}
var :: Label t -> Expr t
var = embed1 EVar

{-# INLINE let_ #-}
let_ :: List1 (Label t, Expr t) -> Expr t -> Expr t
let_ = embed2 ELet

{-# INLINE lit #-}
lit :: Prim -> Expr t
lit = embed1 ELit

{-# INLINE if_ #-}
if_ :: t -> Expr t -> Expr t -> Expr t -> Expr t
if_ = embed4 EIf

{-# INLINE match #-}
match :: t -> Expr t -> List1 (Clause t (Expr t)) -> Expr t
match = embed3 EPat

-- {-# INLINE ext #-}
-- ext :: Name -> Expr t -> Expr t -> Expr t
-- ext = embed3 EExt
--
-- {-# INLINE nil #-}
-- nil :: Expr t
-- nil = embed ENil
--
-- {-# INLINE sel #-}
-- sel :: Focus e t -> Expr t -> Expr t -> Expr t
-- sel = embed3 ESel
--
-- {-# INLINE call #-}
-- call :: Label e t -> [Expr t] -> Expr t -> Expr t
-- call = embed3 ECall
--
-- {-# INLINE ann #-}
-- ann :: Type -> Expr t -> Expr t
-- ann = embed2 EAnn
--
-- {-# INLINE mem #-}
-- mem :: Expr t -> Expr t
-- mem = embed1 EMem
--

{-# INLINE op #-}
op :: t -> Op (Expr t) -> Expr t
op = embed2 EOp

{-# INLINE lam #-}
lam :: List1 (Label t) -> Expr t -> Expr t
lam = embed2 ELam

{-# INLINE app #-}
app :: t -> Expr t -> List1 (Expr t) -> Expr t
app = embed3 EApp
