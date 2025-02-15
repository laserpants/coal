module Noll.Core.Language.Syntax.Expr (
  var,
  let_,
  lam,
  app,
  match,
  if_,
) where

import Noll.Common.List1 (List1)
import Noll.Core.Language.Expr (Clause (..), Expr, ExprF (..))
import Noll.Core.Language.Op (Op)
import Noll.Core.Language.Prim (Prim)
import Noll.Label (Label (..))
import Noll.Utils.Embed (embed, embed1, embed2, embed3, embed4, embed5)

{-# INLINE var #-}
var :: Label t -> Expr e t
var = embed1 EVar

{-# INLINE let_ #-}
let_ :: List1 (Label t, Expr e t) -> Expr e t -> Expr e t
let_ = embed2 ELet

{-# INLINE lit #-}
lit :: Prim -> Expr e t
lit = embed1 ELit

{-# INLINE if_ #-}
if_ :: t -> Expr e t -> Expr e t -> Expr e t -> Expr e t
if_ = embed4 EIf

{-# INLINE match #-}
match :: t -> Expr e t -> (List1 (Clause e t (Expr e t))) -> Expr e t
match = embed3 EPat

-- {-# INLINE ext #-}
-- ext :: Name -> Expr e t -> Expr e t -> Expr e t
-- ext = embed3 EExt
--
-- {-# INLINE nil #-}
-- nil :: Expr e t
-- nil = embed ENil
--
-- {-# INLINE sel #-}
-- sel :: Focus e t -> Expr e t -> Expr e t -> Expr e t
-- sel = embed3 ESel
--
-- {-# INLINE call #-}
-- call :: Label e t -> [Expr e t] -> Expr e t -> Expr e t
-- call = embed3 ECall
--
-- {-# INLINE ann #-}
-- ann :: Type -> Expr e t -> Expr e t
-- ann = embed2 EAnn
--
-- {-# INLINE mem #-}
-- mem :: Expr e t -> Expr e t
-- mem = embed1 EMem
--
-- {-# INLINE op1 #-}
-- op1 :: (t, Op1) -> Expr e t -> Expr e t
-- op1 = embed2 EOp1
--
-- {-# INLINE op2 #-}
-- op2 :: (t, Op2) -> Expr e t -> Expr e t -> Expr e t
-- op2 = embed3 EOp2

{-# INLINE op #-}
op :: t -> Op (Expr e t) -> Expr e t
op = embed2 EOp

{-# INLINE lam #-}
lam :: List1 (Label t) -> Expr e t -> Expr e t
lam = embed2 ELam

{-# INLINE app #-}
app :: t -> Expr e t -> List1 (Expr e t) -> Expr e t
app = embed3 EApp
