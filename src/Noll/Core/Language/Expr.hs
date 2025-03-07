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
  overBindingLabel,
  overBindingExpr,
  unzipBindings,
) where

import Data.Eq.Deriving (deriveEq1)
import Data.Fix (Fix (..))
import Data.Functor.Foldable (cata, project)
import Data.Set (singleton)
import Noll.AST.FreeVars (BoundVars (..), FreeVars (..), exceptNames)
import Noll.Common.List1 (List1, NonEmpty (..))
import Noll.Core.Language.Op (Op (..))
import Noll.Core.Language.Prim (Prim (..))
import Noll.Core.Language.Type (Type (..), normalizeRow)
import Noll.Core.Language.Type.Arrow (foldType, returnTypeOf)
import Noll.Core.Language.Typed (Typed (..))
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

instance (FreeVars a t) => FreeVars (Clause t a) t where
  freeIn (Clause (_ :| lls) e1) = freeIn e1 `exceptNames` boundIn lls

-- | Field selector
data Focus t = Focus Name (Label t) (Label t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

instance (BoundVars (Focus t)) where
  boundIn (Focus _ ll1 ll2) = boundIn ll1 <> boundIn ll2

data Binding t a = Binding {bindingLabel :: Label t, bindingExpr :: a}
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

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

instance (FreeVars a t) => FreeVars (Binding t a) t where
  freeIn (Binding _ e) = freeIn e

instance BoundVars (Binding t a) where
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
  | -- | Operators
    EOp (Op a)
  | -- | Pattern matching expression
    EMat t a (List1 (Clause t a))
  | -- | Record field extension
    EExt (Label t) a a
  | -- | Empty record
    ENil
  | -- | Field selector
    ESel (Focus t) a a
  | -- | External C function call
    ECall (Label t) [a] a
  | -- | Memoized top-level constant
    EMem a
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

-- | Main expression tree grammar
type Expr t = Fix (ExprF t)

deriveShow1 ''ExprF
deriveEq1 ''ExprF

instance (Ord t) => FreeVars (Expr t) t where
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
        EMem e ->
          e

instance (Typed t, Typed a) => Typed (ExprF t a) where
  typeOf =
    \case
      EVar t ->
        typeOf t
      ELit t ->
        typeOf t
      ELet _ t ->
        typeOf t
      EIf _ _ t ->
        typeOf t
      EApp t _ _ ->
        typeOf t
      EMat t _ _ ->
        typeOf t
      ESel _ _ t ->
        typeOf t
      EOp op ->
        typeOf op
      ENil ->
        RNil
      EExt (Label _ n) t1 t2 ->
        normalizeRow (RExt n (typeOf t1) (typeOf t2))
      ELam ts t ->
        foldType (typeOf t) (typeOf <$> ts)
      ECall _ _ t ->
        returnTypeOf t
      EMem t ->
        typeOf t

instance (Typed t) => Typed (Expr t) where
  typeOf = typeOf . project
