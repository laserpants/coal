{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TemplateHaskell #-}

module Coal.Kernel.Language.Expr (
  ExprF (..),
  Expr,
  Focus (..),
  Clause (..),
  Binding (..),
  overBindingLabel,
  overBindingExpr,
  unzipBindings,
  isPrim,
) where

import Coal.Common.FreeVars (BoundVars (..), FreeVars (..), exceptNames)
import Coal.Common.Label (Label (..))
import Coal.Kernel.Language.Op (Op (..))
import Coal.Kernel.Language.Prim (Prim (..))
import Coal.Kernel.Language.Type (Type (..))
import Coal.Kernel.Language.Type.Arrow (foldType, returnTypeOf)
import Coal.Kernel.Language.Type.Row (extend)
import Coal.Kernel.Language.Typed (Typed (..))
import Data.Data (Data)
import Data.Eq.Deriving (deriveEq1)
import Data.Fix (Fix (..))
import Data.Functor.Foldable (cata, project)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import Data.Set (singleton)
import qualified Data.Set as Set
import Extras (Name, Over)
import Text.Show.Deriving (deriveShow1)

-- | Pattern matching clause
data Clause t a = Clause (NonEmpty (Label t)) a
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

deriveShow1 ''Clause
deriveEq1 ''Clause

instance (FreeVars a t) => FreeVars (Clause t a) t where
  freeIn (Clause (_ :| lls) e1) = freeIn e1 `exceptNames` boundIn lls

-- | Field selector
data Focus t = Focus Name (Label t) (Label t)
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

instance BoundVars (Focus t) where
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
unzipBindings :: NonEmpty (Binding t a) -> (NonEmpty (Label t), NonEmpty a)
unzipBindings = NonEmpty.unzip . fmap unpackBinding

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
    ELet (NonEmpty (Binding t a)) a
  | -- | Literal value
    ELit Prim
  | -- | Lambda abstraction
    ELam (NonEmpty (Label t)) a
  | -- | Function application
    EApp t a (NonEmpty a)
  | -- | If-statement
    EIf a a a
  | -- | Operators
    EOp (Op a)
  | -- | Pattern matching expression
    EMat t a (NonEmpty (Clause t a))
  | -- | Record field extension
    EExt Name a a
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

instance (Data t, Ord t) => FreeVars (Expr t) t where
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
      EExt f t1 t2 ->
        extend f (typeOf t1) (typeOf t2)
      ELam ts t ->
        foldType (typeOf t) (typeOf <$> ts)
      ECall _ _ t ->
        returnTypeOf t
      EMem t ->
        typeOf t

instance (Typed t) => Typed (Expr t) where
  typeOf = typeOf . project

isPrim :: Expr Type -> Bool
isPrim =
  cata $
    \case
      ELit{} ->
        True
      _ ->
        False
