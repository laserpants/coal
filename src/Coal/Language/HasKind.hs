{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

{- |
Module: Coal.Language.HasKind

Type class for extracting and folding kinds from language constructs.
-}
module Coal.Language.HasKind (HasKind (..), foldKindOf, hasKind) where

import Coal.Language.Type (Parameter (..), Type (..), TypeIndex (..))
import Coal.Language.Type.Kind (Kind (..), foldKind)
import Data.Data (Data, Typeable)
import Data.Generics.Uniplate.Data (universeBi)

{- | Safe helper to extract the first kind, with invariant checking
This should never fail for well-formed kind-indexed AST nodes
-}
safeHeadKind :: [Kind] -> Kind
safeHeadKind [] = error "Internal compiler error: kindOf called on AST node with no kind annotation"
safeHeadKind (k : _) = k

class HasKind k where
  kindOf :: k -> Kind

instance HasKind Kind where
  kindOf = id

instance HasKind (TypeIndex Kind) where
  kindOf = safeHeadKind . universeBi

instance HasKind (Parameter Kind) where
  kindOf = safeHeadKind . universeBi

instance (Data (o Kind), Typeable o) => HasKind (Type o Kind) where
  kindOf =
    \case
      TRow{} ->
        KRow
      TArrow{} ->
        KType
      TIntrinsic{} ->
        KType
      TRecord{} ->
        KType
      TAlias _ _ k ->
        kindOf k
      k ->
        safeHeadKind (universeBi k)

{-# INLINE foldKindOf #-}
foldKindOf :: (HasKind k, HasKind i, Functor f, Foldable f) => i -> f k -> Kind
foldKindOf t ts = foldKind (kindOf t) (kindOf <$> ts)

{-# INLINE hasKind #-}
hasKind :: (HasKind k) => Kind -> k -> Bool
hasKind k o = kindOf o == k
