{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Noll.Language.HasTag (HasTag (..)) where

import Noll.Language.Expression (Expression (..))
import Noll.Language.Pattern (Pattern (..))

class HasTag t a where
  tag :: t -> a

instance HasTag (Expression a t) a where
  tag =
    \case
      EAnnotation a _ _ ->
        a
      EApplication a _ _ _ ->
        a
      ELambda a _ _ ->
        a
      ELet a _ _ ->
        a
      ERecursiveLet a _ _ _ ->
        a
      EVariable a _ ->
        a
      EConstructor a _ ->
        a
      ELiteral a _ ->
        a
      EIf a _ _ _ _ ->
        a
      EUnaryOperator a _ ->
        a
      EBinaryOperator a _ ->
        a
      ERecord a _ _ _ ->
        a
      EListCons a _ _ _ ->
        a
      EListLiteral a _ _ ->
        a
      EMatch a _ _ _ ->
        a
      EFold a _ _ _ _ ->
        a
      ESelect a _ _ ->
        a

instance HasTag (Pattern a t) a where
  tag =
    \case
      PAnnotation a _ _ ->
        a
      PAny a _ ->
        a
      PVariable a _ ->
        a
      PConstructor a _ _ ->
        a
      PLiteral a _ ->
        a
      PRecord a _ _ _ ->
        a
      PListCons a _ _ _ ->
        a
      PListLiteral a _ _ ->
        a
      POr a _ _ _ ->
        a
      PShorthand a _ ->
        a
      PAtVariable a _ ->
        a
