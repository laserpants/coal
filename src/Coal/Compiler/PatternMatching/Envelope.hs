{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Coal.Compiler.PatternMatching.Envelope (
  EnvelopeExpression (..),
  EnvelopePattern (..),
  EnvelopeHost (..),
  EnvelopeClause (..),
  fails,
) where

import Coal.Common.Label (Label (..))
import Coal.Compiler.Transform.Tree (rename)
import Coal.Language (Expression (..), HasType (..))
import Data.Data (Data)
import Extras (Name)

data EnvelopeClause e t = EnvelopeClause (Label t) [Label t] (EnvelopeExpression e t)
  deriving (Show, Eq, Ord, Read)

data EnvelopePattern e t
  = MConstructor (Label t) [EnvelopePattern e t]
  | MVariable (Label t)
  | MLiteral t (e t)
  deriving (Show, Eq, Ord, Read)

data EnvelopeExpression e t
  = MFail
  | MExpression (e t)
  | MCase (Label t) [EnvelopeClause e t]
  | MConditional (Label t) (e t) (EnvelopeExpression e t) (EnvelopeExpression e t)
  deriving (Show, Eq, Ord, Read)

class EnvelopeHost e t where
  replace :: Name -> Name -> e t -> e t

instance (Ord t, Data a, Data t) => EnvelopeHost (Expression a) t where
  replace = rename

instance (EnvelopeHost a t) => EnvelopeHost (EnvelopeClause a) t where
  replace var new (EnvelopeClause l1 ls e) =
    EnvelopeClause l1 ls (replace var new e)

instance (EnvelopeHost a t) => EnvelopeHost (EnvelopeExpression a) t where
  replace _ _ MFail =
    MFail
  replace var new (MExpression e) =
    MExpression (replace var new e)
  replace _ _ _ =
    error "Implementation error"

instance (HasType o k (e t)) => HasType o k (EnvelopeClause e t) where
  typeOf =
    \case
      EnvelopeClause _ _ t ->
        typeOf t

instance (HasType o k (e t)) => HasType o k (EnvelopeExpression e t) where
  typeOf =
    \case
      MFail ->
        error "MFail"
      MExpression t ->
        typeOf t
      MCase _ [] ->
        error "Implementation error"
      MCase _ (t : _) ->
        typeOf t
      MConditional _ _ t _ ->
        typeOf t

{-# INLINE fails #-}
fails :: EnvelopeClause e t -> Bool
fails (EnvelopeClause _ _ MFail) = True
fails EnvelopeClause{} = False
