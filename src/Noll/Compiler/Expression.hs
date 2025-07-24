module Noll.Compiler.Expression (
  matchE,
  varE,
  letE,
  applicationE,
  lambdaE,
  lambda1E,
  lambdaAnyE,
) where

import Lang.Common.List1 (List1, NonEmpty (..))
import Lang.Label (Label (..))
import Lang.Utils (Name)
import Noll.Language

{-# INLINE matchE #-}
matchE :: (Monoid a) => Expression a () -> List1 (Clause a ()) -> Expression a ()
matchE = EMatch mempty ()

{-# INLINE varE #-}
varE :: (Monoid a) => Name -> Expression a ()
varE = EVariable mempty . label

{-# INLINE letE #-}
letE :: (Monoid a) => Name -> Expression a () -> Expression a () -> Expression a ()
letE name = ERecursiveLet mempty (PVariable mempty (label name))

{-# INLINE applicationE #-}
applicationE :: (Monoid a) => Expression a () -> List1 (Expression a ()) -> Expression a ()
applicationE = EApplication mempty ()

{-# INLINE lambdaE #-}
lambdaE :: (Monoid a) => List1 (Pattern a ()) -> Expression a () -> Expression a ()
lambdaE = ELambda mempty

{-# INLINE lambda1E #-}
lambda1E :: (Monoid a) => Name -> Expression a () -> Expression a ()
lambda1E var = ELambda mempty (PVariable mempty (label var) :| [])

{-# INLINE lambdaAnyE #-}
lambdaAnyE :: (Monoid a) => Expression a () -> Expression a ()
lambdaAnyE = ELambda mempty (PAny mempty () :| [])

{-# INLINE label #-}
label :: Name -> Label ()
label = Label ()
