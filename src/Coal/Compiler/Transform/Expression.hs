module Coal.Compiler.Transform.Expression (
  matchE,
  varE,
  letE,
  applicationE,
  lambdaE,
  lambda1E,
  lambdaAnyE,
  selectE,
) where

import Coal.Common.Label (Label (..))
import Coal.Language
import Data.List.NonEmpty (NonEmpty (..))
import Extra (Name)

{-# INLINE matchE #-}
matchE :: (Monoid a) => Expression a () -> NonEmpty (Clause a ()) -> Expression a ()
matchE = EMatch mempty ()

{-# INLINE varE #-}
varE :: (Monoid a) => Name -> Expression a ()
varE = EVariable mempty . label

{-# INLINE letE #-}
letE :: (Monoid a) => Name -> Expression a () -> Expression a () -> Expression a ()
letE name = ERecursiveLet mempty (PVariable mempty (label name))

{-# INLINE applicationE #-}
applicationE :: (Monoid a) => Expression a () -> NonEmpty (Expression a ()) -> Expression a ()
applicationE = EApplication mempty ()

{-# INLINE lambdaE #-}
lambdaE :: (Monoid a) => NonEmpty (Pattern a ()) -> Expression a () -> Expression a ()
lambdaE = ELambda mempty

{-# INLINE lambda1E #-}
lambda1E :: (Monoid a) => Name -> Expression a () -> Expression a ()
lambda1E var = ELambda mempty (PVariable mempty (label var) :| [])

{-# INLINE lambdaAnyE #-}
lambdaAnyE :: (Monoid a) => Expression a () -> Expression a ()
lambdaAnyE = ELambda mempty (PAny mempty () :| [])

{-# INLINE selectE #-}
selectE :: (Monoid a) => Name -> Expression a () -> Expression a ()
selectE = ESelect mempty . label

{-# INLINE label #-}
label :: Name -> Label ()
label = Label ()
