{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.Unification (TypeUnifiable (..)) where

import Noll.TypeSystem.Substitution (TypeSubstitution (..))

class TypeUnifiable u k | u -> k where
  unify :: (Monad m) => u -> u -> m (TypeSubstitution k)
