{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StrictData #-}

module Noll.TypeSystem.Unifier (Unifier (..), runUnifier, evalUnifier) where

import Control.Monad.State (MonadState, State, runState)

newtype Unifier a = Unifier {unifierState :: State Int a}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState Int
    )

{-# INLINE runUnifier #-}
runUnifier :: Int -> Unifier a -> (a, Int)
runUnifier n u = runState (unifierState u) n

{-# INLINE evalUnifier #-}
evalUnifier :: Int -> Unifier a -> a
evalUnifier n u = fst (runUnifier n u)
