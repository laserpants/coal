{-# LANGUAGE OverloadedStrings #-}

module Noll.Compiler.CoreSpec where

import Control.Monad.Identity (runIdentity)
import Control.Monad.State (evalState)
import Noll.Common.List1 (NonEmpty (..), (<|))
import Noll.Compiler.Core
import Noll.Label (Label (..))
import Test.Hspec (Spec, describe, it)

import qualified Noll.Core.Language as Core

spec :: Spec
spec =
  describe "Noll.Compiler.Core" $ do
    describe "" $ do
      it "" $ do
        evalState (transSuffixExpr fixture1) 0
          == Core.let_
            undefined
            undefined

fixture1 :: Core.Expr ()
fixture1 =
  Core.let_
    ( ( Label () "a"
      , Core.var (Label () "b")
      )
        :| []
    )
    (Core.var (Label () "c"))

fixture2 :: Core.Expr ()
fixture2 =
  Core.lam
    (Label () "a" :| [])
    (Core.var (Label () "a"))
