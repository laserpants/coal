{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Compiler.Transform.ExpressionSpec where

import Control.Monad.Identity (runIdentity)
import Noll.Common.List1 (NonEmpty (..))
import Noll.Compiler.Transform.Expression
import Noll.Label (Label (..))
import Noll.Language (Expression (..), Pattern (..), Primitive (..))
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "Noll.Compiler.Transform.Expression" $ do
    describe "" $ do
      it "" $ do
        mapOverExpression fun fixture1 == fixture2
      it "" $ do
        runIdentity (overExpression (pure . fun) fixture1) == fixture2
      it "" $ do
        runIdentity (overExpression (pure . fun) (Just fixture1)) == Just fixture2
      it "" $ do
        runIdentity (overExpression (pure . fun) [Just fixture1, Just fixture1]) == [Just fixture2, Just fixture2]

fixture1 :: Expression () ()
fixture1 =
  EIf
    ()
    ()
    (ELiteral () (LInt32 1))
    (ELiteral () (LInt32 2))
    ( ELambda
        ()
        ( PVariable () (Label () "x") :| []
        )
        (ELiteral () (LInt32 3))
    )

fixture2 :: Expression () ()
fixture2 =
  EIf
    ()
    ()
    (ELiteral () (LInt32 2))
    (ELiteral () (LInt32 3))
    ( ELambda
        ()
        ( PVariable () (Label () "x") :| []
        )
        (ELiteral () (LInt32 4))
    )

fun :: Expression () () -> Expression () ()
fun =
  \case
    ELiteral a (LInt32 n) ->
      ELiteral a (LInt32 (n + 1))
    e ->
      e
