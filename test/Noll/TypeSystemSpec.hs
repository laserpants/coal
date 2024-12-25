{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.TypeSystemSpec where

import Data.List.NonEmpty (NonEmpty (..))
import Noll.Label (Label (..))
import Noll.Language (Expression)
import qualified Noll.Language.Expression as Expr
import qualified Noll.Language.Expression.Binding as Binding
import qualified Noll.Language.Pattern as Pattern
import qualified Noll.Language.Primitive as Prim
import qualified Noll.Language.Type as Type
import qualified Noll.Language.Type.Intrinsic as Intrinsic
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec =
  describe "Noll.TypeSystem" $ do
    it "" $
      1 == 2

-- fn(m) => let y = m in let x = y(true) in x
fixture_1 :: Expression ()
fixture_1 =
  Expr.Lambda
    (Pattern.Variable (Label () "m") :| [])
    ( Expr.Let
        ( Binding.Pattern
            (Pattern.Variable (Label () "y"))
            (Expr.Variable (Label () "m"))
            :| []
        )
        ( Expr.Let
            ( Binding.Pattern
                (Pattern.Variable (Label () "x"))
                ( Expr.Application
                    ()
                    (Expr.Variable (Label () "y"))
                    (Expr.Literal (Prim.Bool True) :| [])
                )
                :| []
            )
            ( Expr.Variable (Label () "x")
            )
        )
    )
