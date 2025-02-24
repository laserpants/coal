{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.Language.ReplaceSpec where

import Noll.Common.List1 (NonEmpty (..), (<|))
import Noll.Core.Language.Replace (rewrite)
import Noll.Label (Label (..))
import Test.Hspec (Spec, describe, it, shouldBe)

import qualified Noll.Core.Language as Core

ordering :: Core.Type
ordering = Core.TCon "Ordering" []

spec :: Spec
spec =
  describe "Noll.Core.Language.Replace" $ do
    --    it "EVar" $ do
    --      error "TODO" == True
    --    it "ELet" $ do
    --      error "TODO" == True
    --    it "ELit" $ do
    --      error "TODO" == True
    --    it "ELam" $ do
    --      error "TODO" == True
    --    it "EApp" $ do
    --      error "TODO" == True
    --    it "EIf" $ do
    --      error "TODO" == True
    --    it "EOp" $ do
    --      error "TODO" == True
    --    it "EMat" $ do
    --      error "TODO" == True
    --    it "EExt" $ do
    --      error "TODO" == True
    --    it "ENil" $ do
    --      error "TODO" == True
    --    it "ESel" $ do
    --      error "TODO" == True
    --    it "ECall" $ do
    --      error "TODO" == True
    it "" $ do
      rewrite
        "y"
        "z"
        ( Core.let_
            ( Core.Binding
              ( Label () "f")
              ( Core.lam
                  (Label () "x" :| [])
                  ( Core.op
                      ( Core.OAddInt32
                          (Core.var (Label () "x"))
                          (Core.var (Label () "y"))
                      )
                  )
              )
                :| []
            )
            (Core.lit (Core.PInt32 1))
        )
        == Core.let_
          ( Core.Binding
             ( Label () "f" )
            ( Core.lam
                (Label () "x" :| [])
                ( Core.op
                    ( Core.OAddInt32
                        (Core.var (Label () "x"))
                        (Core.var (Label () "z"))
                    )
                )
            )
              :| []
          )
          (Core.lit (Core.PInt32 1))
    it "" $ do
      rewrite "x" "z" fixture1 == fixture1
    it "" $ do
      rewrite "y1" "y2" fixture2 == fixture3

fixture1 :: Core.Expr Core.Type
fixture1 =
  Core.lam
    ( Label Core.int32 "x"
        <| Label Core.int32 "y"
        :| []
    )
    ( Core.if_
        ( Core.op
            ( Core.OLtInt32
                (Core.var (Label Core.int32 "x"))
                (Core.var (Label Core.int32 "y"))
            )
        )
        (Core.var (Label ordering "LessThan"))
        ( Core.if_
            ( Core.op
                ( Core.OGtInt32
                    (Core.var (Label Core.int32 "x"))
                    (Core.var (Label Core.int32 "y"))
                )
            )
            (Core.var (Label ordering "GreaterThan"))
            (Core.var (Label ordering "EqualTo"))
        )
    )

fixture2 :: Core.Expr Core.Type
fixture2 =
  Core.lam
    ( Label Core.int32 "x"
        <| Label Core.int32 "y"
        :| []
    )
    ( Core.if_
        ( Core.op
            ( Core.OLtInt32
                (Core.var (Label Core.int32 "x"))
                (Core.var (Label Core.int32 "y1"))
            )
        )
        (Core.var (Label ordering "LessThan"))
        ( Core.if_
            ( Core.op
                ( Core.OGtInt32
                    (Core.var (Label Core.int32 "x"))
                    (Core.var (Label Core.int32 "y"))
                )
            )
            (Core.var (Label ordering "GreaterThan"))
            (Core.var (Label ordering "EqualTo"))
        )
    )

fixture3 :: Core.Expr Core.Type
fixture3 =
  Core.lam
    ( Label Core.int32 "x"
        <| Label Core.int32 "y"
        :| []
    )
    ( Core.if_
        ( Core.op
            ( Core.OLtInt32
                (Core.var (Label Core.int32 "x"))
                (Core.var (Label Core.int32 "y2"))
            )
        )
        (Core.var (Label ordering "LessThan"))
        ( Core.if_
            ( Core.op
                ( Core.OGtInt32
                    (Core.var (Label Core.int32 "x"))
                    (Core.var (Label Core.int32 "y"))
                )
            )
            (Core.var (Label ordering "GreaterThan"))
            (Core.var (Label ordering "EqualTo"))
        )
    )
