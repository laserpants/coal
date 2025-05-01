{-# LANGUAGE OverloadedStrings #-}

module Noll.Compiler.Lowpass.TranslateExpressionSpec where

import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Noll.Compiler.Lowpass.TranslateExpression
import Noll.Language
import Noll.Module (Constant (..), Definition (..), Function (..), Module (..), Path (..))

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Lang.Lowpass.Language as LP
import qualified Noll.Module as Module

foobaz1 =
  ELambda
    ()
    ( PVariable () (Label (TIntrinsic IInt32) "x")
        <| PVariable () (Label (TIntrinsic IInt32) "y")
        :| []
    )
    ( EIf
        ()
        (TConstructor KType "Ordering")
        ( EApplication
            ()
            (TIntrinsic IBool)
            ( EBinaryOperator
                ()
                (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IBool)
                OLessThan
            )
            ( EVariable () (Label (TIntrinsic IInt32) "x")
                <| EVariable () (Label (TIntrinsic IInt32) "y")
                :| []
            )
        )
        (EConstructor () (Label (TConstructor KType "Ordering") "LessThan"))
        ( EIf
            ()
            (TConstructor KType "Ordering")
            ( EApplication
                ()
                (TIntrinsic IBool)
                ( EBinaryOperator
                    ()
                    (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IBool)
                    OGreaterThan
                )
                ( EVariable () (Label (TIntrinsic IInt32) "x")
                    <| EVariable () (Label (TIntrinsic IInt32) "y")
                    :| []
                )
            )
            (EConstructor () (Label (TConstructor KType "Ordering") "GreaterThan"))
            (EConstructor () (Label (TConstructor KType "Ordering") "EqualTo"))
        )
    )

foobaz1r =
  LP.lam
    (Label LP.TOpq "x" <| Label LP.TOpq "y" :| [])
    ( LP.if_
        undefined
        (LP.var (Label (LP.TCon "Ordering" []) "LessThan"))
        ( LP.if_
            undefined
            (LP.var (Label (LP.TCon "Ordering" []) "GreaterThan"))
            (LP.var (Label (LP.TCon "Ordering" []) "EqualTo"))
        )
    )
