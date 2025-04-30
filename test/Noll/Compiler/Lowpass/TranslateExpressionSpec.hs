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
  ( ELambda
      ()
      ( PVariable () (Label (TVariable (TypeIndex KType 0)) "x")
          <| PVariable () (Label (TVariable (TypeIndex KType 0)) "y")
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
                  ( (TVariable (TypeIndex KType 0))
                      `TArrow` (TVariable (TypeIndex KType 0))
                      `TArrow` TIntrinsic IBool
                  )
                  OLessThan
              )
              ( EVariable () (Label (TVariable (TypeIndex KType 0)) "x")
                  <| EVariable () (Label (TVariable (TypeIndex KType 0)) "y")
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
                      ( (TVariable (TypeIndex KType 0))
                          `TArrow` (TVariable (TypeIndex KType 0))
                          `TArrow` TIntrinsic IBool
                      )
                      OGreaterThan
                  )
                  ( EVariable () (Label (TVariable (TypeIndex KType 0)) "x")
                      <| EVariable () (Label (TVariable (TypeIndex KType 0)) "y")
                      :| []
                  )
              )
              (EConstructor () (Label (TConstructor KType "Ordering") "GreaterThan"))
              (EConstructor () (Label (TConstructor KType "Ordering") "EqualTo"))
          )
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
