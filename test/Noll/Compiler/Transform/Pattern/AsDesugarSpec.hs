{-# LANGUAGE OverloadedStrings #-}

module Noll.Compiler.Transform.Pattern.AsDesugarSpec where

import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Common.Label (Label (..))
import Noll.Compiler.Transform.Fold
import Noll.Language

testPattern1 :: Pattern () ()
testPattern1 =
  PAs
    ()
    (Label () "m")
    ( PConstructor
        ()
        (Label () "Succ")
        [ PAtVariable () (Label () "f")
        ]
    )

testPattern2 :: Pattern () ()
testPattern2 =
  PTuple
    ()
    ()
    ( PAs
        ()
        (Label () "m")
        ( PConstructor
            ()
            (Label () "Succ")
            [ PAtVariable () (Label () "f")
            ]
        )
        <| PAs
          ()
          (Label () "n")
          (PAny () ())
        :| []
    )

testExpression1 =
  EFold
    ()
    ()
    ( EApplication
        ()
        ()
        (EVariable () (Label () "pack_nat"))
        ( EVariable () (Label () "n")
            :| []
        )
        :| []
    )
    ( EClause
        ()
        ( PConstructor
            ()
            (Label () "Zero")
            []
        )
        ( CPlain
            ()
            []
            (ELiteral () (LInt32 1))
            :| []
        )
        <| EClause
          ()
          ( PAs
              ()
              (Label () "m")
              ( PConstructor
                  ()
                  (Label () "Succ")
                  [ PAtVariable () (Label () "f")
                  ]
              )
          )
          ( CPlain
              ()
              []
              ( ELet
                  ()
                  ( BPattern
                      ()
                      (PVariable () (Label () "aa"))
                      ( EApplication
                          ()
                          ()
                          (EVariable () (Label () "trace_int32"))
                          (EVariable () (Label () "n") :| [])
                      )
                      :| []
                  )
                  ( EApplication
                      ()
                      ()
                      (EBinaryOperator () () OMultiplication)
                      ( EVariable () (Label () "n")
                          <| EVariable () (Label () "f")
                          :| []
                      )
                  )
              )
              :| []
          )
        :| []
    )
    Nothing

testExpression1_1 = evalFoldExpansion "fold" 1 (compileFolds testExpression1)
