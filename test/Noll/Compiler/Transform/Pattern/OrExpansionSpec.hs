{-# LANGUAGE OverloadedStrings #-}

module Noll.Compiler.Transform.Pattern.OrExpansionSpec where

import Noll.Examples.Test06 (test06)
import Noll.Examples.Test07 (test07)
import Control.Monad.Identity (runIdentity)
import Noll.Common.List1 (NonEmpty (..), fromList1)
import Noll.Compiler.Transform.Pattern.OrExpansion
import Noll.Label (Label (..))
import Noll.Language (
  Choice (..),
  Clause (..),
  Expression (..),
  Kind (..),
  Pattern (..),
  Primitive (..),
  Type (..),
 )
import Test.Hspec (Spec, describe, it)

spec :: Spec
spec = do
  describe "expandOrPatterns" $ do
    it "" $
      fromList1 (runIdentity (expandOrPatterns fixture)) == fixture1
    it "" $
      fromList1 (runIdentity (expandOrPatterns fixture2)) == fixture3
    it "" $
      fromList1 (runIdentity (expandOrPatterns fixture4)) == fixture5
  describe "expandOrPatterns" $ do
    it "" $
      1 == 2

fixture :: Pattern () ()
fixture =
  PConstructor
    ()
    (Label () "Foo")
    [ POr () () (PVariable () (Label () "a")) (PVariable () (Label () "b"))
    , PVariable () (Label () "c")
    ]

fixture1 :: [Pattern () ()]
fixture1 =
  [ PConstructor
      ()
      (Label () "Foo")
      [ PVariable () (Label () "a")
      , PVariable () (Label () "c")
      ]
  , PConstructor
      ()
      (Label () "Foo")
      [ PVariable () (Label () "b")
      , PVariable () (Label () "c")
      ]
  ]

-- Foo(a or Baz(b or c), d)
--
fixture2 :: Pattern () ()
fixture2 =
  PConstructor
    ()
    (Label () "Foo")
    [ POr
        ()
        ()
        (PVariable () (Label () "a"))
        ( PConstructor
            ()
            (Label () "Baz")
            [ POr () () (PVariable () (Label () "b")) (PVariable () (Label () "c"))
            ]
        )
    , PVariable () (Label () "d")
    ]

-- Foo(a, d)
-- Foo(Baz(b), d)
-- Foo(Baz(c), d)
--
fixture3 :: [Pattern () ()]
fixture3 =
  [ PConstructor
      ()
      (Label () "Foo")
      [ PVariable () (Label () "a")
      , PVariable () (Label () "d")
      ]
  , PConstructor
      ()
      (Label () "Foo")
      [ PConstructor () (Label () "Baz") [PVariable () (Label () "b")]
      , PVariable () (Label () "d")
      ]
  , PConstructor
      ()
      (Label () "Foo")
      [ PConstructor () (Label () "Baz") [PVariable () (Label () "c")]
      , PVariable () (Label () "d")
      ]
  ]

fixture4 =
  EClause
    ()
    ( POr
        ()
        ()
        (PConstructor () (Label () "LessThan") [])
        (PConstructor () (Label () "EqualTo") [])
    )
    (CPlain () [] (ELiteral () (LBool True)) :| [])

fixture5 =
  [ EClause
      ()
      (PConstructor () (Label () "LessThan") [])
      (CPlain () [] (ELiteral () (LBool True)) :| [])
  , EClause
      ()
      (PConstructor () (Label () "EqualTo") [])
      (CPlain () [] (ELiteral () (LBool True)) :| [])
  ]
