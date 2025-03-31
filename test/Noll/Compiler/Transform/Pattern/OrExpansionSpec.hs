{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Noll.Compiler.Transform.Pattern.OrExpansionSpec where

import Control.Monad.Identity (runIdentity)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (transformBiM)
import Noll.Common.List1 (NonEmpty (..), fromList1, (<|))
import Noll.Compiler.Transform.Pattern.OrExpansion
import Noll.Examples.Test06 (test06)
import Noll.Examples.Test07 (test07)
import Lang.Label (Label (..))
import Noll.Language (
  Choice (..),
  Clause (..),
  Expression (..),
  Intrinsic (..),
  Kind (..),
  Pattern (..),
  Primitive (..),
  Type (..),
  TypeIndex (..),
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
      runIdentity (traverse compileOrPatterns test06) == test07
  describe "expandExpression" $ do
    it "" $
      runIdentity (compileExpression fixture6) == fixture7

compileExpression :: forall m e a t. (Monad m, Data a, Data t) => Expression a t -> m (Expression a t)
compileExpression = transformBiM (expandExpression :: Expression a t -> m (Expression a t))

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

tree0 :: Type TypeIndex Kind
tree0 = TApplication KType (TConstructor (KArrow KType KType) "Tree") (TVariable (TypeIndex KType 0) :| [])

list0 :: Type TypeIndex Kind
list0 = TIntrinsic (IList (TVariable (TypeIndex KType 0)))

tvar0 :: Type TypeIndex Kind
tvar0 = TVariable (TypeIndex KType 0)

tree1 :: Type TypeIndex Kind
tree1 = TApplication KType (TConstructor (KArrow KType KType) "Tree") (TVariable (TypeIndex KType 1) :| [])

list1 :: Type TypeIndex Kind
list1 = TIntrinsic (IList (TVariable (TypeIndex KType 1)))

tvar1 :: Type TypeIndex Kind
tvar1 = TVariable (TypeIndex KType 1)

fixture6 =
  ELambda
    ()
    ( PVariable () (Label tvar0 "m")
        <| PVariable () (Label tvar0 "n")
        :| []
    )
    ( EMatch
        ()
        (TIntrinsic IBool)
        ( EApplication
            ()
            (TConstructor KType "Ordering")
            ( EVariable
                ()
                ( Label
                    (tvar0 `TArrow` tvar0 `TArrow` TConstructor KType "Ordering")
                    "compare"
                )
            )
            ( EVariable () (Label (TVariable (TypeIndex KType 0)) "m")
                <| EVariable () (Label (TVariable (TypeIndex KType 0)) "n")
                :| []
            )
        )
        ( EClause
            ()
            ( POr
                ()
                (TConstructor KType "Ordering")
                (PConstructor () (Label (TConstructor KType "Ordering") "LessThan") [])
                (PConstructor () (Label (TConstructor KType "Ordering") "EqualTo") [])
            )
            (CPlain () [] (ELiteral () (LBool True)) :| [])
            :| [ EClause
                  ()
                  (PConstructor () (Label (TConstructor KType "Ordering") "GreaterThan") [])
                  ( CPlain
                      ()
                      []
                      ( EMatch
                          ()
                          (TIntrinsic IBool)
                          (EVariable () (Label (TConstructor KType "Ordering") "foo"))
                          ( EClause
                              ()
                              ( POr
                                  ()
                                  (TConstructor KType "Ordering")
                                  (PConstructor () (Label (TConstructor KType "Ordering") "LessThan") [])
                                  (PConstructor () (Label (TConstructor KType "Ordering") "EqualTo") [])
                              )
                              (CPlain () [] (ELiteral () (LBool True)) :| [])
                              :| []
                          )
                      )
                      :| []
                  )
               ]
        )
    )

fixture7 =
  ELambda
    ()
    ( PVariable () (Label tvar0 "m")
        <| PVariable () (Label tvar0 "n")
        :| []
    )
    ( EMatch
        ()
        (TIntrinsic IBool)
        ( EApplication
            ()
            (TConstructor KType "Ordering")
            ( EVariable
                ()
                ( Label
                    (tvar0 `TArrow` tvar0 `TArrow` TConstructor KType "Ordering")
                    "compare"
                )
            )
            ( EVariable () (Label (TVariable (TypeIndex KType 0)) "m")
                <| EVariable () (Label (TVariable (TypeIndex KType 0)) "n")
                :| []
            )
        )
        ( EClause
            ()
            (PConstructor () (Label (TConstructor KType "Ordering") "LessThan") [])
            (CPlain () [] (ELiteral () (LBool True)) :| [])
            :| [ EClause
                  ()
                  (PConstructor () (Label (TConstructor KType "Ordering") "EqualTo") [])
                  (CPlain () [] (ELiteral () (LBool True)) :| [])
               , EClause
                  ()
                  (PConstructor () (Label (TConstructor KType "Ordering") "GreaterThan") [])
                  ( CPlain
                      ()
                      []
                      ( EMatch
                          ()
                          (TIntrinsic IBool)
                          (EVariable () (Label (TConstructor KType "Ordering") "foo"))
                          ( EClause
                              ()
                              (PConstructor () (Label (TConstructor KType "Ordering") "LessThan") [])
                              (CPlain () [] (ELiteral () (LBool True)) :| [])
                              :| [ EClause
                                    ()
                                    (PConstructor () (Label (TConstructor KType "Ordering") "EqualTo") [])
                                    (CPlain () [] (ELiteral () (LBool True)) :| [])
                                 ]
                          )
                      )
                      :| []
                  )
               ]
        )
    )
