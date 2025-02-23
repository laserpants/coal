{-# LANGUAGE OverloadedStrings #-}

module Noll.Compiler.CoreSpec where

import Control.Monad.Identity (runIdentity)
import Control.Monad.State (evalState)
import Data.Fix (Fix (..))
import Noll.Common.List1 (NonEmpty (..), (<|))
import Noll.Compiler.Core
import Noll.Core.Language (ExprF (..), Prim (..), list, opaque, (~>))
import Noll.Core.Language.Expr (Clause (..), Focus (..))
import Noll.Label (Label (..))
import Test.Hspec (Spec, describe, it)

import qualified Noll.Core.Language as Core

spec :: Spec
spec =
  describe "Noll.Compiler.Core" $ do
    describe "transSuffixExpr" $ do
      it "" $ do
        evalState (transSuffixExpr fixture3) 0 == fixture4
    describe "liftLambdas" $ do
      it "" $ do
        liftLambdas test1 == test1Result
      it "" $ do
        liftLambdas test2 == test2Result
    describe "simplifyELet" $ do
      it "" $ do
        simplifyELet fixture40 == fixture41
    describe "" $ do
      it "" $ do
        closeDefs foo1 == foo1Result

--      it "" $ do
--        liftLambdas test3 == test3Result

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

compareRow :: Core.Type
compareRow = Core.RExt "compare" (opaque ~> opaque ~> Core.TCon "Ordering" []) opaque

fromInt32Row :: Core.Type
fromInt32Row = Core.RExt "from_int32" (Core.int32 ~> opaque) opaque

orderedRow :: Core.Type
orderedRow = Core.RExt "compare" (opaque ~> opaque ~> Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 ~> opaque) opaque)

orderedInt32Row :: Core.Type
orderedInt32Row = Core.RExt "compare" (Core.int32 ~> Core.int32 ~> Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 ~> Core.int32) opaque)

maxMinRow :: Core.Type -> Core.Type
maxMinRow r = Core.RExt "max" opaque (Core.RExt "min" opaque r)

-- record({ compare : * -> * -> Ordering | * })
compareDict :: Core.Type
compareDict = Core.record compareRow

-- record({ from_int32 : int32 -> * | * })
fromInt32Dict :: Core.Type
fromInt32Dict = Core.record fromInt32Row

-- record({ compare : * -> * -> Ordering | from_int32 : int32 -> * | * })
orderedDict :: Core.Type
orderedDict = Core.record orderedRow

orderedInt32Dict :: Core.Type
orderedInt32Dict = Core.record orderedInt32Row

ordering :: Core.Type
ordering = Core.TCon "Ordering" []

-- record({ max : 0 | min : 0 | r })
maxMinRecord :: Core.Type -> Core.Type
maxMinRecord r = Core.record (maxMinRow r)

tree :: Core.Type -> Core.Type
tree t = Core.TCon "Tree" [t]

fixture3 :: Core.Expr Core.Type
fixture3 =
  Core.let_
    ( ( Label ((opaque ~> opaque) ~> (opaque ~> opaque) ~> opaque ~> opaque) "_compose_"
      , Core.lam
          ( Label (opaque ~> opaque) "f"
              :| [ Label (opaque ~> opaque) "g"
                 , Label opaque "x"
                 ]
          )
          ( Core.app
              opaque
              (Core.var (Label (opaque ~> opaque) "f"))
              ( Core.app
                  opaque
                  (Core.var (Label (opaque ~> opaque) "g"))
                  (Core.var (Label opaque "x") :| [])
                  :| []
              )
          )
      )
        :| []
    )
    ( Core.let_
        ( ( Label (list opaque ~> list opaque ~> list opaque) "_list_concat_"
          , Core.lam
              (Label (list opaque) "a" :| [Label (list opaque) "b"])
              ( Core.match
                  (list opaque)
                  (Core.var (Label (list opaque) "a"))
                  ( Clause
                      (Label (list opaque) "$Nil" :| [])
                      (Core.var (Label (list opaque) "b"))
                      :| [ Clause
                            ( Label (opaque ~> list opaque ~> list opaque) "$Cons"
                                <| Label opaque "x"
                                <| Label (list opaque) "xs"
                                :| []
                            )
                            ( Core.app
                                (list opaque)
                                (Core.var (Label (opaque ~> list opaque ~> list opaque) "$Cons"))
                                ( Core.var (Label opaque "x")
                                    <| Core.app
                                      (list opaque)
                                      (Core.var (Label (list opaque ~> list opaque ~> list opaque) "_list_concat_"))
                                      ( Core.var (Label (list opaque) "xs")
                                          <| Core.var (Label (list opaque) "b")
                                          :| []
                                      )
                                    :| []
                                )
                            )
                         ]
                  )
              )
          )
            :| [
                 ( Label (compareDict ~> opaque ~> opaque ~> ordering) "compare"
                 , Core.lam
                    ( Label compareDict "a_1"
                        <| Label opaque "a_2"
                        <| Label opaque "a_3"
                        :| []
                    )
                    ( Core.match
                        ordering
                        (Core.var (Label compareDict "a_1"))
                        ( Clause
                            ( Label (compareRow ~> compareDict) "$Record"
                                <| Label compareRow "r_1"
                                :| []
                            )
                            ( Core.sel
                                ( Focus
                                    "compare"
                                    (Label (opaque ~> opaque ~> ordering) "f_1")
                                    (Label opaque "q_1")
                                )
                                (Core.var (Label compareRow "r_1"))
                                ( Core.app
                                    ordering
                                    (Core.var (Label (opaque ~> opaque ~> ordering) "f_1"))
                                    ( Core.var (Label opaque "a_2")
                                        <| Core.var (Label opaque "a_3")
                                        :| []
                                    )
                                )
                            )
                            :| []
                        )
                    )
                 )
               ]
        )
        (Core.lit (Core.PInt32 1))
    )

fixture4 :: Core.Expr Core.Type
fixture4 =
  Core.let_
    ( ( Label ((opaque ~> opaque) ~> (opaque ~> opaque) ~> opaque ~> opaque) "_compose_.[15]"
      , Core.lam
          ( Label (opaque ~> opaque) "f.[0]"
              :| [ Label (opaque ~> opaque) "g.[1]"
                 , Label opaque "x.[2]"
                 ]
          )
          ( Core.app
              opaque
              (Core.var (Label (opaque ~> opaque) "f.[0]"))
              ( Core.app
                  opaque
                  (Core.var (Label (opaque ~> opaque) "g.[1]"))
                  (Core.var (Label opaque "x.[2]") :| [])
                  :| []
              )
          )
      )
        :| []
    )
    ( Core.let_
        ( ( Label (list opaque ~> list opaque ~> list opaque) "_list_concat_.[13]"
          , Core.lam
              (Label (list opaque) "a.[5]" :| [Label (list opaque) "b.[6]"])
              ( Core.match
                  (list opaque)
                  (Core.var (Label (list opaque) "a.[5]"))
                  ( Clause
                      (Label (list opaque) "$Nil" :| [])
                      (Core.var (Label (list opaque) "b.[6]"))
                      :| [ Clause
                            ( Label (opaque ~> list opaque ~> list opaque) "$Cons"
                                <| Label opaque "x.[3]"
                                <| Label (list opaque) "xs.[4]"
                                :| []
                            )
                            ( Core.app
                                (list opaque)
                                (Core.var (Label (opaque ~> list opaque ~> list opaque) "$Cons"))
                                ( Core.var (Label opaque "x.[3]")
                                    <| Core.app
                                      (list opaque)
                                      (Core.var (Label (list opaque ~> list opaque ~> list opaque) "_list_concat_.[13]"))
                                      ( Core.var (Label (list opaque) "xs.[4]")
                                          <| Core.var (Label (list opaque) "b.[6]")
                                          :| []
                                      )
                                    :| []
                                )
                            )
                         ]
                  )
              )
          )
            :| [
                 ( Label (compareDict ~> opaque ~> opaque ~> ordering) "compare.[14]"
                 , Core.lam
                    ( Label compareDict "a_1.[10]"
                        <| Label opaque "a_2.[11]"
                        <| Label opaque "a_3.[12]"
                        :| []
                    )
                    ( Core.match
                        ordering
                        (Core.var (Label compareDict "a_1.[10]"))
                        ( Clause
                            ( Label (compareRow ~> compareDict) "$Record"
                                <| Label compareRow "r_1.[9]"
                                :| []
                            )
                            ( Core.sel
                                ( Focus
                                    "compare"
                                    (Label (opaque ~> opaque ~> ordering) "f_1.[7]")
                                    (Label opaque "q_1.[8]")
                                )
                                (Core.var (Label compareRow "r_1.[9]"))
                                ( Core.app
                                    ordering
                                    (Core.var (Label (opaque ~> opaque ~> ordering) "f_1.[7]"))
                                    ( Core.var (Label opaque "a_2.[11]")
                                        <| Core.var (Label opaque "a_3.[12]")
                                        :| []
                                    )
                                )
                            )
                            :| []
                        )
                    )
                 )
               ]
        )
        (Core.lit (Core.PInt32 1))
    )

test1 :: ObjectList
test1 =
  [ OFunction "f" [Label Core.TOpq "x", Label Core.TOpq "n"] (Core.var (Label Core.TOpq "x"))
  , OConstant
      "h"
      ( Core.app
          (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.int32)
          ( Core.var
              ( Label
                  ((Core.TOpq `Core.arrow` Core.int32) `Core.arrow` Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.int32)
                  "f"
              )
          )
          ( Core.lam
              (Label Core.TOpq "y" :| [])
              (Core.lit (Core.PInt32 33))
              :| []
          )
      )
  ]

test1Result :: ObjectList
test1Result =
  [ OFunction "f" [Label Core.TOpq "x", Label Core.TOpq "n"] (Core.var (Label Core.TOpq "x"))
  , OConstant
      "h"
      ( Core.app
          (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.int32)
          ( Core.var
              ( Label
                  ((Core.TOpq `Core.arrow` Core.int32) `Core.arrow` Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.int32)
                  "f"
              )
          )
          ( Core.var (Label (Core.TOpq `Core.arrow` Core.int32) "$anon.1")
              :| []
          )
      )
  , OFunction "$anon.1" [Label Core.TOpq "y"] (Core.lit (Core.PInt32 33))
  ]

test2 :: ObjectList
test2 =
  [ OFunction
      "f"
      [Label Core.int32 "x"]
      ( Core.if_
          ( Core.op
              ( Core.OEqInt32
                  (Core.var (Label Core.int32 "x"))
                  (Core.lit (Core.PInt32 0))
              )
          )
          (Core.var (Label Core.int32 "n"))
          ( Core.let_
              ( ( Label Core.int32 "m"
                , Core.op
                    ( Core.OMulInt32
                        (Core.var (Label Core.int32 "x"))
                        ( Core.app
                            Core.int32
                            (Core.var (Label (Core.int32 `Core.arrow` Core.int32) "f"))
                            ( Core.op
                                ( Core.OSubInt32
                                    (Core.var (Label Core.int32 "x"))
                                    (Core.lit (Core.PInt32 1))
                                )
                                :| []
                            )
                        )
                    )
                )
                  :| []
              )
              ( Core.call
                  (Label (Core.int32 `Core.arrow` Core.TOpq) "print_int32")
                  [Core.var (Label Core.int32 "m")]
                  ( Core.lam
                      (Label Core.TOpq "_" :| [])
                      (Core.var (Label Core.int32 "m"))
                  )
              )
          )
      )
  , OFunction
      "factorial"
      [Label Core.int32 "z"]
      ( Core.let_
          ( ( Label Core.int32 "n"
            , Core.lit (Core.PInt32 1)
            )
              :| []
          )
          ( Core.app
              Core.int32
              (Core.var (Label (Core.int32 `Core.arrow` Core.int32) "f"))
              ( Core.var (Label Core.int32 "z")
                  :| []
              )
          )
      )
  ]

test2Result :: ObjectList
test2Result =
  [ OFunction
      "f"
      [Label Core.int32 "x"]
      ( Core.if_
          ( Core.op
              ( Core.OEqInt32
                  (Core.var (Label Core.int32 "x"))
                  (Core.lit (Core.PInt32 0))
              )
          )
          (Core.var (Label Core.int32 "n"))
          ( Core.let_
              ( ( Label Core.int32 "m"
                , Core.op
                    ( Core.OMulInt32
                        (Core.var (Label Core.int32 "x"))
                        ( Core.app
                            Core.int32
                            (Core.var (Label (Core.int32 `Core.arrow` Core.int32) "f"))
                            ( Core.op
                                ( Core.OSubInt32
                                    (Core.var (Label Core.int32 "x"))
                                    (Core.lit (Core.PInt32 1))
                                )
                                :| []
                            )
                        )
                    )
                )
                  :| []
              )
              ( Core.call
                  (Label (Core.int32 `Core.arrow` Core.TOpq) "print_int32")
                  [Core.var (Label Core.int32 "m")]
                  (Core.var (Label (Core.TOpq `Core.arrow` Core.int32) "$anon.1"))
              )
          )
      )
  , OFunction
      "factorial"
      [Label Core.int32 "z"]
      ( Core.let_
          ( ( Label Core.int32 "n"
            , Core.lit (Core.PInt32 1)
            )
              :| []
          )
          ( Core.app
              Core.int32
              (Core.var (Label (Core.int32 `Core.arrow` Core.int32) "f"))
              ( Core.var (Label Core.int32 "z")
                  :| []
              )
          )
      )
  , OFunction
      "$anon.1"
      [Label Core.TOpq "_"]
      ( Core.var (Label Core.int32 "m")
      )
  ]

fixture40 :: Core.Expr ()
fixture40 =
  Core.let_
    ( ( Label () "a"
      , Core.var (Label () "b")
      )
        :| []
    )
    (Core.var (Label () "a"))

fixture41 :: Core.Expr ()
fixture41 = Core.var (Label () "b")

fixture42 =
  ( Fix
      ( ELet
          ((Label () "fold_.[48]", Fix (EVar (Label () "$anon.13"))) :| [])
          (Fix (EApp () (Fix (EVar (Label () "fold_.[48]"))) (Fix (EVar (Label () "list.[49]")) :| [Fix (EApp () (Fix (EVar (Label () "$Record"))) (Fix (EExt (Label () "min") (Fix (EApp () (Fix (EVar (Label () "from_int32.[68]"))) (Fix (EVar (Label () "d_1.[50]")) :| [Fix (ELit (PInt32 0))]))) (Fix (EExt (Label () "max") (Fix (EApp () (Fix (EVar (Label () "from_int32.[68]"))) (Fix (EVar (Label () "d_1.[50]")) :| [Fix (ELit (PInt32 (-1)))]))) (Fix ENil)))) :| []))])))
      )
  )

test3 =
  --  [ OFunction
  --      "lte"
  --      [ Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$dict:1"
  --      , Label Core.TOpq "x"
  --      , Label Core.TOpq "y"
  --      ]
  --      ( Core.match
  --          Core.TOpq
  --          ( Core.app
  --              (Core.TCon "Ordering" [])
  --              ( Core.var
  --                  ( Label
  --                      ( Core.TCon "Ordered" [Core.TOpq]
  --                          `Core.arrow` Core.TOpq
  --                          `Core.arrow` Core.TOpq
  --                          `Core.arrow` Core.TCon "Ordering" []
  --                      )
  --                      "compare"
  --                  )
  --              )
  --              ( Core.var (Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$dict:1")
  --                  <| Core.var (Label Core.TOpq "x")
  --                  <| Core.var (Label Core.TOpq "y")
  --                  :| []
  --              )
  --          )
  --          ( Core.Clause
  --              (Label (Core.TCon "Ordering" []) "EqualTo" :| [])
  --              (Core.lit (Core.PBool True))
  --              <| Core.Clause
  --                (Label (Core.TCon "Ordering" []) "GreaterThan" :| [])
  --                (Core.lit (Core.PBool False))
  --              <| Core.Clause
  --                (Label (Core.TCon "Ordering" []) "LessThan" :| [])
  --                (Core.lit (Core.PBool True))
  --              :| []
  --          )
  --      )
  --  , OFunction
  --      "gt"
  --      [ Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$dict:1"
  --      , Label Core.TOpq "x"
  --      ]
  --      ( Core.app
  --          (Core.TOpq `Core.arrow` Core.bool)
  --          ( Core.var
  --              ( Label
  --                  ( (Core.bool `Core.arrow` Core.bool)
  --                      `Core.arrow` (Core.TOpq `Core.arrow` Core.bool)
  --                      `Core.arrow` (Core.TOpq `Core.arrow` Core.bool)
  --                  )
  --                  "(<<)"
  --              )
  --          )
  --          ( Core.var (Label (Core.bool `Core.arrow` Core.bool) "not")
  --              <| Core.app
  --                (Core.TOpq `Core.arrow` Core.bool)
  --                ( Core.var
  --                    ( Label
  --                        ( Core.TCon "Ordered" [Core.TOpq]
  --                            `Core.arrow` Core.TOpq
  --                            `Core.arrow` Core.TOpq
  --                            `Core.arrow` Core.bool
  --                        )
  --                        "lte"
  --                    )
  --                )
  --                ( Core.var (Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$dict:1")
  --                    <| Core.var (Label Core.TOpq "x")
  --                    :| []
  --                )
  --              :| []
  --          )
  --      )
  --  , OFunction
  --      "in_range"
  --      [ Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$dict:1"
  --      , Label (Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]) "$record:expr:1"
  --      , Label Core.TOpq "n"
  --      ]
  --      ( Core.match
  --          undefined
  --          (Core.var (Label (Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]) "$record:expr:1"))
  --          ( Core.Clause
  --              ( Label
  --                  ( Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)
  --                      `Core.arrow` Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]
  --                  )
  --                  "$Record"
  --                  <| Label (Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)) "$row"
  --                  :| []
  --              )
  --              ( Core.sel
  --                  ( Core.Focus
  --                      "min"
  --                      (Label Core.TOpq "min")
  --                      (Label (Core.RExt "max" Core.TOpq Core.RNil) "$tail:1")
  --                  )
  --                  (Core.var (Label (Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)) "$row"))
  --                  ( Core.sel
  --                      ( Core.Focus
  --                          "max"
  --                          (Label Core.TOpq "max")
  --                          (Label Core.RNil "$tail:2")
  --                      )
  --                      (Core.var (Label (Core.RExt "max" Core.TOpq Core.RNil) "$tail:1"))
  --                      ( Core.op
  --                          -- (&&)
  --                          ( Core.OAnd
  --                              ( Core.app
  --                                  Core.bool
  --                                  -- gt
  --                                  (Core.var (Label (Core.TCon "Ordered" [Core.TOpq] `Core.arrow` Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.bool) "gt"))
  --                                  ( Core.var (Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$dict:1")
  --                                      <| Core.var (Label Core.TOpq "n")
  --                                      <| Core.var (Label Core.TOpq "min")
  --                                      :| []
  --                                  )
  --                              )
  --                              ( Core.op
  --                                  -- (||)
  --                                  ( Core.OOr
  --                                      ( Core.app
  --                                          Core.bool
  --                                          -- gt
  --                                          (Core.var (Label (Core.TCon "Ordered" [Core.TOpq] `Core.arrow` Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.bool) "gt"))
  --                                          ( Core.var (Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$dict:1")
  --                                              <| Core.var (Label Core.TOpq "min")
  --                                              <| Core.var (Label Core.TOpq "max")
  --                                              :| []
  --                                          )
  --                                      )
  --                                      ( Core.app
  --                                          Core.bool
  --                                          -- lte
  --                                          (Core.var (Label (Core.TCon "Ordered" [Core.TOpq] `Core.arrow` Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.bool) "lte"))
  --                                          ( Core.var (Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$dict:1")
  --                                              <| Core.var (Label Core.TOpq "n")
  --                                              <| Core.var (Label Core.TOpq "max")
  --                                              :| []
  --                                          )
  --                                      )
  --                                  )
  --                              )
  --                          )
  --                      )
  --                  )
  --              )
  --              :| []
  --          )
  --      )
  --  , OFunction
  --      "from_list"
  --      [ Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$dict:1"
  --      , Label (Core.TCon "List" [Core.TOpq]) "list"
  --      ]
  --      ( Core.app
  --          (Core.TCon "Tree" [Core.TOpq])
  --          ( Core.var
  --              ( Label
  --                  ( Core.TCon "List" [Core.TOpq]
  --                      `Core.arrow` Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]
  --                      `Core.arrow` Core.TCon "Tree" [Core.TOpq]
  --                  )
  --                  "$fold:1:a"
  --              )
  --          )
  --          ( Core.var (Label (Core.TCon "List" [Core.TOpq]) "list")
  --              <| Core.app
  --                (Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)])
  --                ( Core.var
  --                    ( Label
  --                        ( Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)
  --                            `Core.arrow` Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]
  --                        )
  --                        "$Record"
  --                    )
  --                )
  --                ( Core.ext
  --                    (Label Core.int32 "max")
  --                    ( Core.app
  --                        Core.TOpq
  --                        (Core.var (Label (Core.TCon "Ordered" [Core.TOpq] `Core.arrow` Core.int32 `Core.arrow` Core.TOpq) "from_int32"))
  --                        ( Core.var (Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$1")
  --                            <| Core.lit (Core.PInt32 (-1))
  --                            :| []
  --                        )
  --                    )
  --                    ( Core.ext
  --                        (Label Core.int32 "min")
  --                        ( Core.app
  --                            Core.TOpq
  --                            (Core.var (Label (Core.TCon "Ordered" [Core.TOpq] `Core.arrow` Core.int32 `Core.arrow` Core.TOpq) "from_int32"))
  --                            ( Core.var (Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$1")
  --                                <| Core.lit (Core.PInt32 0)
  --                                :| []
  --                            )
  --                        )
  --                        Core.nil
  --                    )
  --                    :| []
  --                )
  --              :| []
  --          )
  --      )
  --  ,
  [ OFunction
      "$fold:1:a"
      [ Label (Core.TCon "List" [Core.TOpq]) "$fold:1:expr"
      ]
      ( Core.match
          Core.TOpq
          (Core.var (Label (Core.TCon "List" [Core.TOpq]) "$fold:1:expr"))
          ( Core.Clause
              ( Label (Core.TOpq `Core.arrow` Core.TCon "List" [Core.TOpq] `Core.arrow` Core.TCon "List" [Core.TOpq]) "$Cons"
                  <| Label Core.TOpq "$p:0:p"
                  <| Label (Core.TCon "List" [Core.TOpq]) "$p:1:g"
                  :| []
              )
              ( Core.lam
                  (Label (Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]) "range" :| [])
                  ( Core.if_
                      ( Core.app
                          Core.bool
                          -- (|.)
                          ( Core.var
                              ( Label
                                  ( Core.TOpq
                                      `Core.arrow` (Core.TOpq `Core.arrow` Core.bool)
                                      `Core.arrow` Core.bool
                                  )
                                  "(|.)"
                              )
                          )
                          ( Core.var (Label Core.TOpq "$p:0:p")
                              <| Core.app
                                (Core.TOpq `Core.arrow` Core.bool)
                                -- in_range
                                ( Core.var
                                    ( Label
                                        ( Core.TCon "Ordered" [Core.TOpq]
                                            `Core.arrow` Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]
                                            `Core.arrow` Core.TOpq
                                            `Core.arrow` Core.bool
                                        )
                                        "in_range"
                                    )
                                )
                                ( Core.var (Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$dict:1")
                                    <| Core.var (Label (Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]) "range")
                                    :| []
                                )
                              :| []
                          )
                      )
                      ( Core.app
                          (Core.TCon "Tree" [Core.TOpq])
                          ( Core.var
                              ( Label
                                  ( Core.TOpq
                                      `Core.arrow` Core.TCon "Tree" [Core.TOpq]
                                      `Core.arrow` Core.TCon "Tree" [Core.TOpq]
                                      `Core.arrow` Core.TCon "Tree" [Core.TOpq]
                                  )
                                  "Node"
                              )
                          )
                          ( Core.var (Label Core.TOpq "$p:0:p")
                              <| Core.app
                                (Core.TCon "Tree" [Core.TOpq])
                                ( Core.var
                                    ( Label
                                        ( Core.TCon "List" [Core.TOpq]
                                            `Core.arrow` Core.TCon "$Record" [Core.RExt "max" Core.int32 (Core.RExt "min" Core.int32 Core.RNil)]
                                            `Core.arrow` Core.TCon "Tree" [Core.TOpq]
                                        )
                                        "$fold:1:a"
                                    )
                                )
                                ( Core.var (Label (Core.TCon "List" [Core.TOpq]) "$p:1:g")
                                    <| Core.app
                                      (Core.TCon "$Record" [Core.RExt "max" Core.int32 (Core.RExt "min" Core.int32 Core.RNil)])
                                      ( Core.var
                                          ( Label
                                              ( Core.RExt "max" Core.int32 (Core.RExt "min" Core.int32 Core.RNil)
                                                  `Core.arrow` Core.TCon "$Record" [Core.RExt "max" Core.int32 (Core.RExt "min" Core.int32 Core.RNil)]
                                              )
                                              "$Record"
                                          )
                                      )
                                      ( Core.ext
                                          (Label Core.TOpq "max")
                                          (Core.var (Label Core.TOpq "$p:0:p"))
                                          ( Core.ext
                                              (Label Core.TOpq "min")
                                              ( ( Core.match
                                                    Core.TOpq
                                                    (Core.var (Label (Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]) "range"))
                                                    ( Core.Clause
                                                        ( Label
                                                            ( Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)
                                                                `Core.arrow` Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]
                                                            )
                                                            "$Record"
                                                            <| Label (Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)) "$row"
                                                            :| []
                                                        )
                                                        ( Core.sel
                                                            ( Core.Focus
                                                                "min"
                                                                (Label Core.TOpq "$f-1")
                                                                (Label (Core.RExt "max" Core.TOpq Core.RNil) "_")
                                                            )
                                                            (Core.var (Label (Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)) "$row"))
                                                            (Core.var (Label Core.TOpq "$f-1"))
                                                        )
                                                        :| []
                                                    )
                                                )
                                              )
                                              Core.nil
                                          )
                                          :| []
                                      )
                                    :| []
                                )
                              <| Core.app
                                (Core.TCon "Tree" [Core.TOpq])
                                ( Core.var
                                    ( Label
                                        ( Core.TCon "List" [Core.TOpq]
                                            `Core.arrow` Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]
                                            `Core.arrow` Core.TCon "Tree" [Core.TOpq]
                                        )
                                        "$fold:1:a"
                                    )
                                )
                                ( Core.var (Label (Core.TCon "List" [Core.TOpq]) "g")
                                    <| Core.app
                                      (Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)])
                                      ( Core.var
                                          ( Label
                                              ( Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)
                                                  `Core.arrow` Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]
                                              )
                                              "$Record"
                                          )
                                      )
                                      ( Core.ext
                                          (Label Core.TOpq "max")
                                          ( Core.match
                                              Core.TOpq
                                              (Core.var (Label (Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]) "range"))
                                              ( Core.Clause
                                                  ( Label
                                                      ( Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)
                                                          `Core.arrow` Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]
                                                      )
                                                      "$Record"
                                                      <| Label (Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)) "$row"
                                                      :| []
                                                  )
                                                  ( Core.sel
                                                      ( Core.Focus
                                                          "max"
                                                          (Label Core.TOpq "$f-1")
                                                          (Label (Core.RExt "min" Core.TOpq Core.RNil) "_")
                                                      )
                                                      (Core.var (Label (Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)) "$row"))
                                                      (Core.var (Label Core.TOpq "$f-1"))
                                                  )
                                                  :| []
                                              )
                                          )
                                          ( Core.ext
                                              (Label Core.TOpq "min")
                                              (Core.var (Label Core.TOpq "p"))
                                              Core.nil
                                          )
                                          :| []
                                      )
                                    :| []
                                )
                              :| []
                          )
                      )
                      ( Core.app
                          (Core.TCon "Tree" [Core.TOpq])
                          ( Core.var
                              ( Label
                                  ( Core.TCon "List" [Core.TOpq]
                                      `Core.arrow` Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]
                                      `Core.arrow` Core.TCon "Tree" [Core.TOpq]
                                  )
                                  "$fold:1:a"
                              )
                          )
                          ( Core.var (Label (Core.TCon "List" [Core.TOpq]) "g")
                              <| Core.var (Label (Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]) "range")
                              :| []
                          )
                      )
                  )
              )
              <| Core.Clause
                (Label (Core.TCon "List" [Core.TOpq]) "$Nil" :| [])
                ( Core.lam
                    (Label (Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]) "_" :| [])
                    (Core.var (Label (Core.TCon "Tree" [Core.TOpq]) "Leaf"))
                )
              :| []
          )
      )
      --  , OFunction
      --      "flatten"
      --      [ Label (Core.TCon "Tree" [Core.TOpq]) "tree"
      --      ]
      --      ( Core.app
      --          (Core.TCon "List" [Core.TOpq])
      --          ( Core.var
      --              ( Label
      --                  ( Core.TCon "Tree" [Core.TOpq]
      --                      `Core.arrow` Core.TCon "List" [Core.TOpq]
      --                  )
      --                  "$fold:1:b"
      --              )
      --          )
      --          (Core.var (Label (Core.TCon "Tree" [Core.TOpq]) "tree") :| [])
      --      )
      ----  , OFunction
      ----      "$fold:1:b"
      ----      [ Label (Core.TCon "Tree" [Core.TOpq]) "$fold:1:expr"
      ----      ]
      ----      ( Core.match
      ----          (Core.var (Label (Core.TCon "Tree" [Core.TOpq]) "$fold:1:expr"))
      ----          ( Core.Clause
      ----              ( Label
      ----                  ( Core.TOpq
      ----                      `Core.arrow` Core.TCon "Tree" [Core.TOpq]
      ----                      `Core.arrow` Core.TCon "Tree" [Core.TOpq]
      ----                      `Core.arrow` Core.TCon "Tree" [Core.TOpq]
      ----                  )
      ----                  "Node"
      ----                  <| Label Core.TOpq "$p:0:y"
      ----                  <| Label (Core.TCon "Tree" [Core.TOpq]) "$p:1:lhs"
      ----                  <| Label (Core.TCon "Tree" [Core.TOpq]) "$p:2:rhs"
      ----                  :| []
      ----              )
      ----              ( Core.app
      ----                  (Core.TCon "List" [Core.TOpq])
      ----                  -- (++)
      ----                  ( Core.var
      ----                      ( Label
      ----                          ( Core.TCon "List" [Core.TOpq]
      ----                              `Core.arrow` Core.TCon "List" [Core.TOpq]
      ----                              `Core.arrow` Core.TCon "List" [Core.TOpq]
      ----                          )
      ----                          "(++)"
      ----                      )
      ----                  )
      ----                  ( Core.app
      ----                      (Core.TCon "List" [Core.TOpq])
      ----                      ( Core.var
      ----                          ( Label
      ----                              ( Core.TCon "Tree" [Core.TOpq]
      ----                                  `Core.arrow` Core.TCon "List" [Core.TOpq]
      ----                              )
      ----                              "$fold:1:b"
      ----                          )
      ----                      )
      ----                      ( Core.var (Label (Core.TCon "Tree" [Core.TOpq]) "$p:1:lhs")
      ----                          :| []
      ----                      )
      ----                      <|
      ----                      -- y :: $fold:1(rhs)
      ----                      ( Core.app
      ----                          (Core.TCon "List" [Core.TOpq])
      ----                          -- (::)
      ----                          ( Core.var
      ----                              ( Label
      ----                                  ( Core.TOpq
      ----                                      `Core.arrow` Core.TCon "List" [Core.TOpq]
      ----                                      `Core.arrow` Core.TCon "List" [Core.TOpq]
      ----                                  )
      ----                                  "(::)"
      ----                              )
      ----                          )
      ----                          ( Core.var (Label Core.TOpq "y")
      ----                              <| Core.app
      ----                                (Core.TCon "List" [Core.TOpq])
      ----                                ( Core.var
      ----                                    ( Label
      ----                                        ( Core.TCon "Tree" [Core.TOpq]
      ----                                            `Core.arrow` Core.TCon "List" [Core.TOpq]
      ----                                        )
      ----                                        "$fold:1:b"
      ----                                    )
      ----                                )
      ----                                ( Core.var (Label (Core.TCon "Tree" [Core.TOpq]) "$p:2:rhs")
      ----                                    :| []
      ----                                )
      ----                              :| []
      ----                          )
      ----                      )
      ----                      :| []
      ----                  )
      ----              )
      ----              <| Core.Clause
      ----                ( Label (Core.TCon "Tree" [Core.TOpq]) "Leaf"
      ----                    :| []
      ----                )
      ----                ( Core.var (Label (Core.TCon "List" [Core.TOpq]) "$Nil")
      ----                )
      ----              :| []
      ----          )
      ----      )
      --  , OFunction
      --      "qsort"
      --      [ Label (Core.TCon "Ordered" [Core.TOpq]) "$dict:1"
      --      ]
      --      ( Core.app
      --          ( Core.TCon "List" [Core.TOpq]
      --              `Core.arrow` Core.TCon "List" [Core.TOpq]
      --          )
      --          -- (<<)
      --          ( Core.var
      --              ( Label
      --                  ( (Core.TCon "Tree" [Core.TOpq] `Core.arrow` Core.TCon "List" [Core.TOpq])
      --                      `Core.arrow` (Core.TCon "List" [Core.TOpq] `Core.arrow` Core.TCon "Tree" [Core.TOpq])
      --                      `Core.arrow` (Core.TCon "List" [Core.TOpq] `Core.arrow` Core.TCon "List" [Core.TOpq])
      --                  )
      --                  "(<<)"
      --              )
      --          )
      --          ( Core.var
      --              ( Label
      --                  ( Core.TCon "Tree" [Core.TOpq]
      --                      `Core.arrow` Core.TCon "List" [Core.TOpq]
      --                  )
      --                  "flatten"
      --              )
      --              <| Core.app
      --                ( Core.TCon "List" [Core.TOpq]
      --                    `Core.arrow` Core.TCon "Tree" [Core.TOpq]
      --                )
      --                ( Core.var
      --                    ( Label
      --                        ( Core.TCon "Ordered" [Core.TOpq]
      --                            `Core.arrow` Core.TCon "List" [Core.TOpq]
      --                            `Core.arrow` Core.TCon "Tree" [Core.TOpq]
      --                        )
      --                        "from_list"
      --                    )
      --                )
      --                ( Core.var (Label (Core.TCon "Ordered" [Core.TOpq]) "$dict:1")
      --                    :| []
      --                )
      --              :| []
      --          )
      --      )
      --  ]

      ---- test3Result =
      ----  [ OFunction
      ----      "lte"
      ----      [ Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$dict:1"
      ----      , Label Core.TOpq "x"
      ----      , Label Core.TOpq "y"
      ----      ]
      ----      ( Core.match
      ----          ( Core.app
      ----              (Core.TCon "Ordering" [])
      ----              ( Core.var
      ----                  ( Label
      ----                      ( Core.TCon "Ordered" [Core.TOpq]
      ----                          `Core.arrow` Core.TOpq
      ----                          `Core.arrow` Core.TOpq
      ----                          `Core.arrow` Core.TCon "Ordering" []
      ----                      )
      ----                      "compare"
      ----                  )
      ----              )
      ----              ( Core.var (Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$dict:1")
      ----                  <| Core.var (Label Core.TOpq "x")
      ----                  <| Core.var (Label Core.TOpq "y")
      ----                  :| []
      ----              )
      ----          )
      ----          ( Core.Clause
      ----              (Label (Core.TCon "Ordering" []) "EqualTo" :| [])
      ----              (Core.lit (Core.PBool True))
      ----              <| Core.Clause
      ----                (Label (Core.TCon "Ordering" []) "GreaterThan" :| [])
      ----                (Core.lit (Core.PBool False))
      ----              <| Core.Clause
      ----                (Label (Core.TCon "Ordering" []) "LessThan" :| [])
      ----                (Core.lit (Core.PBool True))
      ----              :| []
      ----          )
      ----      )
      ----  , OFunction
      ----      "gt"
      ----      [ Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$dict:1"
      ----      , Label Core.TOpq "x"
      ----      ]
      ----      ( Core.app
      ----          (Core.TOpq `Core.arrow` Core.bool)
      ----          ( Core.var
      ----              ( Label
      ----                  ( (Core.bool `Core.arrow` Core.bool)
      ----                      `Core.arrow` (Core.TOpq `Core.arrow` Core.bool)
      ----                      `Core.arrow` (Core.TOpq `Core.arrow` Core.bool)
      ----                  )
      ----                  "(<<)"
      ----              )
      ----          )
      ----          ( Core.var (Label (Core.bool `Core.arrow` Core.bool) "not")
      ----              <| Core.app
      ----                (Core.TOpq `Core.arrow` Core.bool)
      ----                ( Core.var
      ----                    ( Label
      ----                        ( Core.TCon "Ordered" [Core.TOpq]
      ----                            `Core.arrow` Core.TOpq
      ----                            `Core.arrow` Core.TOpq
      ----                            `Core.arrow` Core.bool
      ----                        )
      ----                        "lte"
      ----                    )
      ----                )
      ----                ( Core.var (Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$dict:1")
      ----                    <| Core.var (Label Core.TOpq "x")
      ----                    :| []
      ----                )
      ----              :| []
      ----          )
      ----      )
      ----  , OFunction
      ----      "in_range"
      ----      [ Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$dict:1"
      ----      , Label (Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]) "$record:expr:1"
      ----      , Label Core.TOpq "n"
      ----      ]
      ----      ( Core.match
      ----          (Core.var (Label (Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]) "$record:expr:1"))
      ----          ( Core.Clause
      ----              ( Label
      ----                  ( Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)
      ----                      `Core.arrow` Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]
      ----                  )
      ----                  "$Record"
      ----                  <| Label (Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)) "$row"
      ----                  :| []
      ----              )
      ----              ( Core.sel
      ----                  ( Core.Focus
      ----                      "min"
      ----                      (Label Core.TOpq "min")
      ----                      (Label (Core.RExt "max" Core.TOpq Core.RNil) "$tail:1")
      ----                  )
      ----                  (Core.var (Label (Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)) "$row"))
      ----                  ( Core.sel
      ----                      ( Core.Focus
      ----                          "max"
      ----                          (Label Core.TOpq "max")
      ----                          (Label Core.RNil "$tail:2")
      ----                      )
      ----                      (Core.var (Label (Core.RExt "max" Core.TOpq Core.RNil) "$tail:1"))
      ----                      ( Core.op
      ----                          -- (&&)
      ----                          ( Core.OAnd
      ----                              ( Core.app
      ----                                  Core.bool
      ----                                  -- gt
      ----                                  (Core.var (Label (Core.TCon "Ordered" [Core.TOpq] `Core.arrow` Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.bool) "gt"))
      ----                                  ( Core.var (Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$dict:1")
      ----                                      <| Core.var (Label Core.TOpq "n")
      ----                                      <| Core.var (Label Core.TOpq "min")
      ----                                      :| []
      ----                                  )
      ----                              )
      ----                              ( Core.op
      ----                                  -- (||)
      ----                                  ( Core.OOr
      ----                                      ( Core.app
      ----                                          Core.bool
      ----                                          -- gt
      ----                                          (Core.var (Label (Core.TCon "Ordered" [Core.TOpq] `Core.arrow` Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.bool) "gt"))
      ----                                          ( Core.var (Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$dict:1")
      ----                                              <| Core.var (Label Core.TOpq "min")
      ----                                              <| Core.var (Label Core.TOpq "max")
      ----                                              :| []
      ----                                          )
      ----                                      )
      ----                                      ( Core.app
      ----                                          Core.bool
      ----                                          -- lte
      ----                                          (Core.var (Label (Core.TCon "Ordered" [Core.TOpq] `Core.arrow` Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.bool) "lte"))
      ----                                          ( Core.var (Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$dict:1")
      ----                                              <| Core.var (Label Core.TOpq "n")
      ----                                              <| Core.var (Label Core.TOpq "max")
      ----                                              :| []
      ----                                          )
      ----                                      )
      ----                                  )
      ----                              )
      ----                          )
      ----                      )
      ----                  )
      ----              )
      ----              :| []
      ----          )
      ----      )
      ----  , OFunction
      ----      "from_list"
      ----      [ Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$dict:1"
      ----      , Label (Core.TCon "List" [Core.TOpq]) "list"
      ----      ]
      ----      ( Core.app
      ----          (Core.TCon "Tree" [Core.TOpq])
      ----          ( Core.var
      ----              ( Label
      ----                  ( Core.TCon "List" [Core.TOpq]
      ----                      `Core.arrow` Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]
      ----                      `Core.arrow` Core.TCon "Tree" [Core.TOpq]
      ----                  )
      ----                  "$fold:1:a"
      ----              )
      ----          )
      ----          ( Core.var (Label (Core.TCon "List" [Core.TOpq]) "list")
      ----              <| Core.app
      ----                (Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)])
      ----                ( Core.var
      ----                    ( Label
      ----                        ( Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)
      ----                            `Core.arrow` Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]
      ----                        )
      ----                        "$Record"
      ----                    )
      ----                )
      ----                ( Core.ext
      ----                    "max"
      ----                    ( Core.app
      ----                        Core.TOpq
      ----                        (Core.var (Label (Core.TCon "Ordered" [Core.TOpq] `Core.arrow` Core.int32 `Core.arrow` Core.TOpq) "from_int32"))
      ----                        ( Core.var (Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$1")
      ----                            <| Core.lit (Core.PInt32 (-1))
      ----                            :| []
      ----                        )
      ----                    )
      ----                    ( Core.ext
      ----                        "min"
      ----                        ( Core.app
      ----                            Core.TOpq
      ----                            (Core.var (Label (Core.TCon "Ordered" [Core.TOpq] `Core.arrow` Core.int32 `Core.arrow` Core.TOpq) "from_int32"))
      ----                            ( Core.var (Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$1")
      ----                                <| Core.lit (Core.PInt32 0)
      ----                                :| []
      ----                            )
      ----                        )
      ----                        Core.nil
      ----                    )
      ----                    :| []
      ----                )
      ----              :| []
      ----          )
      ----      )
      ----  , OFunction
      ----      "$fold:1:a"
      ----      [ Label (Core.TCon "List" [Core.TOpq]) "$fold:1:expr"
      ----      ]
      ----      ( Core.match
      ----          (Core.var (Label (Core.TCon "List" [Core.TOpq]) "$fold:1:expr"))
      ----          ( Core.Clause
      ----              ( Label (Core.TOpq `Core.arrow` Core.TCon "List" [Core.TOpq] `Core.arrow` Core.TCon "List" [Core.TOpq]) "$Cons"
      ----                  <| Label Core.TOpq "$p:0:p"
      ----                  <| Label (Core.TCon "List" [Core.TOpq]) "$p:1:g"
      ----                  :| []
      ----              )
      ----              (Core.var (Label (Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)] `Core.arrow` Core.TCon "Tree" [Core.TOpq]) "$anon.1"))
      ----              <| Core.Clause
      ----                (Label (Core.TCon "List" [Core.TOpq]) "$Nil" :| [])
      ----                ( Core.var (Label (Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)] `Core.arrow` Core.TCon "Tree" [Core.TOpq]) "$anon.2")
      ----                )
      ----              :| []
      ----          )
      ----      )
      ----  , OFunction
      ----      "$anon.2"
      ----      [ Label (Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]) "_"
      ----      ]
      ----      (Core.var (Label (Core.TCon "Tree" [Core.TOpq]) "Leaf"))
      ----  , OFunction
      ----      "$anon.1"
      ----      [ Label (Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]) "range"
      ----      ]
      ----      ( Core.if_
      ----          ( Core.app
      ----              Core.bool
      ----              -- (|.)
      ----              ( Core.var
      ----                  ( Label
      ----                      ( Core.TOpq
      ----                          `Core.arrow` (Core.TOpq `Core.arrow` Core.bool)
      ----                          `Core.arrow` Core.bool
      ----                      )
      ----                      "(|.)"
      ----                  )
      ----              )
      ----              ( Core.var (Label Core.TOpq "$p:0:p")
      ----                  <| Core.app
      ----                    (Core.TOpq `Core.arrow` Core.bool)
      ----                    -- in_range
      ----                    ( Core.var
      ----                        ( Label
      ----                            ( Core.TCon "Ordered" [Core.TOpq]
      ----                                `Core.arrow` Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]
      ----                                `Core.arrow` Core.TOpq
      ----                                `Core.arrow` Core.bool
      ----                            )
      ----                            "in_range"
      ----                        )
      ----                    )
      ----                    ( Core.var (Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$dict:1")
      ----                        <| Core.var (Label (Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]) "range")
      ----                        :| []
      ----                    )
      ----                  :| []
      ----              )
      ----          )
      ----          ( Core.app
      ----              (Core.TCon "Tree" [Core.TOpq])
      ----              ( Core.var
      ----                  ( Label
      ----                      ( Core.TOpq
      ----                          `Core.arrow` Core.TCon "Tree" [Core.TOpq]
      ----                          `Core.arrow` Core.TCon "Tree" [Core.TOpq]
      ----                          `Core.arrow` Core.TCon "Tree" [Core.TOpq]
      ----                      )
      ----                      "Node"
      ----                  )
      ----              )
      ----              ( Core.var (Label Core.TOpq "$p:0:p")
      ----                  <| Core.app
      ----                    (Core.TCon "Tree" [Core.TOpq])
      ----                    ( Core.var
      ----                        ( Label
      ----                            ( Core.TCon "List" [Core.TOpq]
      ----                                `Core.arrow` Core.TCon "$Record" [Core.RExt "max" Core.int32 (Core.RExt "min" Core.int32 Core.RNil)]
      ----                                `Core.arrow` Core.TCon "Tree" [Core.TOpq]
      ----                            )
      ----                            "$fold:1:a"
      ----                        )
      ----                    )
      ----                    ( Core.var (Label (Core.TCon "List" [Core.TOpq]) "$p:1:g")
      ----                        <| Core.app
      ----                          (Core.TCon "$Record" [Core.RExt "max" Core.int32 (Core.RExt "min" Core.int32 Core.RNil)])
      ----                          ( Core.var
      ----                              ( Label
      ----                                  ( Core.RExt "max" Core.int32 (Core.RExt "min" Core.int32 Core.RNil)
      ----                                      `Core.arrow` Core.TCon "$Record" [Core.RExt "max" Core.int32 (Core.RExt "min" Core.int32 Core.RNil)]
      ----                                  )
      ----                                  "$Record"
      ----                              )
      ----                          )
      ----                          ( Core.ext
      ----                              "max"
      ----                              (Core.var (Label Core.TOpq "$p:0:p"))
      ----                              ( Core.ext
      ----                                  "min"
      ----                                  ( ( Core.match
      ----                                        (Core.var (Label (Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]) "range"))
      ----                                        ( Core.Clause
      ----                                            ( Label
      ----                                                ( Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)
      ----                                                    `Core.arrow` Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]
      ----                                                )
      ----                                                "$Record"
      ----                                                <| Label (Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)) "$row"
      ----                                                :| []
      ----                                            )
      ----                                            ( Core.sel
      ----                                                ( Core.Focus
      ----                                                    "min"
      ----                                                    (Label Core.TOpq "$f-1")
      ----                                                    (Label (Core.RExt "max" Core.TOpq Core.RNil) "_")
      ----                                                )
      ----                                                (Core.var (Label (Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)) "$row"))
      ----                                                (Core.var (Label Core.TOpq "$f-1"))
      ----                                            )
      ----                                            :| []
      ----                                        )
      ----                                    )
      ----                                  )
      ----                                  Core.nil
      ----                              )
      ----                              :| []
      ----                          )
      ----                        :| []
      ----                    )
      ----                  <| Core.app
      ----                    (Core.TCon "Tree" [Core.TOpq])
      ----                    ( Core.var
      ----                        ( Label
      ----                            ( Core.TCon "List" [Core.TOpq]
      ----                                `Core.arrow` Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]
      ----                                `Core.arrow` Core.TCon "Tree" [Core.TOpq]
      ----                            )
      ----                            "$fold:1:a"
      ----                        )
      ----                    )
      ----                    ( Core.var (Label (Core.TCon "List" [Core.TOpq]) "g")
      ----                        <| Core.app
      ----                          (Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)])
      ----                          ( Core.var
      ----                              ( Label
      ----                                  ( Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)
      ----                                      `Core.arrow` Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]
      ----                                  )
      ----                                  "$Record"
      ----                              )
      ----                          )
      ----                          ( Core.ext
      ----                              "max"
      ----                              ( ( Core.match
      ----                                    (Core.var (Label (Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]) "range"))
      ----                                    ( Core.Clause
      ----                                        ( Label
      ----                                            ( Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)
      ----                                                `Core.arrow` Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]
      ----                                            )
      ----                                            "$Record"
      ----                                            <| Label (Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)) "$row"
      ----                                            :| []
      ----                                        )
      ----                                        ( Core.sel
      ----                                            ( Core.Focus
      ----                                                "max"
      ----                                                (Label Core.TOpq "$f-1")
      ----                                                (Label (Core.RExt "min" Core.TOpq Core.RNil) "_")
      ----                                            )
      ----                                            (Core.var (Label (Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)) "$row"))
      ----                                            (Core.var (Label Core.TOpq "$f-1"))
      ----                                        )
      ----                                        :| []
      ----                                    )
      ----                                )
      ----                              )
      ----                              ( Core.ext
      ----                                  "min"
      ----                                  (Core.var (Label Core.TOpq "p"))
      ----                                  Core.nil
      ----                              )
      ----                              :| []
      ----                          )
      ----                        :| []
      ----                    )
      ----                  :| []
      ----              )
      ----          )
      ----          ( Core.app
      ----              (Core.TCon "Tree" [Core.TOpq])
      ----              ( Core.var
      ----                  ( Label
      ----                      ( Core.TCon "List" [Core.TOpq]
      ----                          `Core.arrow` Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]
      ----                          `Core.arrow` Core.TCon "Tree" [Core.TOpq]
      ----                      )
      ----                      "$fold:1:a"
      ----                  )
      ----              )
      ----              ( Core.var (Label (Core.TCon "List" [Core.TOpq]) "g")
      ----                  <| Core.var (Label (Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]) "range")
      ----                  :| []
      ----              )
      ----          )
      ----      )
      ----  , OFunction
      ----      "flatten"
      ----      [ Label (Core.TCon "Tree" [Core.TOpq]) "tree"
      ----      ]
      ----      ( Core.app
      ----          (Core.TCon "List" [Core.TOpq])
      ----          ( Core.var
      ----              ( Label
      ----                  ( Core.TCon "Tree" [Core.TOpq]
      ----                      `Core.arrow` Core.TCon "List" [Core.TOpq]
      ----                  )
      ----                  "$fold:1:b"
      ----              )
      ----          )
      ----          (Core.var (Label (Core.TCon "Tree" [Core.TOpq]) "tree") :| [])
      ----      )
      ----  , OFunction
      ----      "$fold:1:b"
      ----      [ Label (Core.TCon "Tree" [Core.TOpq]) "$fold:1:expr"
      ----      ]
      ----      ( Core.match
      ----          (Core.var (Label (Core.TCon "Tree" [Core.TOpq]) "$fold:1:expr"))
      ----          ( Core.Clause
      ----              ( Label
      ----                  ( Core.TOpq
      ----                      `Core.arrow` Core.TCon "Tree" [Core.TOpq]
      ----                      `Core.arrow` Core.TCon "Tree" [Core.TOpq]
      ----                      `Core.arrow` Core.TCon "Tree" [Core.TOpq]
      ----                  )
      ----                  "Node"
      ----                  <| Label Core.TOpq "$p:0:y"
      ----                  <| Label (Core.TCon "Tree" [Core.TOpq]) "$p:1:lhs"
      ----                  <| Label (Core.TCon "Tree" [Core.TOpq]) "$p:2:rhs"
      ----                  :| []
      ----              )
      ----              ( Core.app
      ----                  (Core.TCon "List" [Core.TOpq])
      ----                  -- (++)
      ----                  ( Core.var
      ----                      ( Label
      ----                          ( Core.TCon "List" [Core.TOpq]
      ----                              `Core.arrow` Core.TCon "List" [Core.TOpq]
      ----                              `Core.arrow` Core.TCon "List" [Core.TOpq]
      ----                          )
      ----                          "(++)"
      ----                      )
      ----                  )
      ----                  ( Core.app
      ----                      (Core.TCon "List" [Core.TOpq])
      ----                      ( Core.var
      ----                          ( Label
      ----                              ( Core.TCon "Tree" [Core.TOpq]
      ----                                  `Core.arrow` Core.TCon "List" [Core.TOpq]
      ----                              )
      ----                              "$fold:1:b"
      ----                          )
      ----                      )
      ----                      ( Core.var (Label (Core.TCon "Tree" [Core.TOpq]) "$p:1:lhs")
      ----                          :| []
      ----                      )
      ----                      <|
      ----                      -- y :: $fold:1(rhs)
      ----                      ( Core.app
      ----                          (Core.TCon "List" [Core.TOpq])
      ----                          -- (::)
      ----                          ( Core.var
      ----                              ( Label
      ----                                  ( Core.TOpq
      ----                                      `Core.arrow` Core.TCon "List" [Core.TOpq]
      ----                                      `Core.arrow` Core.TCon "List" [Core.TOpq]
      ----                                  )
      ----                                  "(::)"
      ----                              )
      ----                          )
      ----                          ( Core.var (Label Core.TOpq "y")
      ----                              <| Core.app
      ----                                (Core.TCon "List" [Core.TOpq])
      ----                                ( Core.var
      ----                                    ( Label
      ----                                        ( Core.TCon "Tree" [Core.TOpq]
      ----                                            `Core.arrow` Core.TCon "List" [Core.TOpq]
      ----                                        )
      ----                                        "$fold:1:b"
      ----                                    )
      ----                                )
      ----                                ( Core.var (Label (Core.TCon "Tree" [Core.TOpq]) "$p:2:rhs")
      ----                                    :| []
      ----                                )
      ----                              :| []
      ----                          )
      ----                      )
      ----                      :| []
      ----                  )
      ----              )
      ----              <| Core.Clause
      ----                ( Label (Core.TCon "Tree" [Core.TOpq]) "Leaf"
      ----                    :| []
      ----                )
      ----                ( Core.var (Label (Core.TCon "List" [Core.TOpq]) "$Nil")
      ----                )
      ----              :| []
      ----          )
      ----      )
      ----  , OFunction
      ----      "qsort"
      ----      [ Label (Core.TCon "Ordered" [Core.TOpq]) "$dict:1"
      ----      ]
      ----      ( Core.app
      ----          ( Core.TCon "List" [Core.TOpq]
      ----              `Core.arrow` Core.TCon "List" [Core.TOpq]
      ----          )
      ----          -- (<<)
      ----          ( Core.var
      ----              ( Label
      ----                  ( (Core.TCon "Tree" [Core.TOpq] `Core.arrow` Core.TCon "List" [Core.TOpq])
      ----                      `Core.arrow` (Core.TCon "List" [Core.TOpq] `Core.arrow` Core.TCon "Tree" [Core.TOpq])
      ----                      `Core.arrow` (Core.TCon "List" [Core.TOpq] `Core.arrow` Core.TCon "List" [Core.TOpq])
      ----                  )
      ----                  "(<<)"
      ----              )
      ----          )
      ----          ( Core.var
      ----              ( Label
      ----                  ( Core.TCon "Tree" [Core.TOpq]
      ----                      `Core.arrow` Core.TCon "List" [Core.TOpq]
      ----                  )
      ----                  "flatten"
      ----              )
      ----              <| Core.app
      ----                ( Core.TCon "List" [Core.TOpq]
      ----                    `Core.arrow` Core.TCon "Tree" [Core.TOpq]
      ----                )
      ----                ( Core.var
      ----                    ( Label
      ----                        ( Core.TCon "Ordered" [Core.TOpq]
      ----                            `Core.arrow` Core.TCon "List" [Core.TOpq]
      ----                            `Core.arrow` Core.TCon "Tree" [Core.TOpq]
      ----                        )
      ----                        "from_list"
      ----                    )
      ----                )
      ----                ( Core.var (Label (Core.TCon "Ordered" [Core.TOpq]) "$dict:1")
      ----                    :| []
      ----                )
      ----              :| []
      ----          )
      ----      )
  ]

compareField = Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" [])

fromInt32Field = Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq)

foo1 =
  [ OExternal "(<<)" Core.TOpq
  , OExternal "(|.)" Core.TOpq
  , OExternal "(++)" Core.TOpq
  , OExternal "(::)" Core.TOpq
  , OExternal "not" Core.TOpq
  , OFunction
      "lte"
      [ Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$dict:1"
      , Label Core.TOpq "x"
      , Label Core.TOpq "y"
      ]
      ( Core.match
          Core.TOpq
          (Core.var (Label (Core.TCon "$Record" [compareField (fromInt32Field Core.RNil)]) "$dict:1"))
          ( Core.Clause
              ( Label
                  ( compareField (fromInt32Field Core.RNil)
                      `Core.arrow` Core.TCon "$Record" [compareField (fromInt32Field Core.RNil)]
                  )
                  "$Record"
                  <| Label (compareField (fromInt32Field Core.RNil)) "$record:r:1"
                  :| []
              )
              ( Core.sel
                  ( Core.Focus
                      "compare"
                      ( Label
                          ( Core.TCon "Ordered" [Core.TOpq]
                              `Core.arrow` Core.TOpq
                              `Core.arrow` Core.TOpq
                              `Core.arrow` Core.TCon "Ordering" []
                          )
                          "compare"
                      )
                      (Label Core.TOpq "_")
                  )
                  (Core.var (Label (compareField (fromInt32Field Core.RNil)) "$record:r:1"))
                  ( Core.match
                      Core.TOpq
                      ( Core.app
                          (Core.TCon "Ordering" [])
                          ( Core.var
                              ( Label
                                  ( Core.TCon "Ordered" [Core.TOpq]
                                      `Core.arrow` Core.TOpq
                                      `Core.arrow` Core.TOpq
                                      `Core.arrow` Core.TCon "Ordering" []
                                  )
                                  "compare"
                              )
                          )
                          ( Core.var (Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$dict:1")
                              <| Core.var (Label Core.TOpq "x")
                              <| Core.var (Label Core.TOpq "y")
                              :| []
                          )
                      )
                      ( Core.Clause
                          (Label (Core.TCon "Ordering" []) "EqualTo" :| [])
                          (Core.lit (Core.PBool True))
                          <| Core.Clause
                            (Label (Core.TCon "Ordering" []) "GreaterThan" :| [])
                            (Core.lit (Core.PBool False))
                          <| Core.Clause
                            (Label (Core.TCon "Ordering" []) "LessThan" :| [])
                            (Core.lit (Core.PBool True))
                          :| []
                      )
                  )
              )
              :| []
          )
      )
  , OFunction
      "gt"
      [ Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$dict:1"
      , Label Core.TOpq "x"
      ]
      ( Core.app
          (Core.TOpq `Core.arrow` Core.bool)
          ( Core.var
              ( Label
                  ( (Core.bool `Core.arrow` Core.bool)
                      `Core.arrow` (Core.TOpq `Core.arrow` Core.bool)
                      `Core.arrow` (Core.TOpq `Core.arrow` Core.bool)
                  )
                  "(<<)"
              )
          )
          ( Core.var (Label (Core.bool `Core.arrow` Core.bool) "not")
              <| Core.app
                (Core.TOpq `Core.arrow` Core.bool)
                ( Core.var
                    ( Label
                        ( Core.TCon "Ordered" [Core.TOpq]
                            `Core.arrow` Core.TOpq
                            `Core.arrow` Core.TOpq
                            `Core.arrow` Core.bool
                        )
                        "lte"
                    )
                )
                ( Core.var (Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$dict:1")
                    <| Core.var (Label Core.TOpq "x")
                    :| []
                )
              :| []
          )
      )
  , OFunction
      "in_range"
      [ Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$dict:1"
      , Label (Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]) "$record:expr:1"
      , Label Core.TOpq "n"
      ]
      ( Core.match
          Core.TOpq
          (Core.var (Label (Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]) "$record:expr:1"))
          ( Core.Clause
              ( Label
                  ( Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)
                      `Core.arrow` Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]
                  )
                  "$Record"
                  <| Label (Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)) "$row"
                  :| []
              )
              ( Core.sel
                  ( Core.Focus
                      "min"
                      (Label Core.TOpq "min")
                      (Label (Core.RExt "max" Core.TOpq Core.RNil) "$tail:1")
                  )
                  (Core.var (Label (Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)) "$row"))
                  ( Core.sel
                      ( Core.Focus
                          "max"
                          (Label Core.TOpq "max")
                          (Label Core.RNil "$tail:2")
                      )
                      (Core.var (Label (Core.RExt "max" Core.TOpq Core.RNil) "$tail:1"))
                      ( Core.op
                          -- (&&)
                          ( Core.OAnd
                              ( Core.app
                                  Core.bool
                                  -- gt
                                  (Core.var (Label (Core.TCon "Ordered" [Core.TOpq] `Core.arrow` Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.bool) "gt"))
                                  ( Core.var (Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$dict:1")
                                      <| Core.var (Label Core.TOpq "n")
                                      <| Core.var (Label Core.TOpq "min")
                                      :| []
                                  )
                              )
                              ( Core.op
                                  -- (||)
                                  ( Core.OOr
                                      ( Core.app
                                          Core.bool
                                          -- gt
                                          (Core.var (Label (Core.TCon "Ordered" [Core.TOpq] `Core.arrow` Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.bool) "gt"))
                                          ( Core.var (Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$dict:1")
                                              <| Core.var (Label Core.TOpq "min")
                                              <| Core.var (Label Core.TOpq "max")
                                              :| []
                                          )
                                      )
                                      ( Core.app
                                          Core.bool
                                          -- lte
                                          (Core.var (Label (Core.TCon "Ordered" [Core.TOpq] `Core.arrow` Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.bool) "lte"))
                                          ( Core.var (Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$dict:1")
                                              <| Core.var (Label Core.TOpq "n")
                                              <| Core.var (Label Core.TOpq "max")
                                              :| []
                                          )
                                      )
                                  )
                              )
                          )
                      )
                  )
              )
              :| []
          )
      )
  , OFunction
      "from_list"
      [ Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$dict:1"
      , Label (Core.TCon "List" [Core.TOpq]) "list"
      ]
      ( Core.match
          Core.TOpq
          (Core.var (Label (Core.TCon "$Record" [compareField (fromInt32Field Core.RNil)]) "$dict:1"))
          ( Core.Clause
              ( Label
                  ( compareField (fromInt32Field Core.RNil)
                      `Core.arrow` Core.TCon "$Record" [compareField (fromInt32Field Core.RNil)]
                  )
                  "$Record"
                  <| Label (compareField (fromInt32Field Core.RNil)) "$record:r:1"
                  :| []
              )
              ( Core.sel
                  ( Core.Focus
                      "from_int32"
                      (Label (Core.TCon "Ordered" [Core.TOpq] `Core.arrow` Core.int32 `Core.arrow` Core.TOpq) "from_int32")
                      (Label Core.TOpq "_")
                  )
                  (Core.var (Label (compareField (fromInt32Field Core.RNil)) "$record:r:1"))
                  ( Core.app
                      (Core.TCon "Tree" [Core.TOpq])
                      ( Core.var
                          ( Label
                              ( Core.TCon "List" [Core.TOpq]
                                  `Core.arrow` Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]
                                  `Core.arrow` Core.TCon "Tree" [Core.TOpq]
                              )
                              "$fold:1:a"
                          )
                      )
                      ( Core.var (Label (Core.TCon "List" [Core.TOpq]) "list")
                          <| Core.app
                            (Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)])
                            ( Core.var
                                ( Label
                                    ( Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)
                                        `Core.arrow` Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]
                                    )
                                    "$Record"
                                )
                            )
                            ( Core.ext
                                (Label Core.TOpq "max")
                                ( Core.app
                                    Core.TOpq
                                    (Core.var (Label (Core.TCon "Ordered" [Core.TOpq] `Core.arrow` Core.int32 `Core.arrow` Core.TOpq) "from_int32"))
                                    ( Core.var (Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$dict:1")
                                        <| Core.lit (Core.PInt32 (-1))
                                        :| []
                                    )
                                )
                                ( Core.ext
                                    (Label Core.TOpq "min")
                                    ( Core.app
                                        Core.TOpq
                                        (Core.var (Label (Core.TCon "Ordered" [Core.TOpq] `Core.arrow` Core.int32 `Core.arrow` Core.TOpq) "from_int32"))
                                        ( Core.var (Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$dict:1")
                                            <| Core.lit (Core.PInt32 0)
                                            :| []
                                        )
                                    )
                                    Core.nil
                                )
                                :| []
                            )
                          :| []
                      )
                  )
              )
              :| []
          )
      )
  , OFunction
      "$fold:1:a"
      [ Label (Core.TCon "List" [Core.TOpq]) "$fold:1:expr"
      ]
      ( Core.match
          Core.TOpq
          (Core.var (Label (Core.TCon "List" [Core.TOpq]) "$fold:1:expr"))
          ( Core.Clause
              ( Label (Core.TOpq `Core.arrow` Core.TCon "List" [Core.TOpq] `Core.arrow` Core.TCon "List" [Core.TOpq]) "$Cons"
                  <| Label Core.TOpq "$p:0:p"
                  <| Label (Core.TCon "List" [Core.TOpq]) "$p:1:g"
                  :| []
              )
              (Core.var (Label (Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)] `Core.arrow` Core.TCon "Tree" [Core.TOpq]) "$anon:1"))
              <| Core.Clause
                (Label (Core.TCon "List" [Core.TOpq]) "$Nil" :| [])
                ( Core.var (Label (Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)] `Core.arrow` Core.TCon "Tree" [Core.TOpq]) "$anon:2")
                )
              :| []
          )
      )
  , OFunction
      "$anon:2"
      [ Label (Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]) "_"
      ]
      (Core.var (Label (Core.TCon "Tree" [Core.TOpq]) "Leaf"))
  , OFunction
      "$anon:1"
      [ Label (Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]) "range"
      ]
      ( Core.if_
          ( Core.app
              Core.bool
              -- (|.)
              ( Core.var
                  ( Label
                      ( Core.TOpq
                          `Core.arrow` (Core.TOpq `Core.arrow` Core.bool)
                          `Core.arrow` Core.bool
                      )
                      "(|.)"
                  )
              )
              ( Core.var (Label Core.TOpq "$p:0:p")
                  <| Core.app
                    (Core.TOpq `Core.arrow` Core.bool)
                    -- in_range
                    ( Core.var
                        ( Label
                            ( Core.TCon "Ordered" [Core.TOpq]
                                `Core.arrow` Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]
                                `Core.arrow` Core.TOpq
                                `Core.arrow` Core.bool
                            )
                            "in_range"
                        )
                    )
                    ( Core.var (Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$dict:1")
                        <| Core.var (Label (Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]) "range")
                        :| []
                    )
                  :| []
              )
          )
          ( Core.app
              (Core.TCon "Tree" [Core.TOpq])
              ( Core.var
                  ( Label
                      ( Core.TOpq
                          `Core.arrow` Core.TCon "Tree" [Core.TOpq]
                          `Core.arrow` Core.TCon "Tree" [Core.TOpq]
                          `Core.arrow` Core.TCon "Tree" [Core.TOpq]
                      )
                      "Node"
                  )
              )
              ( Core.var (Label Core.TOpq "$p:0:p")
                  <| Core.app
                    (Core.TCon "Tree" [Core.TOpq])
                    ( Core.var
                        ( Label
                            ( Core.TCon "List" [Core.TOpq]
                                `Core.arrow` Core.TCon "$Record" [Core.RExt "max" Core.int32 (Core.RExt "min" Core.int32 Core.RNil)]
                                `Core.arrow` Core.TCon "Tree" [Core.TOpq]
                            )
                            "$fold:1:a"
                        )
                    )
                    ( Core.var (Label (Core.TCon "List" [Core.TOpq]) "$p:1:g")
                        <| Core.app
                          (Core.TCon "$Record" [Core.RExt "max" Core.int32 (Core.RExt "min" Core.int32 Core.RNil)])
                          ( Core.var
                              ( Label
                                  ( Core.RExt "max" Core.int32 (Core.RExt "min" Core.int32 Core.RNil)
                                      `Core.arrow` Core.TCon "$Record" [Core.RExt "max" Core.int32 (Core.RExt "min" Core.int32 Core.RNil)]
                                  )
                                  "$Record"
                              )
                          )
                          ( Core.ext
                              (Label Core.TOpq "max")
                              (Core.var (Label Core.TOpq "$p:0:p"))
                              ( Core.ext
                                  (Label Core.TOpq "min")
                                  ( ( Core.match
                                        Core.TOpq
                                        (Core.var (Label (Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]) "range"))
                                        ( Core.Clause
                                            ( Label
                                                ( Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)
                                                    `Core.arrow` Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]
                                                )
                                                "$Record"
                                                <| Label (Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)) "$row"
                                                :| []
                                            )
                                            ( Core.sel
                                                ( Core.Focus
                                                    "min"
                                                    (Label Core.TOpq "$f-1")
                                                    (Label (Core.RExt "max" Core.TOpq Core.RNil) "_")
                                                )
                                                (Core.var (Label (Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)) "$row"))
                                                (Core.var (Label Core.TOpq "$f-1"))
                                            )
                                            :| []
                                        )
                                    )
                                  )
                                  Core.nil
                              )
                              :| []
                          )
                        :| []
                    )
                  <| Core.app
                    (Core.TCon "Tree" [Core.TOpq])
                    ( Core.var
                        ( Label
                            ( Core.TCon "List" [Core.TOpq]
                                `Core.arrow` Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]
                                `Core.arrow` Core.TCon "Tree" [Core.TOpq]
                            )
                            "$fold:1:a"
                        )
                    )
                    ( Core.var (Label (Core.TCon "List" [Core.TOpq]) "$p:1:g")
                        <| Core.app
                          (Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)])
                          ( Core.var
                              ( Label
                                  ( Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)
                                      `Core.arrow` Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]
                                  )
                                  "$Record"
                              )
                          )
                          ( Core.ext
                              (Label Core.TOpq "max")
                              ( ( Core.match
                                    Core.TOpq
                                    (Core.var (Label (Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]) "range"))
                                    ( Core.Clause
                                        ( Label
                                            ( Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)
                                                `Core.arrow` Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]
                                            )
                                            "$Record"
                                            <| Label (Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)) "$row"
                                            :| []
                                        )
                                        ( Core.sel
                                            ( Core.Focus
                                                "max"
                                                (Label Core.TOpq "$f-1")
                                                (Label (Core.RExt "min" Core.TOpq Core.RNil) "_")
                                            )
                                            (Core.var (Label (Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)) "$row"))
                                            (Core.var (Label Core.TOpq "$f-1"))
                                        )
                                        :| []
                                    )
                                )
                              )
                              ( Core.ext
                                  (Label Core.TOpq "min")
                                  (Core.var (Label Core.TOpq "$p:0:p"))
                                  Core.nil
                              )
                              :| []
                          )
                        :| []
                    )
                  :| []
              )
          )
          ( Core.app
              (Core.TCon "Tree" [Core.TOpq])
              ( Core.var
                  ( Label
                      ( Core.TCon "List" [Core.TOpq]
                          `Core.arrow` Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]
                          `Core.arrow` Core.TCon "Tree" [Core.TOpq]
                      )
                      "$fold:1:a"
                  )
              )
              ( Core.var (Label (Core.TCon "List" [Core.TOpq]) "$p:1:g")
                  <| Core.var (Label (Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]) "range")
                  :| []
              )
          )
      )
  , OFunction
      "flatten"
      [ Label (Core.TCon "Tree" [Core.TOpq]) "tree"
      ]
      ( Core.app
          (Core.TCon "List" [Core.TOpq])
          ( Core.var
              ( Label
                  ( Core.TCon "Tree" [Core.TOpq]
                      `Core.arrow` Core.TCon "List" [Core.TOpq]
                  )
                  "$fold:1:b"
              )
          )
          (Core.var (Label (Core.TCon "Tree" [Core.TOpq]) "tree") :| [])
      )
  , OFunction
      "$fold:1:b"
      [ Label (Core.TCon "Tree" [Core.TOpq]) "$fold:1:expr"
      ]
      ( Core.match
          Core.TOpq
          (Core.var (Label (Core.TCon "Tree" [Core.TOpq]) "$fold:1:expr"))
          ( Core.Clause
              ( Label
                  ( Core.TOpq
                      `Core.arrow` Core.TCon "Tree" [Core.TOpq]
                      `Core.arrow` Core.TCon "Tree" [Core.TOpq]
                      `Core.arrow` Core.TCon "Tree" [Core.TOpq]
                  )
                  "Node"
                  <| Label Core.TOpq "$p:0:y"
                  <| Label (Core.TCon "Tree" [Core.TOpq]) "$p:1:lhs"
                  <| Label (Core.TCon "Tree" [Core.TOpq]) "$p:2:rhs"
                  :| []
              )
              ( Core.app
                  (Core.TCon "List" [Core.TOpq])
                  -- (++)
                  ( Core.var
                      ( Label
                          ( Core.TCon "List" [Core.TOpq]
                              `Core.arrow` Core.TCon "List" [Core.TOpq]
                              `Core.arrow` Core.TCon "List" [Core.TOpq]
                          )
                          "(++)"
                      )
                  )
                  ( Core.app
                      (Core.TCon "List" [Core.TOpq])
                      ( Core.var
                          ( Label
                              ( Core.TCon "Tree" [Core.TOpq]
                                  `Core.arrow` Core.TCon "List" [Core.TOpq]
                              )
                              "$fold:1:b"
                          )
                      )
                      ( Core.var (Label (Core.TCon "Tree" [Core.TOpq]) "$p:1:lhs")
                          :| []
                      )
                      <|
                      -- y :: $fold:1(rhs)
                      ( Core.app
                          (Core.TCon "List" [Core.TOpq])
                          -- (::)
                          ( Core.var
                              ( Label
                                  ( Core.TOpq
                                      `Core.arrow` Core.TCon "List" [Core.TOpq]
                                      `Core.arrow` Core.TCon "List" [Core.TOpq]
                                  )
                                  "(::)"
                              )
                          )
                          ( Core.var (Label Core.TOpq "y")
                              <| Core.app
                                (Core.TCon "List" [Core.TOpq])
                                ( Core.var
                                    ( Label
                                        ( Core.TCon "Tree" [Core.TOpq]
                                            `Core.arrow` Core.TCon "List" [Core.TOpq]
                                        )
                                        "$fold:1:b"
                                    )
                                )
                                ( Core.var (Label (Core.TCon "Tree" [Core.TOpq]) "$p:2:rhs")
                                    :| []
                                )
                              :| []
                          )
                      )
                      :| []
                  )
              )
              <| Core.Clause
                ( Label (Core.TCon "Tree" [Core.TOpq]) "Leaf"
                    :| []
                )
                ( Core.var (Label (Core.TCon "List" [Core.TOpq]) "$Nil")
                )
              :| []
          )
      )
  , OFunction
      "qsort"
      [ Label (Core.TCon "Ordered" [Core.TOpq]) "$dict:1"
      ]
      ( Core.app
          ( Core.TCon "List" [Core.TOpq]
              `Core.arrow` Core.TCon "List" [Core.TOpq]
          )
          -- (<<)
          ( Core.var
              ( Label
                  ( (Core.TCon "Tree" [Core.TOpq] `Core.arrow` Core.TCon "List" [Core.TOpq])
                      `Core.arrow` (Core.TCon "List" [Core.TOpq] `Core.arrow` Core.TCon "Tree" [Core.TOpq])
                      `Core.arrow` (Core.TCon "List" [Core.TOpq] `Core.arrow` Core.TCon "List" [Core.TOpq])
                  )
                  "(<<)"
              )
          )
          ( Core.var
              ( Label
                  ( Core.TCon "Tree" [Core.TOpq]
                      `Core.arrow` Core.TCon "List" [Core.TOpq]
                  )
                  "flatten"
              )
              <| Core.app
                ( Core.TCon "List" [Core.TOpq]
                    `Core.arrow` Core.TCon "Tree" [Core.TOpq]
                )
                ( Core.var
                    ( Label
                        ( Core.TCon "Ordered" [Core.TOpq]
                            `Core.arrow` Core.TCon "List" [Core.TOpq]
                            `Core.arrow` Core.TCon "Tree" [Core.TOpq]
                        )
                        "from_list"
                    )
                )
                ( Core.var (Label (Core.TCon "Ordered" [Core.TOpq]) "$dict:1")
                    :| []
                )
              :| []
          )
      )
  ]

foo1Result =
  [ OExternal "(<<)" Core.TOpq
  , OExternal "(|.)" Core.TOpq
  , OExternal "(++)" Core.TOpq
  , OExternal "(::)" Core.TOpq
  , OExternal "not" Core.TOpq
  , OFunction
      "lte"
      [ Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$dict:1"
      , Label Core.TOpq "x"
      , Label Core.TOpq "y"
      ]
      ( Core.match
          Core.TOpq
          (Core.var (Label (Core.TCon "$Record" [compareField (fromInt32Field Core.RNil)]) "$dict:1"))
          ( Core.Clause
              ( Label
                  ( compareField (fromInt32Field Core.RNil)
                      `Core.arrow` Core.TCon "$Record" [compareField (fromInt32Field Core.RNil)]
                  )
                  "$Record"
                  <| Label (compareField (fromInt32Field Core.RNil)) "$record:r:1"
                  :| []
              )
              ( Core.sel
                  ( Core.Focus
                      "compare"
                      ( Label
                          ( Core.TCon "Ordered" [Core.TOpq]
                              `Core.arrow` Core.TOpq
                              `Core.arrow` Core.TOpq
                              `Core.arrow` Core.TCon "Ordering" []
                          )
                          "compare"
                      )
                      (Label Core.TOpq "_")
                  )
                  (Core.var (Label (compareField (fromInt32Field Core.RNil)) "$record:r:1"))
                  ( Core.match
                      Core.TOpq
                      ( Core.app
                          (Core.TCon "Ordering" [])
                          ( Core.var
                              ( Label
                                  ( Core.TCon "Ordered" [Core.TOpq]
                                      `Core.arrow` Core.TOpq
                                      `Core.arrow` Core.TOpq
                                      `Core.arrow` Core.TCon "Ordering" []
                                  )
                                  "compare"
                              )
                          )
                          ( Core.var (Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$dict:1")
                              <| Core.var (Label Core.TOpq "x")
                              <| Core.var (Label Core.TOpq "y")
                              :| []
                          )
                      )
                      ( Core.Clause
                          (Label (Core.TCon "Ordering" []) "EqualTo" :| [])
                          (Core.lit (Core.PBool True))
                          <| Core.Clause
                            (Label (Core.TCon "Ordering" []) "GreaterThan" :| [])
                            (Core.lit (Core.PBool False))
                          <| Core.Clause
                            (Label (Core.TCon "Ordering" []) "LessThan" :| [])
                            (Core.lit (Core.PBool True))
                          :| []
                      )
                  )
              )
              :| []
          )
      )
  , OFunction
      "gt"
      [ Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$dict:1"
      , Label Core.TOpq "x"
      ]
      ( Core.app
          (Core.TOpq `Core.arrow` Core.bool)
          ( Core.var
              ( Label
                  ( (Core.bool `Core.arrow` Core.bool)
                      `Core.arrow` (Core.TOpq `Core.arrow` Core.bool)
                      `Core.arrow` (Core.TOpq `Core.arrow` Core.bool)
                  )
                  "(<<)"
              )
          )
          ( Core.var (Label (Core.bool `Core.arrow` Core.bool) "not")
              <| Core.app
                (Core.TOpq `Core.arrow` Core.bool)
                ( Core.var
                    ( Label
                        ( Core.TCon "Ordered" [Core.TOpq]
                            `Core.arrow` Core.TOpq
                            `Core.arrow` Core.TOpq
                            `Core.arrow` Core.bool
                        )
                        "lte"
                    )
                )
                ( Core.var (Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$dict:1")
                    <| Core.var (Label Core.TOpq "x")
                    :| []
                )
              :| []
          )
      )
  , OFunction
      "in_range"
      [ Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$dict:1"
      , Label (Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]) "$record:expr:1"
      , Label Core.TOpq "n"
      ]
      ( Core.match
          Core.TOpq
          (Core.var (Label (Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]) "$record:expr:1"))
          ( Core.Clause
              ( Label
                  ( Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)
                      `Core.arrow` Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]
                  )
                  "$Record"
                  <| Label (Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)) "$row"
                  :| []
              )
              ( Core.sel
                  ( Core.Focus
                      "min"
                      (Label Core.TOpq "min")
                      (Label (Core.RExt "max" Core.TOpq Core.RNil) "$tail:1")
                  )
                  (Core.var (Label (Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)) "$row"))
                  ( Core.sel
                      ( Core.Focus
                          "max"
                          (Label Core.TOpq "max")
                          (Label Core.RNil "$tail:2")
                      )
                      (Core.var (Label (Core.RExt "max" Core.TOpq Core.RNil) "$tail:1"))
                      ( Core.op
                          -- (&&)
                          ( Core.OAnd
                              ( Core.app
                                  Core.bool
                                  -- gt
                                  (Core.var (Label (Core.TCon "Ordered" [Core.TOpq] `Core.arrow` Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.bool) "gt"))
                                  ( Core.var (Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$dict:1")
                                      <| Core.var (Label Core.TOpq "n")
                                      <| Core.var (Label Core.TOpq "min")
                                      :| []
                                  )
                              )
                              ( Core.op
                                  -- (||)
                                  ( Core.OOr
                                      ( Core.app
                                          Core.bool
                                          -- gt
                                          (Core.var (Label (Core.TCon "Ordered" [Core.TOpq] `Core.arrow` Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.bool) "gt"))
                                          ( Core.var (Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$dict:1")
                                              <| Core.var (Label Core.TOpq "min")
                                              <| Core.var (Label Core.TOpq "max")
                                              :| []
                                          )
                                      )
                                      ( Core.app
                                          Core.bool
                                          -- lte
                                          (Core.var (Label (Core.TCon "Ordered" [Core.TOpq] `Core.arrow` Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.bool) "lte"))
                                          ( Core.var (Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$dict:1")
                                              <| Core.var (Label Core.TOpq "n")
                                              <| Core.var (Label Core.TOpq "max")
                                              :| []
                                          )
                                      )
                                  )
                              )
                          )
                      )
                  )
              )
              :| []
          )
      )
  , OFunction
      "from_list"
      [ Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$dict:1"
      , Label (Core.TCon "List" [Core.TOpq]) "list"
      ]
      ( Core.match
          Core.TOpq
          (Core.var (Label (Core.TCon "$Record" [compareField (fromInt32Field Core.RNil)]) "$dict:1"))
          ( Core.Clause
              ( Label
                  ( compareField (fromInt32Field Core.RNil)
                      `Core.arrow` Core.TCon "$Record" [compareField (fromInt32Field Core.RNil)]
                  )
                  "$Record"
                  <| Label (compareField (fromInt32Field Core.RNil)) "$record:r:1"
                  :| []
              )
              ( Core.sel
                  ( Core.Focus
                      "from_int32"
                      (Label (Core.TCon "Ordered" [Core.TOpq] `Core.arrow` Core.int32 `Core.arrow` Core.TOpq) "from_int32")
                      (Label Core.TOpq "_")
                  )
                  (Core.var (Label (compareField (fromInt32Field Core.RNil)) "$record:r:1"))
                  ( Core.app
                      (Core.TCon "Tree" [Core.TOpq])
                      ( Core.var
                          ( Label
                              ( Core.TCon "$Record" [compareField (fromInt32Field Core.RNil)]
                                  `Core.arrow` Core.TCon "List" [Core.TOpq]
                                  `Core.arrow` Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]
                                  `Core.arrow` Core.TCon "Tree" [Core.TOpq]
                              )
                              "$fold:1:a"
                          )
                      )
                      ( Core.var (Label (Core.TCon "$Record" [compareField (fromInt32Field Core.RNil)]) "$dict:1")
                          <| Core.var (Label (Core.TCon "List" [Core.TOpq]) "list")
                          <| Core.app
                            (Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)])
                            ( Core.var
                                ( Label
                                    ( Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)
                                        `Core.arrow` Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]
                                    )
                                    "$Record"
                                )
                            )
                            ( Core.ext
                                (Label Core.TOpq "max")
                                ( Core.app
                                    Core.TOpq
                                    (Core.var (Label (Core.TCon "Ordered" [Core.TOpq] `Core.arrow` Core.int32 `Core.arrow` Core.TOpq) "from_int32"))
                                    ( Core.var (Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$dict:1")
                                        <| Core.lit (Core.PInt32 (-1))
                                        :| []
                                    )
                                )
                                ( Core.ext
                                    (Label Core.TOpq "min")
                                    ( Core.app
                                        Core.TOpq
                                        (Core.var (Label (Core.TCon "Ordered" [Core.TOpq] `Core.arrow` Core.int32 `Core.arrow` Core.TOpq) "from_int32"))
                                        ( Core.var (Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$dict:1")
                                            <| Core.lit (Core.PInt32 0)
                                            :| []
                                        )
                                    )
                                    Core.nil
                                )
                                :| []
                            )
                          :| []
                      )
                  )
              )
              :| []
          )
      )
  , OFunction
      "$fold:1:a"
      [ Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$dict:1"
      , Label (Core.TCon "List" [Core.TOpq]) "$fold:1:expr"
      ]
      ( Core.match
          Core.TOpq
          (Core.var (Label (Core.TCon "List" [Core.TOpq]) "$fold:1:expr"))
          ( Core.Clause
              ( Label (Core.TOpq `Core.arrow` Core.TCon "List" [Core.TOpq] `Core.arrow` Core.TCon "List" [Core.TOpq]) "$Cons"
                  <| Label Core.TOpq "$p:0:p"
                  <| Label (Core.TCon "List" [Core.TOpq]) "$p:1:g"
                  :| []
              )
              ( Core.app
                  (Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)] `Core.arrow` Core.TCon "Tree" [Core.TOpq])
                  ( Core.var
                      ( Label
                          ( (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)])
                              `Core.arrow` Core.TCon "List" [Core.TOpq]
                              `Core.arrow` Core.TOpq
                              `Core.arrow` Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]
                              `Core.arrow` Core.TCon "Tree" [Core.TOpq]
                          )
                          "$anon:1"
                      )
                  )
                  ( Core.var (Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$dict:1")
                      <| Core.var (Label (Core.TCon "List" [Core.TOpq]) "$p:1:g")
                      <| Core.var (Label Core.TOpq "$p:0:p")
                      :| []
                  )
              )
              <| Core.Clause
                (Label (Core.TCon "List" [Core.TOpq]) "$Nil" :| [])
                ( Core.var (Label (Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)] `Core.arrow` Core.TCon "Tree" [Core.TOpq]) "$anon:2")
                )
              :| []
          )
      )
  , OFunction
      "$anon:2"
      [ Label (Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]) "_"
      ]
      (Core.var (Label (Core.TCon "Tree" [Core.TOpq]) "Leaf"))
  , OFunction
      "$anon:1"
      [ Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$dict:1"
      , Label (Core.TCon "List" [Core.TOpq]) "$p:1:g"
      , Label Core.TOpq "$p:0:p"
      , Label (Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]) "range"
      ]
      ( Core.if_
          ( Core.app
              Core.bool
              -- (|.)
              ( Core.var
                  ( Label
                      ( Core.TOpq
                          `Core.arrow` (Core.TOpq `Core.arrow` Core.bool)
                          `Core.arrow` Core.bool
                      )
                      "(|.)"
                  )
              )
              ( Core.var (Label Core.TOpq "$p:0:p")
                  <| Core.app
                    (Core.TOpq `Core.arrow` Core.bool)
                    -- in_range
                    ( Core.var
                        ( Label
                            ( Core.TCon "Ordered" [Core.TOpq]
                                `Core.arrow` Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]
                                `Core.arrow` Core.TOpq
                                `Core.arrow` Core.bool
                            )
                            "in_range"
                        )
                    )
                    ( Core.var (Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$dict:1")
                        <| Core.var (Label (Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]) "range")
                        :| []
                    )
                  :| []
              )
          )
          ( Core.app
              (Core.TCon "Tree" [Core.TOpq])
              ( Core.var
                  ( Label
                      ( Core.TOpq
                          `Core.arrow` Core.TCon "Tree" [Core.TOpq]
                          `Core.arrow` Core.TCon "Tree" [Core.TOpq]
                          `Core.arrow` Core.TCon "Tree" [Core.TOpq]
                      )
                      "Node"
                  )
              )
              ( Core.var (Label Core.TOpq "$p:0:p")
                  <| Core.app
                    (Core.TCon "Tree" [Core.TOpq])
                    ( Core.var
                        ( Label
                            ( Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]
                                `Core.arrow` Core.TCon "List" [Core.TOpq]
                                `Core.arrow` Core.TCon "$Record" [Core.RExt "max" Core.int32 (Core.RExt "min" Core.int32 Core.RNil)]
                                `Core.arrow` Core.TCon "Tree" [Core.TOpq]
                            )
                            "$fold:1:a"
                        )
                    )
                    ( Core.var (Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$dict:1")
                        <| Core.var (Label (Core.TCon "List" [Core.TOpq]) "$p:1:g")
                        <| Core.app
                          (Core.TCon "$Record" [Core.RExt "max" Core.int32 (Core.RExt "min" Core.int32 Core.RNil)])
                          ( Core.var
                              ( Label
                                  ( Core.RExt "max" Core.int32 (Core.RExt "min" Core.int32 Core.RNil)
                                      `Core.arrow` Core.TCon "$Record" [Core.RExt "max" Core.int32 (Core.RExt "min" Core.int32 Core.RNil)]
                                  )
                                  "$Record"
                              )
                          )
                          ( Core.ext
                              (Label Core.TOpq "max")
                              (Core.var (Label Core.TOpq "$p:0:p"))
                              ( Core.ext
                                  (Label Core.TOpq "min")
                                  ( ( Core.match
                                        Core.TOpq
                                        (Core.var (Label (Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]) "range"))
                                        ( Core.Clause
                                            ( Label
                                                ( Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)
                                                    `Core.arrow` Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]
                                                )
                                                "$Record"
                                                <| Label (Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)) "$row"
                                                :| []
                                            )
                                            ( Core.sel
                                                ( Core.Focus
                                                    "min"
                                                    (Label Core.TOpq "$f-1")
                                                    (Label (Core.RExt "max" Core.TOpq Core.RNil) "_")
                                                )
                                                (Core.var (Label (Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)) "$row"))
                                                (Core.var (Label Core.TOpq "$f-1"))
                                            )
                                            :| []
                                        )
                                    )
                                  )
                                  Core.nil
                              )
                              :| []
                          )
                        :| []
                    )
                  <| Core.app
                    (Core.TCon "Tree" [Core.TOpq])
                    ( Core.var
                        ( Label
                            ( Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]
                                `Core.arrow` Core.TCon "List" [Core.TOpq]
                                `Core.arrow` Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]
                                `Core.arrow` Core.TCon "Tree" [Core.TOpq]
                            )
                            "$fold:1:a"
                        )
                    )
                    ( Core.var (Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$dict:1")
                        <| Core.var (Label (Core.TCon "List" [Core.TOpq]) "$p:1:g")
                        <| Core.app
                          (Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)])
                          ( Core.var
                              ( Label
                                  ( Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)
                                      `Core.arrow` Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]
                                  )
                                  "$Record"
                              )
                          )
                          ( Core.ext
                              (Label Core.TOpq "max")
                              ( ( Core.match
                                    Core.TOpq
                                    (Core.var (Label (Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]) "range"))
                                    ( Core.Clause
                                        ( Label
                                            ( Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)
                                                `Core.arrow` Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]
                                            )
                                            "$Record"
                                            <| Label (Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)) "$row"
                                            :| []
                                        )
                                        ( Core.sel
                                            ( Core.Focus
                                                "max"
                                                (Label Core.TOpq "$f-1")
                                                (Label (Core.RExt "min" Core.TOpq Core.RNil) "_")
                                            )
                                            (Core.var (Label (Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)) "$row"))
                                            (Core.var (Label Core.TOpq "$f-1"))
                                        )
                                        :| []
                                    )
                                )
                              )
                              ( Core.ext
                                  (Label Core.TOpq "min")
                                  (Core.var (Label Core.TOpq "$p:0:p"))
                                  Core.nil
                              )
                              :| []
                          )
                        :| []
                    )
                  :| []
              )
          )
          ( Core.app
              (Core.TCon "Tree" [Core.TOpq])
              ( Core.var
                  ( Label
                      ( Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]
                          `Core.arrow` Core.TCon "List" [Core.TOpq]
                          `Core.arrow` Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]
                          `Core.arrow` Core.TCon "Tree" [Core.TOpq]
                      )
                      "$fold:1:a"
                  )
              )
              ( Core.var (Label (Core.TCon "$Record" [Core.RExt "compare" (Core.TOpq `Core.arrow` Core.TOpq `Core.arrow` Core.TCon "Ordering" []) (Core.RExt "from_int32" (Core.int32 `Core.arrow` Core.TOpq) Core.RNil)]) "$dict:1")
                  <| Core.var (Label (Core.TCon "List" [Core.TOpq]) "$p:1:g")
                  <| Core.var (Label (Core.TCon "$Record" [Core.RExt "max" Core.TOpq (Core.RExt "min" Core.TOpq Core.RNil)]) "range")
                  :| []
              )
          )
      )
  , OFunction
      "flatten"
      [ Label Core.TOpq "y"
      , Label (Core.TCon "Tree" [Core.TOpq]) "tree"
      ]
      ( Core.app
          (Core.TCon "List" [Core.TOpq])
          ( Core.var
              ( Label
                  ( Core.TOpq
                      `Core.arrow` Core.TCon "Tree" [Core.TOpq]
                      `Core.arrow` Core.TCon "List" [Core.TOpq]
                  )
                  "$fold:1:b"
              )
          )
          ( Core.var (Label Core.TOpq "y")
              <| Core.var (Label (Core.TCon "Tree" [Core.TOpq]) "tree")
              :| []
          )
      )
  , OFunction
      "$fold:1:b"
      [ Label Core.TOpq "y"
      , Label (Core.TCon "Tree" [Core.TOpq]) "$fold:1:expr"
      ]
      ( Core.match
          Core.TOpq
          (Core.var (Label (Core.TCon "Tree" [Core.TOpq]) "$fold:1:expr"))
          ( Core.Clause
              ( Label
                  ( Core.TOpq
                      `Core.arrow` Core.TCon "Tree" [Core.TOpq]
                      `Core.arrow` Core.TCon "Tree" [Core.TOpq]
                      `Core.arrow` Core.TCon "Tree" [Core.TOpq]
                  )
                  "Node"
                  <| Label Core.TOpq "$p:0:y"
                  <| Label (Core.TCon "Tree" [Core.TOpq]) "$p:1:lhs"
                  <| Label (Core.TCon "Tree" [Core.TOpq]) "$p:2:rhs"
                  :| []
              )
              ( Core.app
                  (Core.TCon "List" [Core.TOpq])
                  -- (++)
                  ( Core.var
                      ( Label
                          ( Core.TCon "List" [Core.TOpq]
                              `Core.arrow` Core.TCon "List" [Core.TOpq]
                              `Core.arrow` Core.TCon "List" [Core.TOpq]
                          )
                          "(++)"
                      )
                  )
                  ( Core.app
                      (Core.TCon "List" [Core.TOpq])
                      ( Core.var
                          ( Label
                              ( Core.TOpq
                                  `Core.arrow` Core.TCon "Tree" [Core.TOpq]
                                  `Core.arrow` Core.TCon "List" [Core.TOpq]
                              )
                              "$fold:1:b"
                          )
                      )
                      ( Core.var (Label Core.TOpq "y")
                          <| Core.var (Label (Core.TCon "Tree" [Core.TOpq]) "$p:1:lhs")
                          :| []
                      )
                      <|
                      -- y :: $fold:1(rhs)
                      ( Core.app
                          (Core.TCon "List" [Core.TOpq])
                          -- (::)
                          ( Core.var
                              ( Label
                                  ( Core.TOpq
                                      `Core.arrow` Core.TCon "List" [Core.TOpq]
                                      `Core.arrow` Core.TCon "List" [Core.TOpq]
                                  )
                                  "(::)"
                              )
                          )
                          ( Core.var (Label Core.TOpq "y")
                              <| Core.app
                                (Core.TCon "List" [Core.TOpq])
                                ( Core.var
                                    ( Label
                                        ( Core.TOpq
                                            `Core.arrow` Core.TCon "Tree" [Core.TOpq]
                                            `Core.arrow` Core.TCon "List" [Core.TOpq]
                                        )
                                        "$fold:1:b"
                                    )
                                )
                                ( Core.var (Label Core.TOpq "y")
                                    <| Core.var (Label (Core.TCon "Tree" [Core.TOpq]) "$p:2:rhs")
                                    :| []
                                )
                              :| []
                          )
                      )
                      :| []
                  )
              )
              <| Core.Clause
                ( Label (Core.TCon "Tree" [Core.TOpq]) "Leaf"
                    :| []
                )
                ( Core.var (Label (Core.TCon "List" [Core.TOpq]) "$Nil")
                )
              :| []
          )
      )
  , OFunction
      "qsort"
      [ Label Core.TOpq "y"
      , Label (Core.TCon "Ordered" [Core.TOpq]) "$dict:1"
      ]
      ( Core.app
          ( Core.TCon "List" [Core.TOpq]
              `Core.arrow` Core.TCon "List" [Core.TOpq]
          )
          -- (<<)
          ( Core.var
              ( Label
                  ( (Core.TCon "Tree" [Core.TOpq] `Core.arrow` Core.TCon "List" [Core.TOpq])
                      `Core.arrow` (Core.TCon "List" [Core.TOpq] `Core.arrow` Core.TCon "Tree" [Core.TOpq])
                      `Core.arrow` (Core.TCon "List" [Core.TOpq] `Core.arrow` Core.TCon "List" [Core.TOpq])
                  )
                  "(<<)"
              )
          )
          ( Core.app
              (Core.TCon "Tree" [Core.TOpq] `Core.arrow` Core.TCon "List" [Core.TOpq])
              ( Core.var
                  ( Label
                      ( Core.TOpq
                          `Core.arrow` Core.TCon "Tree" [Core.TOpq]
                          `Core.arrow` Core.TCon "List" [Core.TOpq]
                      )
                      "flatten"
                  )
              )
              ( Core.var (Label Core.TOpq "y")
                  :| []
              )
              <| Core.app
                ( Core.TCon "List" [Core.TOpq]
                    `Core.arrow` Core.TCon "Tree" [Core.TOpq]
                )
                ( Core.var
                    ( Label
                        ( Core.TCon "Ordered" [Core.TOpq]
                            `Core.arrow` Core.TCon "List" [Core.TOpq]
                            `Core.arrow` Core.TCon "Tree" [Core.TOpq]
                        )
                        "from_list"
                    )
                )
                ( Core.var (Label (Core.TCon "Ordered" [Core.TOpq]) "$dict:1")
                    :| []
                )
              :| []
          )
      )
  ]
