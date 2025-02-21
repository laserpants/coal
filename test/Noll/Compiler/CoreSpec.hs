{-# LANGUAGE OverloadedStrings #-}

module Noll.Compiler.CoreSpec where

import Control.Monad.Identity (runIdentity)
import Control.Monad.State (evalState)
import Noll.Common.List1 (NonEmpty (..), (<|))
import Noll.Compiler.Core
import Noll.Core.Language (list, opaque, (~>))
import Noll.Core.Language.Expr (Clause (..), Focus (..))
import Noll.Label (Label (..))
import Test.Hspec (Spec, describe, it)

import qualified Noll.Core.Language as Core

spec :: Spec
spec =
  describe "Noll.Compiler.Core" $ do
    describe "" $ do
      it "" $ do
        evalState (transSuffixExpr fixture3) 0 == fixture4

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
    ( ( Label ((opaque ~> opaque) ~> (opaque ~> opaque) ~> opaque ~> opaque) "_compose_.[0]"
      , Core.lam
          ( Label (opaque ~> opaque) "f.[1]"
              :| [ Label (opaque ~> opaque) "g.[2]"
                 , Label opaque "x.[3]"
                 ]
          )
          ( Core.app
              opaque
              (Core.var (Label (opaque ~> opaque) "f.[1]"))
              ( Core.app
                  opaque
                  (Core.var (Label (opaque ~> opaque) "g.[2]"))
                  (Core.var (Label opaque "x.[3]") :| [])
                  :| []
              )
          )
      )
        :| []
    )
    ( Core.let_
        ( ( Label (list opaque ~> list opaque ~> list opaque) "_list_concat_.[4]"
          , Core.lam
              (Label (list opaque) "a.[6]" :| [Label (list opaque) "b.[7]"])
              ( Core.match
                  (list opaque)
                  (Core.var (Label (list opaque) "a.[6]"))
                  ( Clause
                      (Label (list opaque) "$Nil" :| [])
                      (Core.var (Label (list opaque) "b.[7]"))
                      :| [ Clause
                            ( Label (opaque ~> list opaque ~> list opaque) "$Cons"
                                <| Label opaque "x.[8]"
                                <| Label (list opaque) "xs.[9]"
                                :| []
                            )
                            ( Core.app
                                (list opaque)
                                (Core.var (Label (opaque ~> list opaque ~> list opaque) "$Cons"))
                                ( Core.var (Label opaque "x.[8]")
                                    <| Core.app
                                      (list opaque)
                                      (Core.var (Label (list opaque ~> list opaque ~> list opaque) "_list_concat_.[4]"))
                                      ( Core.var (Label (list opaque) "xs.[9]")
                                          <| Core.var (Label (list opaque) "b.[7]")
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
                 ( Label (compareDict ~> opaque ~> opaque ~> ordering) "compare.[5]"
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
                                <| Label compareRow "r_1.[15]"
                                :| []
                            )
                            ( Core.sel
                                ( Focus
                                    "compare"
                                    (Label (opaque ~> opaque ~> ordering) "f_1.[13]")
                                    (Label opaque "q_1.[14]")
                                )
                                (Core.var (Label compareRow "r_1.[15]"))
                                ( Core.app
                                    ordering
                                    (Core.var (Label (opaque ~> opaque ~> ordering) "f_1.[13]"))
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
