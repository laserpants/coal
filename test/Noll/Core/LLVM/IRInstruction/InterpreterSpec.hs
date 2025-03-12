{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Noll.Core.LLVM.IRInstruction.InterpreterSpec where

import Control.Monad.Reader (local)
import Control.Monad.Writer (listen)
import Data.Tuple.Extra (thd3)
import Noll.Common.Environment (Environment)
import Noll.Common.List1 (NonEmpty (..), (<|))
import Noll.Core.Compiler
import Noll.Core.Compiler.Pipeline (runPipeline)
import Noll.Core.Compiler.Pipeline.Kernel (Kernel (..))
import Noll.Core.LLVM.IRConstruct (IRConstruct (..))
import Noll.Core.LLVM.IREncodable (IREncodable (..))
import Noll.Core.LLVM.IREval.Closure.Apply (irClosureApply)
import Noll.Core.LLVM.IREval.Closure.Extend (irClosureExtend)
import Noll.Core.LLVM.IREval.Closure.Finalize (irClosureFinalize)
import Noll.Core.LLVM.IREval.Expr (irEvalExpr)
import Noll.Core.LLVM.IRInstruction.TH
import Noll.Core.LLVM.IRInterpreter
import Noll.Core.LLVM.IRInterpreter.Environment
import Noll.Core.LLVM.IRInterpreter.Monad
import Noll.Core.LLVM.IRType
import Noll.Core.LLVM.IRType.Syntax
import Noll.Core.LLVM.IRValue
import Noll.Core.Language (list, opaque, (~>))
import Noll.Core.Language.Object (Object (..), ObjectList, objectName)
import Noll.Core.Parser.Expr (expr)
import Noll.Label (Label (..))
import Noll.Utils (Name)
import Test.Hspec (Spec, describe, it)
import Text.Megaparsec (runParser)
import TextShow (showt)

import qualified Data.Text.IO as Text
import qualified Noll.Common.Environment as Environment
import qualified Noll.Core.Language as Core

spec :: Spec
spec =
  undefined

blockObjects :: ObjectList
blockObjects =
  [ OFunction
      "main"
      [Label Core.opaque "_"]
      ( Core.let_
          ( Core.Binding
              (Label Core.int32 "one")
              (Core.lit (Core.PInt32 1))
              :| [ Core.Binding
                    (Label (Core.int32 `Core.arrow` Core.int32) "f")
                    ( Core.lam
                        (Label Core.int32 "x" :| [])
                        ( Core.if_
                            ( Core.op
                                ( Core.OEqInt32
                                    (Core.var (Label Core.int32 "x"))
                                    (Core.lit (Core.PInt32 0))
                                )
                            )
                            (Core.var (Label Core.int32 "one"))
                            ( Core.let_
                                ( Core.Binding
                                    (Label Core.int32 "m")
                                    ( Core.op
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
                    )
                 ]
          )
          ( Core.app
              Core.int32
              (Core.var (Label (Core.int32 `Core.arrow` Core.int32) "f"))
              (Core.lit (Core.PInt32 6) :| [])
          )
      )
  ]

blockObjects2 :: ObjectList
blockObjects2 =
  [ OFunction
      "main"
      [Label Core.opaque "_"]
      ( Core.let_
          ( Core.Binding
              (Label ((opaque ~> opaque) ~> (opaque ~> opaque) ~> opaque ~> opaque) "_compose_")
              ( Core.lam
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
              ( Core.Binding
                  (Label (Core.int32 ~> Core.int32) "f")
                  ( Core.lam
                      (Label Core.int32 "x" :| [])
                      ( Core.op
                          ( Core.OAddInt32
                              (Core.var (Label Core.int32 "x"))
                              (Core.lit (Core.PInt32 1))
                          )
                      )
                  )
                  <| Core.Binding
                    (Label (Core.int32 ~> Core.int32) "g")
                    ( Core.lam
                        (Label Core.int32 "x" :| [])
                        ( Core.op
                            ( Core.OAddInt32
                                (Core.var (Label Core.int32 "x"))
                                (Core.lit (Core.PInt32 3))
                            )
                        )
                    )
                  <| Core.Binding
                    (Label (Core.int32 ~> Core.int32) "h")
                    ( Core.app
                        (Core.int32 ~> Core.int32)
                        (Core.var (Label ((Core.int32 ~> Core.int32) ~> (Core.int32 ~> Core.int32) ~> Core.int32 ~> Core.int32) "_compose_"))
                        ( Core.var (Label (Core.int32 ~> Core.int32) "f")
                            <| Core.var (Label (Core.int32 ~> Core.int32) "g")
                            :| []
                        )
                    )
                  :| []
              )
              ( Core.let_
                  ( Core.Binding
                      (Label Core.int32 "x")
                      ( Core.app
                          Core.int32
                          (Core.var (Label (Core.int32 ~> Core.int32) "h"))
                          (Core.lit (Core.PInt32 7) :| [])
                      )
                      :| []
                  )
                  ( Core.call
                      (Label (Core.int32 `Core.arrow` Core.TOpq) "print_int32")
                      [Core.var (Label Core.int32 "x")]
                      ( Core.lam
                          (Label Core.TOpq "_" :| [])
                          (Core.var (Label Core.int32 "x"))
                      )
                  )
              )
          )
      )
  ]

blockObjects3 :: ObjectList
blockObjects3 =
  [ OFunction
      "main"
      [Label Core.opaque "_"]
      ( Core.let_
          ( Core.Binding
              (Label (list Core.int32) "xs")
              ( Core.app
                  (list Core.int32)
                  (Core.var (Label (Core.int32 ~> list Core.int32 ~> list Core.int32) "$Cons"))
                  ( Core.lit (Core.PInt32 2)
                      <| Core.app
                        (list Core.int32)
                        (Core.var (Label (Core.int32 ~> list Core.int32 ~> list Core.int32) "$Cons"))
                        ( Core.lit (Core.PInt32 105)
                            <| Core.app
                              (list Core.int32)
                              (Core.var (Label (Core.int32 ~> list Core.int32 ~> list Core.int32) "$Cons"))
                              ( Core.lit (Core.PInt32 103)
                                  <| Core.app
                                    (list Core.int32)
                                    (Core.var (Label (Core.int32 ~> list Core.int32 ~> list Core.int32) "$Cons"))
                                    ( Core.lit (Core.PInt32 104)
                                        <| Core.app
                                          (list Core.int32)
                                          (Core.var (Label (Core.int32 ~> list Core.int32 ~> list Core.int32) "$Cons"))
                                          ( Core.lit (Core.PInt32 2)
                                              <| Core.app
                                                (list Core.int32)
                                                (Core.var (Label (Core.int32 ~> list Core.int32 ~> list Core.int32) "$Cons"))
                                                ( Core.lit (Core.PInt32 106)
                                                    :| [Core.var (Label (list Core.int32) "$Nil")]
                                                )
                                              :| []
                                          )
                                        :| []
                                    )
                                  :| []
                              )
                            :| []
                        )
                      :| []
                  )
              )
              :| []
          )
          ( Core.match
              Core.int32
              (Core.var (Label (list Core.int32) "xs"))
              ( Core.Clause
                  (Label (Core.int32 ~> list Core.int32 ~> list Core.int32) "$Cons" <| Label Core.int32 "a" <| Label (list Core.int32) "b" :| [])
                  ( Core.match
                      Core.int32
                      (Core.var (Label (list Core.int32) "b"))
                      ( Core.Clause
                          (Label (Core.int32 ~> list Core.int32 ~> list Core.int32) "$Cons" <| Label Core.int32 "c" <| Label (list Core.int32) "d" :| [])
                          ( Core.match
                              Core.int32
                              (Core.var (Label (list Core.int32) "d"))
                              ( Core.Clause
                                  (Label (Core.int32 ~> list Core.int32 ~> list Core.int32) "$Cons" <| Label Core.int32 "e" <| Label (list Core.int32) "f" :| [])
                                  ( Core.call
                                      (Label (Core.int32 `Core.arrow` Core.TOpq) "print_int32")
                                      [Core.var (Label Core.int32 "e")]
                                      ( Core.lam
                                          (Label Core.TOpq "_" :| [])
                                          (Core.var (Label Core.int32 "e"))
                                      )
                                  )
                                  :| []
                              )
                          )
                          :| []
                      )
                  )
                  :| []
              )
          )
      )
  ]

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

blockObjects4 :: ObjectList
blockObjects4 =
  [ OFunction
      "main"
      [Label Core.opaque "_"]
      ( Core.let_
          ( Core.Binding
              (Label ((opaque ~> opaque) ~> (opaque ~> opaque) ~> opaque ~> opaque) "_compose_")
              ( Core.lam
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
              ( Core.Binding
                  (Label (list opaque ~> list opaque ~> list opaque) "_list_concat_")
                  ( Core.lam
                      (Label (list opaque) "a" :| [Label (list opaque) "b"])
                      ( Core.match
                          (list opaque)
                          (Core.var (Label (list opaque) "a"))
                          ( Core.Clause
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
                              <| Core.Clause
                                (Label (list opaque) "$Nil" :| [])
                                (Core.var (Label (list opaque) "b"))
                              :| []
                          )
                      )
                  )
                  :| [ Core.Binding
                        (Label (compareDict ~> opaque ~> opaque ~> ordering) "compare")
                        ( Core.lam
                            ( Label compareDict "a_1"
                                <| Label opaque "a_2"
                                <| Label opaque "a_3"
                                :| []
                            )
                            ( Core.match
                                ordering
                                (Core.var (Label compareDict "a_1"))
                                ( Core.Clause
                                    ( Label (compareRow ~> compareDict) "$Record"
                                        <| Label compareRow "r_1"
                                        :| []
                                    )
                                    ( Core.sel
                                        ( Core.Focus
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
                     , Core.Binding
                        (Label (fromInt32Dict ~> Core.int32 ~> opaque) "from_int32")
                        ( Core.lam
                            ( Label fromInt32Dict "a_1"
                                <| Label Core.int32 "a_2"
                                :| []
                            )
                            ( Core.match
                                opaque
                                (Core.var (Label fromInt32Dict "a_1"))
                                ( Core.Clause
                                    ( Label (fromInt32Row ~> fromInt32Dict) "$Record"
                                        <| Label fromInt32Row "r_1"
                                        :| []
                                    )
                                    ( Core.sel
                                        ( Core.Focus
                                            "from_int32"
                                            (Label (Core.int32 ~> opaque) "f_1")
                                            (Label opaque "q_1")
                                        )
                                        (Core.var (Label fromInt32Row "r_1"))
                                        ( Core.app
                                            opaque
                                            (Core.var (Label (Core.int32 ~> opaque) "f_1"))
                                            (Core.var (Label Core.int32 "a_2") :| [])
                                        )
                                    )
                                    :| []
                                )
                            )
                        )
                     , Core.Binding
                        (Label (opaque ~> (opaque ~> opaque) ~> opaque) "_forward_application_")
                        ( Core.lam
                            (Label opaque "x" <| Label (opaque ~> opaque) "f" :| [])
                            ( Core.app
                                opaque
                                (Core.var (Label (opaque ~> opaque) "f"))
                                (Core.var (Label opaque "x") :| [])
                            )
                        )
                     , Core.Binding
                        (Label (Core.bool ~> Core.bool) "_not_")
                        ( Core.lam
                            (Label Core.bool "a" :| [])
                            ( Core.if_
                                (Core.var (Label Core.bool "a"))
                                (Core.lit (Core.PBool False))
                                (Core.lit (Core.PBool True))
                            )
                        )
                     , Core.Binding
                        (Label (Core.int32 ~> Core.int32 ~> ordering) "compare__int32")
                        ( Core.lam
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
                        )
                     , Core.Binding
                        (Label (Core.int32 ~> Core.int32) "from_int32__int32")
                        ( Core.lam
                            (Label Core.int32 "n" :| [])
                            (Core.var (Label Core.int32 "n"))
                        )
                     , Core.Binding
                        (Label (compareDict ~> opaque ~> opaque ~> Core.bool) "lte")
                        ( Core.lam
                            (Label compareDict "d_1" :| [])
                            ( Core.lam
                                (Label opaque "x" :| [])
                                ( Core.lam
                                    (Label opaque "y" :| [])
                                    ( Core.match
                                        Core.bool
                                        ( Core.app
                                            ordering
                                            (Core.var (Label (compareDict ~> opaque ~> opaque ~> ordering) "compare"))
                                            ( Core.var (Label compareDict "d_1")
                                                <| Core.var (Label opaque "x")
                                                <| Core.var (Label opaque "y")
                                                :| []
                                            )
                                        )
                                        ( Core.Clause
                                            (Label ordering "EqualTo" :| [])
                                            (Core.lit (Core.PBool True))
                                            <| Core.Clause
                                              (Label ordering "GreaterThan" :| [])
                                              (Core.lit (Core.PBool False))
                                            <| Core.Clause
                                              (Label ordering "LessThan" :| [])
                                              (Core.lit (Core.PBool True))
                                            :| []
                                        )
                                    )
                                )
                            )
                        )
                     , Core.Binding
                        (Label (compareDict ~> opaque ~> opaque ~> Core.bool) "gt")
                        ( Core.lam
                            (Label compareDict "d_1" :| [])
                            ( Core.lam
                                (Label opaque "x" :| [])
                                ( Core.app
                                    (opaque ~> Core.bool)
                                    (Core.var (Label ((Core.bool ~> Core.bool) ~> (opaque ~> Core.bool) ~> opaque ~> Core.bool) "_compose_"))
                                    ( Core.var (Label (Core.bool ~> Core.bool) "_not_")
                                        :| [ Core.app
                                              (opaque ~> Core.bool)
                                              (Core.var (Label (compareDict ~> opaque ~> opaque ~> Core.bool) "lte"))
                                              ( Core.var (Label compareDict "d_1")
                                                  :| [ Core.var (Label opaque "x")
                                                     ]
                                              )
                                           ]
                                    )
                                )
                            )
                        )
                     , Core.Binding
                        (Label (compareDict ~> maxMinRecord opaque ~> opaque ~> Core.bool) "in_range")
                        ( Core.lam
                            (Label compareDict "d_1" :| [])
                            ( Core.lam
                                (Label (maxMinRecord opaque) "range" <| Label opaque "n" :| [])
                                ( Core.match
                                    Core.bool
                                    (Core.var (Label (maxMinRecord opaque) "range"))
                                    ( Core.Clause
                                        ( Label (maxMinRow opaque ~> maxMinRecord opaque) "$Record"
                                            <| Label (maxMinRow opaque) "row_1"
                                            :| []
                                        )
                                        ( Core.sel
                                            ( Core.Focus
                                                "min"
                                                (Label opaque "min")
                                                (Label (Core.RExt "max" opaque opaque) "row_2")
                                            )
                                            (Core.var (Label (maxMinRow opaque) "row_1"))
                                            ( Core.sel
                                                ( Core.Focus
                                                    "max"
                                                    (Label opaque "max")
                                                    (Label opaque "z")
                                                )
                                                (Core.var (Label (Core.RExt "max" opaque opaque) "row_2"))
                                                ( Core.op
                                                    ( Core.OAnd
                                                        ( Core.app
                                                            Core.bool
                                                            (Core.var (Label (compareDict ~> opaque ~> opaque ~> Core.bool) "gt"))
                                                            ( Core.var (Label compareDict "d_1")
                                                                <| Core.var (Label opaque "n")
                                                                <| Core.var (Label opaque "min")
                                                                :| []
                                                            )
                                                        )
                                                        ( Core.op
                                                            ( Core.OOr
                                                                ( Core.app
                                                                    Core.bool
                                                                    (Core.var (Label (compareDict ~> opaque ~> opaque ~> Core.bool) "gt"))
                                                                    ( Core.var (Label compareDict "d_1")
                                                                        <| Core.var (Label opaque "min")
                                                                        <| Core.var (Label opaque "max")
                                                                        :| []
                                                                    )
                                                                )
                                                                ( Core.app
                                                                    Core.bool
                                                                    (Core.var (Label (compareDict ~> opaque ~> opaque ~> Core.bool) "lte"))
                                                                    ( Core.var (Label compareDict "d_1")
                                                                        <| Core.var (Label opaque "n")
                                                                        <| Core.var (Label opaque "max")
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
                            )
                        )
                     , Core.Binding
                        (Label (orderedDict ~> list opaque ~> tree opaque) "from_list")
                        ( Core.lam
                            (Label orderedDict "d_1" :| [])
                            ( Core.lam
                                (Label (list opaque) "list" :| [])
                                ( Core.let_
                                    ( Core.Binding
                                        (Label (list opaque ~> maxMinRecord opaque ~> tree opaque) "fold_")
                                        ( Core.lam
                                            (Label (list opaque) "a_0" :| [])
                                            ( Core.match
                                                (maxMinRecord opaque ~> tree opaque)
                                                (Core.var (Label (list opaque) "a_0"))
                                                ( Core.Clause
                                                    ( Label (opaque ~> list opaque ~> list opaque) "$Cons"
                                                        <| Label opaque "p"
                                                        <| Label (list opaque) "g"
                                                        :| []
                                                    )
                                                    ( Core.lam
                                                        (Label (maxMinRecord opaque) "range" :| [])
                                                        ( Core.if_
                                                            ( Core.app
                                                                Core.bool
                                                                (Core.var (Label (opaque ~> (opaque ~> opaque) ~> opaque) "_forward_application_"))
                                                                ( Core.var (Label opaque "p")
                                                                    <| Core.app
                                                                      (opaque ~> Core.bool)
                                                                      (Core.var (Label (compareDict ~> maxMinRecord opaque ~> opaque ~> Core.bool) "in_range"))
                                                                      ( Core.var (Label compareDict "d_1")
                                                                          <| Core.var (Label (maxMinRecord opaque) "range")
                                                                          :| []
                                                                      )
                                                                    :| []
                                                                )
                                                            )
                                                            -- then
                                                            ( Core.app
                                                                (tree opaque)
                                                                (Core.var (Label (opaque ~> tree opaque ~> tree opaque ~> tree opaque) "Node"))
                                                                ( Core.var (Label opaque "p")
                                                                    <| Core.app
                                                                      (tree opaque)
                                                                      (Core.var (Label (list opaque ~> maxMinRecord opaque ~> tree opaque) "fold_"))
                                                                      ( Core.var (Label (list opaque) "g")
                                                                          <| Core.app
                                                                            (maxMinRecord opaque)
                                                                            (Core.var (Label (maxMinRow opaque ~> maxMinRecord opaque) "$Record"))
                                                                            ( Core.ext
                                                                                "min"
                                                                                ( Core.match
                                                                                    opaque
                                                                                    (Core.var (Label (maxMinRecord opaque) "range"))
                                                                                    ( Core.Clause
                                                                                        ( Label (maxMinRow opaque ~> maxMinRecord opaque) "$Record"
                                                                                            <| Label (maxMinRow opaque) "row_2"
                                                                                            :| []
                                                                                        )
                                                                                        ( Core.sel
                                                                                            ( Core.Focus
                                                                                                "min"
                                                                                                (Label opaque "m_1")
                                                                                                (Label (Core.RExt "max" opaque opaque) "q_2")
                                                                                            )
                                                                                            (Core.var (Label (maxMinRow opaque) "row_2"))
                                                                                            (Core.var (Label opaque "m_1"))
                                                                                        )
                                                                                        :| []
                                                                                    )
                                                                                )
                                                                                ( Core.ext
                                                                                    "max"
                                                                                    (Core.var (Label opaque "p"))
                                                                                    Core.nil
                                                                                )
                                                                                :| []
                                                                            )
                                                                          :| []
                                                                      )
                                                                    <| Core.app
                                                                      (tree opaque)
                                                                      (Core.var (Label (list opaque ~> maxMinRecord opaque ~> tree opaque) "fold_"))
                                                                      ( Core.var (Label (list opaque) "g")
                                                                          <| Core.app
                                                                            (maxMinRecord opaque)
                                                                            (Core.var (Label (maxMinRow opaque ~> maxMinRecord opaque) "$Record"))
                                                                            ( Core.ext
                                                                                "min"
                                                                                (Core.var (Label opaque "p"))
                                                                                ( Core.ext
                                                                                    "max"
                                                                                    ( Core.match
                                                                                        opaque
                                                                                        (Core.var (Label (maxMinRecord opaque) "range"))
                                                                                        ( Core.Clause
                                                                                            ( Label (maxMinRow opaque ~> maxMinRecord opaque) "$Record"
                                                                                                <| Label (maxMinRow opaque) "row_2"
                                                                                                :| []
                                                                                            )
                                                                                            ( Core.sel
                                                                                                ( Core.Focus
                                                                                                    "max"
                                                                                                    (Label opaque "m_1")
                                                                                                    (Label (Core.RExt "min" opaque opaque) "q_2")
                                                                                                )
                                                                                                (Core.var (Label (maxMinRow opaque) "row_2"))
                                                                                                (Core.var (Label opaque "m_1"))
                                                                                            )
                                                                                            :| []
                                                                                        )
                                                                                    )
                                                                                    Core.nil
                                                                                )
                                                                                :| []
                                                                            )
                                                                          :| []
                                                                      )
                                                                    :| []
                                                                )
                                                            )
                                                            -- else
                                                            ( Core.app
                                                                (tree opaque)
                                                                (Core.var (Label (list opaque ~> maxMinRecord opaque ~> tree opaque) "fold_"))
                                                                ( Core.var (Label (list opaque) "g")
                                                                    <| Core.var (Label (maxMinRecord opaque) "range")
                                                                    :| []
                                                                )
                                                            )
                                                        )
                                                    )
                                                    <| Core.Clause
                                                      (Label (list opaque) "$Nil" :| [])
                                                      ( Core.lam
                                                          (Label (maxMinRecord opaque) "_" :| [])
                                                          (Core.var (Label (tree opaque) "Leaf"))
                                                      )
                                                    :| []
                                                )
                                            )
                                        )
                                        :| []
                                    )
                                    ( Core.app
                                        (tree opaque)
                                        (Core.var (Label (list opaque ~> maxMinRecord opaque ~> tree opaque) "fold_"))
                                        ( Core.var (Label (list opaque) "list")
                                            <| Core.app
                                              (maxMinRecord opaque)
                                              (Core.var (Label (maxMinRow opaque ~> maxMinRecord opaque) "$Record"))
                                              ( Core.ext
                                                  "min"
                                                  ( Core.app
                                                      opaque
                                                      (Core.var (Label (orderedDict ~> Core.int32 ~> opaque) "from_int32"))
                                                      ( Core.var (Label orderedDict "d_1")
                                                          <| Core.lit (Core.PInt32 0)
                                                          :| []
                                                      )
                                                  )
                                                  ( Core.ext
                                                      "max"
                                                      ( Core.app
                                                          opaque
                                                          (Core.var (Label (orderedDict ~> Core.int32 ~> opaque) "from_int32"))
                                                          ( Core.var (Label orderedDict "d_1")
                                                              <| Core.lit (Core.PInt32 (-1))
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
                            )
                        )
                     , Core.Binding
                        (Label (tree opaque ~> list opaque) "flatten")
                        ( Core.lam
                            (Label (tree opaque) "tree" :| [])
                            ( Core.let_
                                ( Core.Binding
                                    (Label (tree opaque ~> list opaque) "fold_")
                                    ( Core.lam
                                        (Label (tree opaque) "a_0" :| [])
                                        ( Core.match
                                            (list opaque)
                                            (Core.var (Label (tree opaque) "a_0"))
                                            ( Core.Clause
                                                (Label (tree opaque) "Leaf" :| [])
                                                (Core.var (Label (list opaque) "$Nil"))
                                                <| Core.Clause
                                                  ( Label (opaque ~> tree opaque ~> tree opaque ~> tree opaque) "Node"
                                                      <| Label opaque "y"
                                                      <| Label (tree opaque) "lhs"
                                                      <| Label (tree opaque) "rhs"
                                                      :| []
                                                  )
                                                  ( Core.app
                                                      (list opaque)
                                                      (Core.var (Label (list opaque ~> list opaque ~> list opaque) "_list_concat_"))
                                                      ( Core.app
                                                          (list opaque)
                                                          (Core.var (Label (tree opaque ~> list opaque) "fold_"))
                                                          (Core.var (Label (tree opaque) "lhs") :| [])
                                                          <| Core.app
                                                            (list opaque)
                                                            (Core.var (Label (opaque ~> list opaque ~> list opaque) "$Cons"))
                                                            ( Core.var (Label opaque "y")
                                                                <| Core.app
                                                                  (list opaque)
                                                                  (Core.var (Label (tree opaque ~> list opaque) "fold_"))
                                                                  (Core.var (Label (tree opaque) "rhs") :| [])
                                                                :| []
                                                            )
                                                          :| []
                                                      )
                                                  )
                                                :| []
                                            )
                                        )
                                    )
                                    :| []
                                )
                                ( Core.app
                                    (list opaque)
                                    (Core.var (Label (tree opaque ~> list opaque) "fold_"))
                                    (Core.var (Label (tree opaque) "tree") :| [])
                                )
                            )
                        )
                     , Core.Binding
                        (Label (orderedDict ~> list opaque ~> list opaque) "qsort")
                        ( Core.lam
                            (Label orderedDict "d_1" :| [])
                            ( Core.app
                                (list opaque ~> list opaque)
                                ( Core.var
                                    ( Label
                                        ( (tree opaque ~> list opaque)
                                            ~> (list opaque ~> tree opaque)
                                            ~> list opaque
                                            ~> list opaque
                                        )
                                        "_compose_"
                                    )
                                )
                                ( Core.var (Label (tree opaque ~> list opaque) "flatten")
                                    <| Core.app
                                      (list opaque ~> tree opaque)
                                      (Core.var (Label (orderedDict ~> list opaque ~> tree opaque) "from_list"))
                                      (Core.var (Label orderedDict "d_1") :| [])
                                    :| []
                                )
                            )
                        )
                     ]
              )
              ( Core.let_
                  ( Core.Binding
                      (Label (list Core.int32) "xs")
                      ( Core.app
                          (list Core.int32)
                          (Core.var (Label (Core.int32 ~> list Core.int32 ~> list Core.int32) "$Cons"))
                          ( Core.lit (Core.PInt32 2)
                              <| Core.app
                                (list Core.int32)
                                (Core.var (Label (Core.int32 ~> list Core.int32 ~> list Core.int32) "$Cons"))
                                ( Core.lit (Core.PInt32 105)
                                    <| Core.app
                                      (list Core.int32)
                                      (Core.var (Label (Core.int32 ~> list Core.int32 ~> list Core.int32) "$Cons"))
                                      ( Core.lit (Core.PInt32 103)
                                          <| Core.app
                                            (list Core.int32)
                                            (Core.var (Label (Core.int32 ~> list Core.int32 ~> list Core.int32) "$Cons"))
                                            ( Core.lit (Core.PInt32 104)
                                                <| Core.app
                                                  (list Core.int32)
                                                  (Core.var (Label (Core.int32 ~> list Core.int32 ~> list Core.int32) "$Cons"))
                                                  ( Core.lit (Core.PInt32 2)
                                                      <| Core.app
                                                        (list Core.int32)
                                                        (Core.var (Label (Core.int32 ~> list Core.int32 ~> list Core.int32) "$Cons"))
                                                        ( Core.lit (Core.PInt32 106)
                                                            :| [Core.var (Label (list Core.int32) "$Nil")]
                                                        )
                                                      :| []
                                                  )
                                                :| []
                                            )
                                          :| []
                                      )
                                    :| []
                                )
                              :| []
                          )
                      )
                      :| []
                  )
                  ( Core.let_
                      ( Core.Binding
                          (Label (list Core.int32) "ys")
                          ( Core.app
                              (list Core.int32)
                              (Core.var (Label (orderedInt32Dict ~> list Core.int32 ~> list Core.int32) "qsort"))
                              ( Core.app
                                  orderedInt32Dict
                                  (Core.var (Label (orderedInt32Row ~> orderedInt32Dict) "$Record"))
                                  ( Core.ext
                                      "compare"
                                      (Core.var (Label (Core.int32 ~> Core.int32 ~> ordering) "compare__int32"))
                                      ( Core.ext
                                          "from_int32"
                                          (Core.var (Label (Core.int32 ~> Core.int32) "from_int32__int32"))
                                          Core.nil
                                      )
                                      :| []
                                  )
                                  <| Core.var (Label (list Core.int32) "xs")
                                  :| []
                              )
                          )
                          :| []
                      )
                      ( Core.match
                          Core.int32
                          (Core.var (Label (list Core.int32) "ys"))
                          ( Core.Clause
                              (Label (Core.int32 ~> list Core.int32 ~> list Core.int32) "$Cons" <| Label Core.int32 "a" <| Label (list Core.int32) "b" :| [])
                              ( Core.match
                                  Core.int32
                                  (Core.var (Label (list Core.int32) "b"))
                                  ( Core.Clause
                                      (Label (Core.int32 ~> list Core.int32 ~> list Core.int32) "$Cons" <| Label Core.int32 "c" <| Label (list Core.int32) "d" :| [])
                                      ( Core.match
                                          Core.int32
                                          (Core.var (Label (list Core.int32) "d"))
                                          ( Core.Clause
                                              (Label (Core.int32 ~> list Core.int32 ~> list Core.int32) "$Cons" <| Label Core.int32 "e" <| Label (list Core.int32) "f" :| [])
                                              ( Core.call
                                                  (Label (Core.int32 `Core.arrow` Core.TOpq) "print_int32")
                                                  [Core.var (Label Core.int32 "e")]
                                                  ( Core.lam
                                                      (Label Core.TOpq "_" :| [])
                                                      (Core.var (Label Core.int32 "e"))
                                                  )
                                              )
                                              :| []
                                          )
                                      )
                                      :| []
                                  )
                              )
                              :| []
                          )
                      )
                  )
              )
          )
      )
  ]

---- main
-- fixture1 =
--  Core.let_
--    ( Core.Binding
--        (Label Core.int32 "one.[3]")
--        (Core.lit (Core.PInt32 1))
--        :| []
--    )
--    ( Core.app
--        Core.int32
--        (Core.var (Label (Core.int32 `Core.arrow` Core.int32 `Core.arrow` Core.int32) "f.[4]"))
--        ( Core.var (Label Core.int32 "one.[3]")
--            :| [Core.lit (Core.PInt32 6)]
--        )
--    )
--
-- fixture2 = Core.var (Label Core.int32 "m.[1]")
--
-- fixture3 =
--  Core.if_
--    ( Core.op
--        ( Core.OEqInt32
--            (Core.var (Label Core.int32 "x.[2]"))
--            (Core.lit (Core.PInt32 0))
--        )
--    )
--    (Core.var (Label Core.int32 "one.[3]"))
--    ( Core.let_
--        ( Core.Binding
--            (Label Core.int32 "m.[1]")
--            ( Core.op
--                ( Core.OMulInt32
--                    (Core.var (Label Core.int32 "x.[2]"))
--                    ( Core.app
--                        Core.int32
--                        (Core.var (Label (Core.int32 `Core.arrow` Core.int32) "f.[4]"))
--                        ( Core.op
--                            ( Core.OSubInt32
--                                (Core.var (Label Core.int32 "x.[2]"))
--                                (Core.lit (Core.PInt32 1))
--                            )
--                            :| []
--                        )
--                    )
--                )
--            )
--            :| []
--        )
--        ( Core.call
--            (Label (Core.int32 `Core.arrow` Core.TOpq) "print_int32")
--            [Core.var (Label Core.int32 "m")]
--            ( Core.app
--                (Core.TOpq `Core.arrow` Core.int32)
--                (Core.var (Label (Core.int32 `Core.arrow` Core.TOpq `Core.arrow` Core.int32) "$fn.2"))
--                (Core.var (Label Core.int32 "m.[1]") :| [])
--            )
--        )
--    )

-- myEnv =
--  IRInterpreterEnv
--    { irInterpreterValueEnv =
--        Environment.fromList
--          [
--            ( "f.[4]"
--            , Global (fun i8Ptr [i8Ptr, i8Ptr]) "f.[4]"
--            )
--          ,
--            ( "$fn.2"
--            , Global (fun i8Ptr [i8Ptr, i8Ptr]) "$fn.2"
--            )
--          ,
--            ( "main"
--            , Global (fun i8Ptr [i8Ptr]) "main"
--            )
--          ]
--    , irInterpreterConstructorEnv = mempty
--    }

-- interpretObject :: Object Core.Type (Core.Expr Core.Type) -> IRInterpreter (IRConstruct [IRLine])
-- interpretObject =
--  \case
--    OFunction name lls e -> do
--      (_, w) <- listen (local (flip (foldr insertLocal) lls) (interpret (irEvalExpr e >>= iRet i8Ptr)))
--      pure (CDefine name i8Ptr Nothing [Label i8Ptr n | Label _ n <- lls] w)
--    OConstant name e -> do
--      (_, w) <- listen (interpret (irEvalExpr e))
--      error "TODO"
-- where
--  insertLocal (Label _ name) =
--    inValueEnv (Environment.insert name (Local i8Ptr name))

-------------------------------------------------------------------------------
-------------------------------------------------------------------------------

funMain =
  OFunction
    "main"
    [Label Core.TOpq "_"]
    ( Core.let_
        ( Core.Binding
            (Label Core.int32 "one.[3]")
            (Core.lit (Core.PInt32 1))
            :| []
        )
        ( Core.app
            Core.int32
            (Core.var (Label (Core.int32 `Core.arrow` Core.int32 `Core.arrow` Core.int32) "f.[4]"))
            ( Core.var (Label Core.int32 "one.[3]")
                :| [Core.lit (Core.PInt32 6)]
            )
        )
    )

funFn2 =
  OFunction
    "$fn.2"
    [Label Core.int32 "m.[1]", Label Core.TOpq "_.[0]"]
    (Core.var (Label Core.int32 "m.[1]"))

funF4 =
  OFunction
    "f.[4]"
    [Label Core.int32 "one.[3]", Label Core.int32 "x.[2]"]
    ( Core.if_
        ( Core.op
            ( Core.OEqInt32
                (Core.var (Label Core.int32 "x.[2]"))
                (Core.lit (Core.PInt32 0))
            )
        )
        (Core.var (Label Core.int32 "one.[3]"))
        ( Core.let_
            ( Core.Binding
                (Label Core.int32 "m.[1]")
                ( Core.op
                    ( Core.OMulInt32
                        (Core.var (Label Core.int32 "x.[2]"))
                        ( Core.app
                            Core.int32
                            (Core.var (Label (Core.int32 `Core.arrow` Core.int32 `Core.arrow` Core.int32) "f.[4]"))
                            ( Core.var (Label Core.int32 "one.[3]")
                                <| Core.op
                                  ( Core.OSubInt32
                                      (Core.var (Label Core.int32 "x.[2]"))
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
                [Core.var (Label Core.int32 "m.[1]")]
                ( Core.app
                    (Core.TOpq `Core.arrow` Core.int32)
                    (Core.var (Label (Core.int32 `Core.arrow` Core.TOpq `Core.arrow` Core.int32) "$fn.2"))
                    (Core.var (Label Core.int32 "m.[1]") :| [])
                )
            )
        )
    )

testEnv :: Environment IRValue
testEnv = objectEnvironment [funMain, funFn2, funF4]

abc2 = runInterpreter (IRInterpreterEnv testEnv mempty) (interpretObject funMain)

abc3 = runInterpreter (IRInterpreterEnv testEnv mempty) (interpretObject funFn2)

abc4 = runInterpreter (IRInterpreterEnv testEnv mempty) (interpretObject funF4)

abcd = Text.putStrLn $ irEncode (thd3 (runInterpreter (IRInterpreterEnv mempty mempty) (interpret (irClosureExtend 2 (Local i8Ptr "f") (Local i32 "n") (Local i8PtrPtr "as")))))

abce = Text.putStrLn $ irEncode (thd3 (runInterpreter (IRInterpreterEnv mempty mempty) (interpret (irClosureFinalize 2 (Local i8Ptr "f") (Local i32 "n") (Local i8PtrPtr "as")))))

abcf = Text.putStrLn $ irEncode (thd3 (runInterpreter (IRInterpreterEnv mempty mempty) (interpret (irClosureApply 3 (Local i8Ptr "f") [Local i8Ptr "a0", Local i8Ptr "a1", Local i8Ptr "a2"]))))

xxd n = CDefine ("apply" <> showt n) i8Ptr Nothing (Label i8Ptr "f" : [Label i8Ptr ("a" <> showt m) | m <- [0 .. n - 1]]) xxc
 where
  xxc = thd3 (runInterpreter (IRInterpreterEnv mempty mempty) (interpret (irClosureApply n (Local i8Ptr "f") args)))
  args = [Local i8Ptr ("a" <> showt m) | m <- [0 .. n - 1]]

abcg1 = Text.putStrLn $ irEncode (xxd 1)
abcg2 = Text.putStrLn $ irEncode (xxd 2)
abcg3 = Text.putStrLn $ irEncode (xxd 3)

constrs = [("$Cons", 0), ("$Nil", 1), ("$Record", 0), ("EqualTo", 0), ("GreaterThan", 1), ("LessThan", 2), ("Node", 1), ("Leaf", 0)]

-- abc5 = Text.putStrLn (irEncode kernelArtifacts)
abc5 = (kernelArtifacts, kernelCode)
 where
  (_, Kernel{..}) = runPipeline (compile constrs blockObjects)

abc6 = (kernelArtifacts, kernelCode)
 where
  (_, Kernel{..}) = runPipeline (compile constrs blockObjects2)

abc7 = (kernelArtifacts, kernelCode)
 where
  (_, Kernel{..}) = runPipeline (compile constrs blockObjects3)

abc8 = (kernelArtifacts, kernelCode)
 where
  (_, Kernel{..}) = runPipeline (compile constrs blockObjects4)

abcx :: FilePath -> IO ()
abcx out = do
  inp <- Text.readFile "test/Noll/fixtures/prog1.txt"
  c <- case runParser expr "" inp of
    Right e ->
      let (_, Kernel{..}) = runPipeline (compile constrs (bob e))
       in pure kernelCode
  let txt = irEncode c
  Text.writeFile out txt
  Text.putStrLn "^^^"
  pure ()
 where
  bob e =
    [ OFunction
        "main"
        [Label Core.opaque "_"]
        e
    ]
