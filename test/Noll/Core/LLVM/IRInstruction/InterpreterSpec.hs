{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.LLVM.IRInstruction.InterpreterSpec where

import Noll.Common.List1 (NonEmpty (..), (<|))
import Noll.Compiler.Core (BlockObject (..), ObjectList)
import Noll.Core.LLVM.IRInstruction.Eval (irEvalExpr)
import Noll.Core.LLVM.IRInstruction.Interpreter (IRInterpreterEnv (..), interpret, runInterpreter)
import Noll.Core.LLVM.IRType
import Noll.Core.LLVM.IRValue
import Noll.Label (Label (..))
import Test.Hspec (Spec, describe, it)

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
      fixture
  ]

fixture =
  Core.let_
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

-- main
fixture1 =
  Core.let_
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

myEnv =
  IRInterpreterEnv
    { irInterpreterValueEnv =
        Environment.fromList
          [
            ( "f.[4]"
            , Global (fun i8Ptr [i8Ptr, i8Ptr]) "f.[4]"
            )
          ]
    , irInterpreterConstructorEnv = mempty
    }

abc1 = runInterpreter myEnv (interpret (irEvalExpr fixture1))
