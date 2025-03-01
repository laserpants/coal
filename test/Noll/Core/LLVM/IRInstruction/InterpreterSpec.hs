{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Noll.Core.LLVM.IRInstruction.InterpreterSpec where

import Control.Monad.Reader (local)
import Control.Monad.Writer (listen)
import Noll.Common.Environment (Environment)
import Noll.Common.List1 (NonEmpty (..), (<|))
import Noll.Compiler.Core
import Noll.Core.LLVM.IRConstruct (IRConstruct (..))
import Noll.Core.LLVM.IREncodable (IREncodable (..))
import Noll.Core.LLVM.IRInstruction.Eval (irEvalExpr)
import Noll.Core.LLVM.IRInstruction.Interpreter (
  IRInterpreter (..),
  IRInterpreterEnv (..),
  IRLine (..),
  inValueEnv,
  interpret,
  runInterpreter,
 )
import Noll.Core.LLVM.IRInstruction.Interpreter.Object (objectEnvironment, objectInterpreter)
import Noll.Core.LLVM.IRInstruction.TH
import Noll.Core.LLVM.IRType
import Noll.Core.LLVM.IRType.Syntax
import Noll.Core.LLVM.IRValue
import Noll.Core.Language (list, opaque, (~>))
import Noll.Core.Language.Object (Object (..), ObjectList, objectName)
import Noll.Label (Label (..))
import Noll.Utils (Name)
import Test.Hspec (Spec, describe, it)

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
--                                  (Core.var (Label Core.int32 "e"))
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

-- objectInterpreter :: Object Core.Type (Core.Expr Core.Type) -> IRInterpreter (IRConstruct [IRLine])
-- objectInterpreter =
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

abc2 = runInterpreter (IRInterpreterEnv testEnv mempty) (objectInterpreter funMain)

abc3 = runInterpreter (IRInterpreterEnv testEnv mempty) (objectInterpreter funFn2)

abc4 = runInterpreter (IRInterpreterEnv testEnv mempty) (objectInterpreter funF4)

-- abc5 = Text.putStrLn (irEncode pipelineStateArtifacts)
abc5 = (pipelineStateArtifacts, pipelineStateCode)
 where
  (_, PipelineState{..}) = runCore (compile blockObjects)

abc6 = (pipelineStateArtifacts, pipelineStateCode)
 where
  (_, PipelineState{..}) = runCore (compile blockObjects2)

abc7 = (pipelineStateArtifacts, pipelineStateCode)
 where
  (_, PipelineState{..}) = runCore (compile blockObjects3)
