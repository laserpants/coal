{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Core.LLVM.IRInstruction.InterpreterSpec where

import Control.Monad.Reader (local)
import Control.Monad.Writer (listen)
import Noll.Common.Environment (Environment)
import Noll.Common.List1 (NonEmpty (..), (<|))
import Noll.Compiler.Core (BlockObject (..), ObjectList, objectName)
import Noll.Core.LLVM.IRConstruct (IRConstruct (..))
import Noll.Core.LLVM.IRInstruction.Eval (irEvalExpr)
import Noll.Core.LLVM.IRInstruction.Interpreter (
  IRInterpreter (..),
  IRInterpreterEnv (..),
  IRLine (..),
  inValueEnv,
  interpret,
  runInterpreter,
 )
import Noll.Core.LLVM.IRInstruction.TH
import Noll.Core.LLVM.IRType
import Noll.Core.LLVM.IRValue
import Noll.Label (Label (..))
import Noll.Utils (Name)
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

myEnv =
  IRInterpreterEnv
    { irInterpreterValueEnv =
        Environment.fromList
          [
            ( "f.[4]"
            , Global (fun i8Ptr [i8Ptr, i8Ptr]) "f.[4]"
            )
          ,
            ( "$fn.2"
            , Global (fun i8Ptr [i8Ptr, i8Ptr]) "$fn.2"
            )
          ,
            ( "main"
            , Global (fun i8Ptr [i8Ptr]) "main"
            )
          ]
    , irInterpreterConstructorEnv = mempty
    }

-- objectIRType1 = runInterpreter myEnv (interpret (irEvalExpr fixture1))

-- blockInterpreter :: BlockObject Core.Type (Core.Expr Core.Type) -> IRInterpreter IRValue
-- blockInterpreter =
--  \case
--    OFunction name lls e ->
--      local (flip (foldr insertLocal) lls) (interpret (irEvalExpr e))
--    OConstant _ e ->
--      interpret (irEvalExpr e)
-- where
--  insertLocal (Label _ name) =
--    inValueEnv (Environment.insert name (Local i8Ptr name))

blockInterpreter :: BlockObject Core.Type (Core.Expr Core.Type) -> IRInterpreter (IRConstruct [IRLine])
blockInterpreter =
  \case
    OFunction name lls e -> do
      (_, w) <- listen (local (flip (foldr insertLocal) lls) (interpret (irEvalExpr e >>= iRet i8Ptr)))
      pure (CDefine name i8Ptr Nothing [Label i8Ptr n | Label _ n <- lls] w)
    OConstant name e -> do
      (_, w) <- listen (interpret (irEvalExpr e))
      error "TODO"
 where
  insertLocal (Label _ name) =
    inValueEnv (Environment.insert name (Local i8Ptr name))

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

objectIRType :: BlockObject Core.Type (Core.Expr Core.Type) -> IRType
objectIRType =
  \case
    OFunction _ lls _ ->
      irOpaqueSignature (length lls)
    _ ->
      error "TODO"

objectValue :: BlockObject Core.Type (Core.Expr Core.Type) -> (Name, IRValue)
objectValue o = (name, Global (objectIRType o) name) where name = objectName o

objectListToEnvironment :: Environment IRValue -> ObjectList -> Environment IRValue
objectListToEnvironment = foldr (uncurry Environment.insert . objectValue)

testEnv :: Environment IRValue
testEnv = objectListToEnvironment mempty [funMain, funFn2, funF4]

objectIRType2 = runInterpreter (IRInterpreterEnv testEnv mempty) (blockInterpreter funMain)

objectIRType3 = runInterpreter (IRInterpreterEnv testEnv mempty) (blockInterpreter funFn2)

objectIRType4 = runInterpreter (IRInterpreterEnv testEnv mempty) (blockInterpreter funF4)
