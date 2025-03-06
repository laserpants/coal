{-# LANGUAGE LambdaCase #-}

module Noll.Core.LLVM.IRInstruction.Interpreter.Object (
  objectInterpreter,
  objectEnvironment,
) where

import Control.Monad.Reader (local)
import Control.Monad.Writer (listen)
import Noll.Common.Environment (Environment)
import Noll.Core.LLVM.IRConstruct (IRConstruct (..))
import Noll.Core.LLVM.IREval (irEvalFun)
import Noll.Core.LLVM.IREval.Expr (irEvalExpr)
import Noll.Core.LLVM.IRInstruction.Interpreter (
  IRInterpreter (..),
  IRLine (..),
  inValueEnv,
  interpret,
 )
import Noll.Core.LLVM.IRType.Syntax (i8Ptr, objectIRType)
import Noll.Core.LLVM.IRValue (IRValue (..))
import Noll.Core.Language.Object (Object (..), ObjectList, objectName)
import Noll.Label (Label (..))
import Noll.Utils (Name)

import qualified Noll.Common.Environment as Environment
import qualified Noll.Core.Language as Core

type CoreObject = Object Core.Type (Core.Expr Core.Type)

objectInterpreter :: CoreObject -> IRInterpreter (IRConstruct [IRLine])
objectInterpreter =
  \case
    OFunction name lls e -> do
      (_, w) <- listen (local (flip (foldr insertLocal) lls) (interpret (irEvalFun e)))
      pure (CDefine name i8Ptr Nothing [Label i8Ptr n | Label _ n <- lls] w)
    OConstant name e -> do
      (_, w) <- listen (interpret (irEvalExpr e))
      error "TODO"
    _ ->
      error "TODO"
 where
  insertLocal (Label _ name) =
    inValueEnv (Environment.insert name (Local i8Ptr name))

objectValue :: CoreObject -> (Name, IRValue)
objectValue o = (name, Global (objectIRType o) name) where name = objectName o

objectEnvironment :: ObjectList -> Environment IRValue
objectEnvironment = foldr (uncurry Environment.insert . objectValue) mempty
