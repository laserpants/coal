module Coal.Compiler.Pass.TypePhase.Prep2 (passPrep2) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Language (Kind)
import Coal.Language.Type.Kind.Indexed (ToKindIndexed (..))
import Coal.ProtoCompiler.ProtoBuild.ProtoPrep
import Coal.ProtoCompiler.ProtoStack (ProtoCompilerT, protoOclearAssumptionsC, protoOclearNameStoreC, protoOgetCurrentBuildC, protoOinsertConstraintsC, protoOinsertNameC, protoOupdateSupplyC, setCurrentModuleC, setCurrentPathC)
import Coal.ProtoLanguage.ProtoModule (ProtoModule (..))
import Control.Monad.Except (MonadIO)
import Control.Monad.Trans (lift)

passPrep2 :: (MonadIO m) => Pass Metadata m (ProtoModule Metadata Kind ()) (ProtoModule Metadata Kind ())
passPrep2 = Pass{runPass = pass}

pass :: (MonadIO m) => ProtoModule Metadata Kind () -> CompilerT Metadata (ProtoCompilerT m Metadata) (ProtoModule Metadata Kind ())
pass m = do
  --  setCompilerCurrentModuleC (modulePath m)
  --  lift $ setCurrentPathC (modulePath m)
  prep m

-- withCurrentModuleC prep

prep :: (MonadIO m) => ProtoModule Metadata Kind () -> CompilerT Metadata (ProtoCompilerT m Metadata) (ProtoModule Metadata Kind ())
prep m = do
  --  m1 <- lift $ do
  --    let modul = toProtoModule [] m
  --    protoOclearAssumptionsC
  --    protoOclearNameStoreC
  --    setCurrentModuleC modul
  --    forM_ builtinFunctions $ uncurry protoOinsertNameC
  --    toKindIndexed modul
  --
  --  y <- expandFunctionGroups m1
  lift $ protoOprepareBuild m
  return m

--  --  clearAssumptionsC
--  --  clearNameStoreC
--  --  (next, build) <- prepareBuild m
--  --  insertCurrentModuleC build
--  --  env <- buildEnv
--  --
--  --  setNamesC env
--  --  insertNamesC builtinFunctions
--
--  pure y
