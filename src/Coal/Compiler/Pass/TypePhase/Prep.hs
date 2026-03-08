module Coal.Compiler.Pass.TypePhase.Prep (passPrep) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Build.Core (buildEnv, prepareBuild)
import Coal.Compiler.Builtin.Definitions (builtinFunctions)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Language (Kind)
import Coal.Language.Module (Module (..), fromProtoModule, principalPath, toProtoModule)
import Coal.Language.Type.Kind.Indexed (ToKindIndexed (..))
import Coal.ProtoCompiler.ProtoBuild.ProtoPrep
import Coal.ProtoCompiler.ProtoStack (ProtoCompilerT, protoOclearAssumptionsC, protoOclearNameStoreC, protoOgetCurrentBuildC, protoOinsertConstraintsC, protoOinsertNameC, protoOupdateSupplyC, setCurrentModuleC, setCurrentPathC)
import Coal.ProtoLanguage.ProtoModule (ProtoModule (..))
import Control.Monad.Except (MonadIO)
import Control.Monad.Trans (lift)
import Extras (forM_)
import Coal.Compiler.Pass.TypePhase.ExpandFunctionGroups

passPrep :: (MonadIO m) => Pass Metadata m (Module Metadata Kind ()) (ProtoModule Metadata Kind ())
passPrep = Pass{runPass = pass}

pass :: (MonadIO m) => Module Metadata Kind () -> CompilerT Metadata (ProtoCompilerT m Metadata) (ProtoModule Metadata Kind ())
pass m = do
  setCompilerCurrentModuleC (modulePath m)
  lift $ setCurrentPathC (modulePath m)
  prep m

-- withCurrentModuleC prep

prep :: (MonadIO m) => Module Metadata Kind () -> CompilerT Metadata (ProtoCompilerT m Metadata) (ProtoModule Metadata Kind ())
prep m = do
  m1 <- lift $ do
    let modul = toProtoModule [] m
    protoOclearAssumptionsC
    protoOclearNameStoreC
    setCurrentModuleC modul
    forM_ builtinFunctions $ uncurry protoOinsertNameC
    toKindIndexed modul

  y <- expandFunctionGroups m1
  lift $ protoOprepareBuild y

  clearAssumptionsC
  clearNameStoreC
  (next, build) <- prepareBuild m
  insertCurrentModuleC build
  env <- buildEnv
  setNamesC env
  insertNamesC builtinFunctions

  pure y
