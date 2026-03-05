module Coal.Compiler.Pass.TypePhase.Prep (passPrep) where

import Coal.Compiler.Build.Core (buildEnv, prepareBuild)
import Coal.Compiler.Builtin.Definitions (builtinFunctions)
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Language (Kind)
import Coal.Language.Module (Module (..), fromProtoModule, principalPath, toProtoModule)
import Coal.ProtoCompiler.ProtoStack (ProtoCompilerT, protoOclearAssumptionsC, protoOclearNameStoreC, protoOgetCurrentBuildC, protoOinsertConstraintsC, protoOinsertNameC, protoOupdateSupplyC, setCurrentModuleC)
import Control.Monad.Except (MonadIO)
import Control.Monad.Trans (lift)
import Extras (forM_)

passPrep :: (MonadIO m, Monoid a, Eq a) => Pass a m (Module a Kind ()) (Module a Kind ())
passPrep = Pass{runPass = pass}

pass :: (MonadIO m, Monoid a, Eq a) => Module a Kind () -> CompilerT a (ProtoCompilerT m a) (Module a Kind ())
pass = withCurrentModuleC prep

prep :: (MonadIO m, Monoid a, Eq a) => Module a Kind () -> CompilerT a (ProtoCompilerT m a) (Module a Kind ())
prep m = do
  lift $ do
    let modul = toProtoModule [] m
    protoOclearAssumptionsC
    protoOclearNameStoreC
    setCurrentModuleC modul
    forM_ builtinFunctions $ uncurry protoOinsertNameC

  clearAssumptionsC
  clearNameStoreC
  (next, build) <- prepareBuild m
  insertCurrentModuleC build
  env <- buildEnv
  setNamesC env
  insertNamesC builtinFunctions
  pure next
