module Coal.Compiler.Pass.TypePhase.Prep2 (passPrep2) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Compiler.Build.Prep
import Coal.Compiler.Pass (Pass (..))
import Coal.Compiler.Stack
import Coal.Language (Kind)
import Coal.Language.Module (Module (..))
import Coal.Language.Type.Kind.Indexed (ToKindIndexed (..))
import Control.Monad.Except (MonadIO)
import Control.Monad.Trans (lift)

passPrep2 :: (MonadIO m) => Pass Metadata m (Module Metadata Kind ()) (Module Metadata Kind ())
passPrep2 = Pass{runPass = pass}

pass :: (MonadIO m) => Module Metadata Kind () -> CompilerT Metadata m (Module Metadata Kind ())
pass m = do
  --  setCompilerCurrentModuleC (modulePath m)
  --  lift $ setCurrentPathC (modulePath m)
  prep m

-- withCurrentModuleC prep

prep :: (MonadIO m) => Module Metadata Kind () -> CompilerT Metadata m (Module Metadata Kind ())
prep m = do
  --  m1 <- lift $ do
  --    let modul = toModule [] m
  --    clearAssumptionsC
  --    clearNameStoreC
  --    setCurrentModuleC modul
  --    forM_ builtinFunctions $ uncurry insertNameC
  --    toKindIndexed modul
  --
  --  y <- expandFunctionGroups m1
  prepareBuild m
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
