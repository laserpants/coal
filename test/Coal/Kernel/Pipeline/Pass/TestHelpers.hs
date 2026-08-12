{-# LANGUAGE OverloadedStrings #-}

module Coal.Kernel.Pipeline.Pass.TestHelpers (
  -- * Pass runner
  runPass,

  -- * Module construction
  mkModule,

  -- * Label construction
  lbl,
  int32Lbl,
  boolLbl,

  -- * Common expressions
  unit_,
  intLit,
  var,
  lam1,
  lam2,

  -- * Non-empty list construction
  ne,

  -- * Common object constructors
  mkFn,
  mkConst,
  mkDData,
  mkDData1,
) where

import Coal.Common.Name (Name)
import Coal.Kernel.Language.Expr (Expr (..), Label (..))
import Coal.Kernel.Language.Module (Module (..))
import Coal.Kernel.Language.Object (FunctionScope (..), Object (..))
import Coal.Kernel.Language.Prim (Prim (..))
import Coal.Kernel.Language.Type (Type (..))
import qualified Coal.Kernel.Language.Type.Constructors as Type
import Coal.Kernel.Pipeline (Pass, PipelineError, evalPipeline, initialPipelineState)
import Control.Monad.Identity (Identity)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)

-- ---------------------------------------------------------------------------
-- Pass runner
-- ---------------------------------------------------------------------------

{- | Run a 'Pass' starting from a fresh 'PipelineState' (counter = 0).
Returns the transformed output or a 'PipelineError'.
-}
runPass :: Pass Identity i o -> i -> Either PipelineError o
runPass pass = evalPipeline initialPipelineState . pass

-- ---------------------------------------------------------------------------
-- Module construction
-- ---------------------------------------------------------------------------

-- | Wrap a list of objects into a minimal 'Module'.
mkModule :: [Object Type] -> Module Type
mkModule = Module "Test" []

-- ---------------------------------------------------------------------------
-- Label helpers
-- ---------------------------------------------------------------------------

-- | A label with an opaque type tag.
lbl :: Text -> Label Type
lbl = Label TOpq

-- | A label with an 'int32' type tag.
int32Lbl :: Text -> Label Type
int32Lbl = Label Type.int32

-- | A label with a 'bool' type tag.
boolLbl :: Text -> Label Type
boolLbl = Label Type.bool

-- ---------------------------------------------------------------------------
-- Expression helpers
-- ---------------------------------------------------------------------------

-- | The unit literal @()@.
unit_ :: Expr Type
unit_ = ELit PUnit

-- | A 32-bit integer literal.
intLit :: Int -> Expr Type
intLit n = ELit (PInt32 (fromIntegral n))

-- | A variable reference at opaque type.
var :: Text -> Expr Type
var name = EVar (lbl name)

-- | @fn(p) => body@
lam1 :: Text -> Expr Type -> Expr Type
lam1 p = ELam (ne [lbl p])

-- | @fn(p, q) => body@
lam2 :: Text -> Text -> Expr Type -> Expr Type
lam2 p q = ELam (ne [lbl p, lbl q])

-- ---------------------------------------------------------------------------
-- Non-empty list helpers
-- ---------------------------------------------------------------------------

-- | Wrap a list into a 'NonEmpty', erroring on empty input.
ne :: [a] -> NonEmpty a
ne (x : xs) = x :| xs
ne [] = error "ne: empty list"

-- ---------------------------------------------------------------------------
-- Object helpers
-- ---------------------------------------------------------------------------

-- | Construct a 'DFunction' top-level object.
mkFn :: Name -> [Label Type] -> Expr Type -> Object Type
mkFn = DFunction Exported

-- | Construct a 'DConstant' top-level object.
mkConst :: Name -> Expr Type -> Object Type
mkConst = DConstant

{- | Construct a 'DData' top-level object with the given type name and
list of (constructor name, constructor type) pairs.

For simple tests, use 'mkDData1' to create a single-constructor type.
-}
mkDData :: Name -> [(Name, Type)] -> Object Type
mkDData = DData

{- | Construct a single-constructor 'DData' with 'TOpq' type.
Suitable for testing pass behaviour without full type information.
-}
mkDData1 :: Name -> Name -> Object Type
mkDData1 typeName ctorName = DData typeName [(ctorName, TOpq)]
