{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TypeApplications #-}

{- |
Data constructor compilation and pattern matching.

This module handles the code generation for data constructors and @case@
expressions. Each constructor is compiled to:

  * A struct type declaration with a tag field and field slots
  * A @make_%@ function that allocates and initializes instances

Pattern matching is implemented via tag-based dispatch, with each clause
compiled to a separate basic block that extracts fields and binds pattern
variables.
-}
module Coal.Kernel.LLVM.Constructor (
  ConstructorDefinition (..),
  irDataConstructor,
  irApplyConstructor,
  irClause,
  irCase,
) where

import Control.Monad (forM, forM_)
import Control.Monad.Reader (local)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text as Text

import LLVM.IR
import qualified LLVM.IROperand.Constructors as O

import qualified Coal.Kernel.LLVM.Boxing as Boxing
import Coal.Kernel.LLVM.Monad (IRCodegen)
import Coal.Kernel.LLVM.Runtime (callRuntime)
import Coal.Kernel.LLVM.RuntimeDefs (rtAlloc)
import Coal.Kernel.Language.Expr (Clause (..), Expr (..), Label (..))
import Coal.Kernel.Language.Type (Type)
import Common (Name)
import qualified Common.Environment as Environment

data ConstructorDefinition = ConstructorDefinition
  { constructorFieldCount :: Int
  , constructorIndex :: Int
  }

{- | Generate the IR type declaration and @make_%@ constructor function for a
data constructor.

The @make_%@ function allocates a struct with a tag (constructor index) and the
specified number of fields, stores the tag and field values, and returns a
pointer to the allocated memory.

==== Built-in constructors

Constructors whose names begin with @$@ are built-in and declared in every
module. These use 'LInternal' linkage to give each translation unit a private
copy and avoid duplicate-symbol linker errors.
-}
irDataConstructor :: Name -> ConstructorDefinition -> IRCodegen ()
irDataConstructor name ConstructorDefinition{constructorFieldCount, constructorIndex} = do
  emitTypeDecl name (TStruct $ [i32] <> fmap fst args)
  -- Built-in constructors (names beginning with '$') are declared in every
  -- module, so we use LInternal linkage to give each translation unit a
  -- private copy and avoid duplicate-symbol linker errors.
  let linkage = if Text.isPrefixOf "$" name then LInternal else LExternal
  define TPtr ("make_%" <> name) args linkage [] $ do
    sizeptr <- gep typ (O.nullPtr TPtr) [O.i32 @Int 1]
    size <- ptrtoint sizeptr i32
    mem <- callRuntime rtAlloc [size]
    tagptr <- gep typ mem [O.i32 @Int 0, O.i32 @Int 0]
    store (O.i32 constructorIndex) tagptr
    forM_ [1 .. constructorFieldCount] $
      \i -> do
        field <- gep typ mem [O.i32 @Int 0, O.i32 @Int i]
        store (OLocal TPtr ("field" <> Text.pack (show i))) field
    ret mem
 where
  args = [(TPtr, "field" <> Text.pack (show n)) | n <- [1 .. constructorFieldCount]]
  typ = TNamed name

{- | Generate a call to a data constructor's @make_%@ function, boxing the
arguments as needed.
-}
irApplyConstructor :: (Expr Type -> IRCodegen IROperand) -> Label Type -> [Expr Type] -> IRCodegen IROperand
irApplyConstructor irValue (Label t name) es = do
  args <- traverse (Boxing.irBoxed irValue) es
  call NoTail TPtr (OGlobal (Boxing.irTypeRep t) ("make_%" <> name)) args

{- | Generate a labeled basic block for a case clause.

Extracts the constructor's fields from the scrutinee, binds them to the pattern
variables, and evaluates
the clause body as a tail call.
-}
irClause :: (Expr Type -> IRCodegen ()) -> IROperand -> Clause Type -> IRCodegen Name
irClause irTail op (Clause (Label _ con :| lls) body) = do
  label <- block con
  if null lls
    then irTail body
    else do
      bound <- forM (zip [1 ..] lls) $
        \(i, Label t name) -> do
          r1 <- gep (TNamed con) op [O.i32 @Int 0, O.i32 @Int i]
          r2 <- load TPtr r1 <##> name
          r3 <- Boxing.irUnbox t r2
          return (name, r3)
      local (Environment.insertMultiple bound) (irTail body)
  return label

{- | Generate a switch statement for pattern matching on a data constructor.
Loads the constructor index (tag) from the scrutinee and dispatches to the
appropriate clause.
-}
irCase :: (Expr Type -> IRCodegen IROperand) -> (Expr Type -> IRCodegen ()) -> Expr Type -> NonEmpty (Clause Type) -> IRCodegen ()
irCase irValue irTail e1 cs = mdo
  r1 <- irValue e1
  r2 <- loadConstructorIndex r1
  switch r2 defaultL blocks
  blocks <- forM (zip [0 ..] (NonEmpty.toList cs)) $
    \(i, clause) -> do
      label <- irClause irTail r1 clause
      return (CInt 32 i, label)
  defaultL <- block "default"
  unreachable
 where
  loadConstructorIndex :: IROperand -> IRCodegen IROperand
  loadConstructorIndex op = do
    tag <- gep (TStruct [i32]) op [O.i32 @Int 0, O.i32 @Int 0]
    load i32 tag
