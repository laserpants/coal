{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Kernel.TranslateDefinition (translateDefinition) where

import Coal.Common.Label (Label (..))
import Coal.Compiler.Kernel.Environment (KernelEnvironment (..), withLocalNames)
import Coal.Compiler.Kernel.TranslateExpression (translateExpression, translatePattern)
import Coal.Compiler.Kernel.TranslateType (translateType)
import Coal.Compiler.Stack
import qualified Coal.Kernel.Language as Kernel
import Coal.Kernel.Language.Object (KernelObject)
import Coal.Language
import Coal.Language.Module
import Control.Monad (forM)
import Control.Monad.Extra (concatForM)
import Control.Monad.Reader (asks)
import Data.Data (Data)
import Data.List.Extra (sortOn)
import Data.List.NonEmpty (NonEmpty ((:|)), toList, (<|))
import Extras (Name, (<.>))

translateDefinition :: (Monad m, Data a) => Definition a Kind IndexedType -> CompilerT a m [KernelObject]
translateDefinition =
  \case
    DType _ _ (TypeDefinition _ ctors) ->
      traverse translateConstructor (zip [0 ..] (sortOn constructorName ctors))
    DFunction _ name (FunctionDefinition _ _ _ ps e :| _) _ -> do
      qs <- traverse translatePattern (toList ps)
      f <- withLocalNames (labelName <$> qs) (translateExpression e)
      moduleName <- asks (kernelEnvironmentModule . compilerKernelEnvironment)
      pure [Kernel.OFunction (moduleName <.> name) qs f]
    DConstant _ name (ConstantDefinition _ _ With{} e) _ -> do
      c <- translateExpression e
      moduleName <- asks (kernelEnvironmentModule . compilerKernelEnvironment)
      pure [Kernel.OConstant (moduleName <.> name) c]
    DFold _ name (FoldDefinition _ _ (Just e)) -> do
      c <- translateExpression e
      moduleName <- asks (kernelEnvironmentModule . compilerKernelEnvironment)
      pure [Kernel.OConstant (moduleName <.> name) c]
    DUnfold _ name (UnfoldDefinition _ _ _ (Just e)) -> do
      c <- translateExpression e
      moduleName <- asks (kernelEnvironmentModule . compilerKernelEnvironment)
      pure [Kernel.OConstant (moduleName <.> name) c]
    DTrait _ name (TraitDefinition _ _ ds) ->
      forM ds $
        \(n, Forall _ _ t) ->
          traitAccessor name n (translateType t)
    DInstance _ name (InstanceDefinition _ t ds) ->
      concatForM ds $
        \case
          DFunction loc n f _ ->
            translateDefinition (DFunction loc (instanceLabel trait n) f [])
          DConstant loc n c _ ->
            translateDefinition (DConstant loc (instanceLabel trait n) c [])
          _ ->
            pure []
     where
      trait = Trait name t
    _ ->
      pure []

traitAccessor :: (Monad m) => Name -> Name -> Kernel.Type -> CompilerT a m KernelObject
traitAccessor trait fn t = do
  moduleName <- asks (kernelEnvironmentModule . compilerKernelEnvironment)
  pure $
    Kernel.OFunction
      (moduleName <.> fn)
      [dict]
      ( Kernel.match
          t
          (Kernel.var dict)
          ( Kernel.Clause
              (Label (Kernel.functionTypeOf dict [row]) "$Record" <| row :| [])
              ( Kernel.sel
                  (Kernel.Focus fn var (Label Kernel.opaque "_"))
                  (Kernel.var row)
                  (Kernel.var var)
              )
              :| []
          )
      )
 where
  var = Label t "$f"
  row = Label (Kernel.RExt fn t Kernel.opaque) "$r"
  dict = Label (Kernel.TCon trait [Kernel.opaque]) "$a"

translateConstructor :: (Monad m) => (Int, DataConstructor Parameter () (Type Parameter ())) -> CompilerT a m KernelObject
translateConstructor (index, DataConstructor name _ (Forall _ _ t)) = do
  moduleName <- asks (kernelEnvironmentModule . compilerKernelEnvironment)
  pure (Kernel.OData (moduleName <.> name) index (translateType t))
