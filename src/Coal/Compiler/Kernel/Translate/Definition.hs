{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Kernel.Translate.Definition (translateDefinition) where

import Coal.Common.Label (Label (..))
import Coal.Compiler.Kernel.Environment (KernelEnvironment (..), withLocalNames)
import Coal.Compiler.Kernel.Translate.Expression (translateExpression, translatePattern)
import Coal.Compiler.Kernel.Translate.Type (translateType)
import Coal.Compiler.Stack
import qualified Coal.Kernel.Language as Kernel
import Coal.Kernel.Language.Object (KernelObject)
import Coal.Language
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
    DType _ _ TypeDefinition{..} ->
      traverse translateConstructor (zip [0 ..] (sortOn constructorName typeDefinitionConstructors))
    DFunction loc name (FunctionDefinition _ _ _ ps e) -> do
      qs <- traverse translatePattern (toList ps)
      f <- withLocalNames (labelName <$> qs) (translateExpression e)
      moduleName <- asks (kernelEnvironmentModule . compilerKernelEnvironment)
      pure [Kernel.OFunction (moduleName <.> name) qs f]
    DLet loc name (LetDefinition _ _ With{} e) -> do
      c <- translateExpression e
      moduleName <- asks (kernelEnvironmentModule . compilerKernelEnvironment)
      pure [Kernel.OConstant (moduleName <.> name) c]
    DTrait loc name TraitDefinition{..} ->
      forM traitDefinitionInterface $
        \(TraitDefinitionInterfaceEntry n (Forall _ _ t)) ->
          traitAccessor name n (translateType t)
    DInstance _ (InstanceDefinition _ trait _ t ds) ->
      concatForM ds $
        \case
          DFunction loc n f ->
            translateDefinition (DFunction loc (instanceLabel (Trait trait t) n) f)
          DLet loc n c ->
            translateDefinition (DLet loc (instanceLabel (Trait trait t) n) c)
          _ ->
            pure []
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

translateConstructor :: (Monad m) => (Int, DataConstructor Parameter Kind (Type Parameter Kind)) -> CompilerT a m KernelObject
translateConstructor (index, DataConstructor name _ (Forall _ _ t)) = do
  moduleName <- asks (kernelEnvironmentModule . compilerKernelEnvironment)
  pure (Kernel.OData (moduleName <.> name) index (translateType t))
