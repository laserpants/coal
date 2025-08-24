{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Kernel.TranslateDefinition (translateDefinition) where

import Coal.Common.Label (Label (..))
import Coal.Common.List1 (NonEmpty ((:|)), fromList1, (<|))
import Coal.Compiler.Kernel.Environment (KernelEnvironment (..), withLocalNames)
import Coal.Compiler.Kernel.TranslateExpression (translateExpression, translatePattern)
import Coal.Compiler.Kernel.TranslateType (translateType)
import Coal.Language
import Coal.Language.Module
import Control.Monad (forM)
import Control.Monad.Reader (MonadReader, asks)
import Data.Data (Data)
import Data.List.Extra (sortOn)
import Extra (Name, (<$$>))

import qualified Coal.Kernel.Language as Kernel

type KernelObject = Kernel.Object Kernel.Type (Kernel.Expr Kernel.Type)

translateDefinition :: (Show a, MonadReader KernelEnvironment m, Data a) => Definition a Kind IndexedType -> m [KernelObject]
translateDefinition =
  \case
    DAnnotation _ d ->
      translateDefinition d
    DType _ _ ctors ->
      traverse translateConstructor (zip [0 ..] (sortOn constructorName ctors))
    DFunction name (Function _ _ ps e) _ -> do
      qs <- traverse translatePattern (fromList1 ps)
      f <- withLocalNames (labelName <$> qs) (translateExpression e)
      moduleName <- asks kernelEnvironmentModule
      pure [Kernel.OFunction (moduleName <> "." <> name) qs f]
    DConstant name (Constant _ With{} e) -> do
      c <- translateExpression e
      moduleName <- asks kernelEnvironmentModule
      pure [Kernel.OConstant (moduleName <> "." <> name) c]
    DTrait name _ _ ins ->
      forM ins $
        \(n, t) ->
          traitAccessor name n (translateType t)
    DInstance name t ds ->
      concat <$$> forM ds $
        \case
          DFunction n f _ -> do
            translateDefinition (DFunction (n <> postfix) f [])
          DConstant n c -> do
            translateDefinition (DConstant (n <> postfix) c)
          _ ->
            error "TODO"
     where
      postfix = "__$instance_" <> serialize (Trait name t)
    _ ->
      pure []

traitAccessor :: (MonadReader KernelEnvironment m) => Name -> Name -> Kernel.Type -> m KernelObject
traitAccessor trait fn t = do
  moduleName <- asks kernelEnvironmentModule
  pure $
    Kernel.OFunction
      (moduleName <> "." <> fn)
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

translateConstructor :: (MonadReader KernelEnvironment m) => (Int, Constructor Parameter () (Type Parameter ())) -> m KernelObject
translateConstructor (index, Constructor name _ (Forall _ _ t)) = do
  moduleName <- asks kernelEnvironmentModule
  pure (Kernel.OData (moduleName <> "." <> name) index (translateType t))
