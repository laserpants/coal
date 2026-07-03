{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Kernel.NewTranslate.Definition (translateDefinition) where

import Coal.Compiler.Kernel.Environment (KernelEnvironment (..), withLocalNames)
import Coal.Compiler.Kernel.NewTranslate.Expression (translateExpression, translatePattern)
import Coal.Compiler.Kernel.NewTranslate.Type (translateType)
import Coal.Compiler.Stack
import qualified Coal.Kernel.Language.Expr as NK
import qualified Coal.Kernel.Language.Object as NKObj
import qualified Coal.Kernel.Language.Type as NKT
import Coal.Language
import Control.Monad (forM)
import Control.Monad.Extra (concatForM)
import Control.Monad.Reader (asks)
import Data.Data (Data)
import Data.List.Extra (sortOn)
import Data.List.NonEmpty (toList)
import Extras (Name, (<.>))

type NKObject = NKObj.Object NKT.Type

translateDefinition ::
  (Monad m, Data a) =>
  Definition a Kind IndexedType ->
  CompilerT a m [NKObject]
translateDefinition =
  \case
    DType _ typeName TypeDefinition{..} -> do
      moduleName <- asks (kernelEnvironmentModule . compilerKernelEnvironment)
      let ctors = sortOn constructorName typeDefinitionConstructors
          ctorPairs =
            [ (moduleName <.> cName, translateType cType)
            | DataConstructor cName _ (Forall _ _ cType) <- ctors
            ]
      pure (if null ctorPairs then [] else [NKObj.DData (moduleName <.> typeName) ctorPairs])
    DFunction _ name FunctionDefinition{..} -> do
      qs <- traverse translatePattern (toList functionDefinitionPatterns)
      f <- withLocalNames (nkLabelName <$> qs) (translateExpression functionDefinitionExpression)
      moduleName <- asks (kernelEnvironmentModule . compilerKernelEnvironment)
      pure [NKObj.DFunction NKObj.Exported (moduleName <.> name) qs f]
    DLet _ name LetDefinition{letDefinitionType = With{}, ..} -> do
      c <- translateExpression letDefinitionExpression
      moduleName <- asks (kernelEnvironmentModule . compilerKernelEnvironment)
      pure [NKObj.DConstant (moduleName <.> name) c]
    DTrait _ name TraitDefinition{..} ->
      forM traitDefinitionInterface $
        \(TraitDefinitionInterfaceEntry n (Forall _ _ t)) ->
          traitAccessor name n (translateType t)
    DInstance _ InstanceDefinition{..} ->
      concatForM instanceDefinitionImplementations $
        \case
          DFunction loc n FunctionDefinition{..} ->
            translateDefinition
              ( DFunction
                  loc
                  (instanceLabel (Trait instanceDefinitionTraitName instanceDefinitionType) n)
                  FunctionDefinition{..}
              )
          DLet loc n LetDefinition{..} ->
            translateDefinition
              ( DLet
                  loc
                  (instanceLabel (Trait instanceDefinitionTraitName instanceDefinitionType) n)
                  LetDefinition{..}
              )
          _ ->
            pure []
    _ ->
      pure []

{- | Generate a trait accessor function.

The accessor projects field @fn@ from a dictionary value of type @trait@.
-}
traitAccessor :: (Monad m) => Name -> Name -> NKT.Type -> CompilerT a m NKObject
traitAccessor trait fn t = do
  moduleName <- asks (kernelEnvironmentModule . compilerKernelEnvironment)
  let dict = NK.Label (NKT.TCon trait [NKT.TOpq]) "$a"
  pure $
    NKObj.DFunction NKObj.Exported
      (moduleName <.> fn)
      [dict]
      (NK.EGet (NK.Label t fn) (NK.EVar dict))

{-# INLINE nkLabelName #-}
nkLabelName :: NK.Label t -> Name
nkLabelName (NK.Label _ name) = name
