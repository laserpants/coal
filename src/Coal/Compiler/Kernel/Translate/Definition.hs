{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Kernel.Translate.Definition (translateDefinition) where

import Coal.Compiler.Kernel.Environment (KernelEnvironment (..), withLocalNames)
import Coal.Compiler.Kernel.Translate.Expression (translateExpression, translatePattern)
import Coal.Compiler.Kernel.Translate.Type (translateType)
import Coal.Compiler.Stack
import qualified Coal.Kernel.Language.Expr as Kernel
import qualified Coal.Kernel.Language.Object as Kernel
import qualified Coal.Kernel.Language.Type as Kernel
import qualified Coal.Kernel.Language.Type.Constructors as T
import Coal.Language
import Control.Monad (forM)
import Control.Monad.Extra (concatForM)
import Control.Monad.Reader (asks)
import Data.Data (Data)
import Data.List.Extra (sortOn)
import Data.List.NonEmpty (NonEmpty (..), toList)
import Extras (Name, (<.>))

type NKObject = Kernel.Object Kernel.Type

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
      pure ([Kernel.DData (moduleName <.> typeName) ctorPairs | not (null ctorPairs)])
    DFunction _ name FunctionDefinition{..} -> do
      qs <- traverse translatePattern (toList functionDefinitionPatterns)
      f <- withLocalNames (nkLabelName <$> qs) (translateExpression functionDefinitionExpression)
      moduleName <- asks (kernelEnvironmentModule . compilerKernelEnvironment)
      pure [Kernel.DFunction Kernel.Exported (moduleName <.> name) qs f]
    DLet _ name LetDefinition{letDefinitionType = With{}, ..} -> do
      c <- translateExpression letDefinitionExpression
      moduleName <- asks (kernelEnvironmentModule . compilerKernelEnvironment)
      pure [Kernel.DConstant (moduleName <.> name) c]
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
traitAccessor :: (Monad m) => Name -> Name -> Kernel.Type -> CompilerT a m NKObject
traitAccessor trait fn t = do
  moduleName <- asks (kernelEnvironmentModule . compilerKernelEnvironment)
  let dict = Kernel.Label (Kernel.TCon trait [Kernel.TOpq]) "$a"
      rt = Kernel.RExt fn t Kernel.TOpq
      row = Kernel.Label rt "$r"
  pure $
    Kernel.DFunction
      Kernel.Exported
      (moduleName <.> fn)
      [dict]
      ( Kernel.ECase
          t
          (Kernel.EVar dict)
          ( Kernel.Clause
              (Kernel.Label (rt `T.arrow` Kernel.TCon "record" [rt]) "$Record" :| [row])
              (Kernel.EGet (Kernel.Label t fn) (Kernel.EVar row))
              :| []
          )
      )

{-# INLINE nkLabelName #-}
nkLabelName :: Kernel.Label t -> Name
nkLabelName (Kernel.Label _ name) = name
