{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Kernel.TranslateDefinition (translateDefinition) where

import Debug.Trace
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
    DFunction name (Function _ _ ps e) -> do
      qs <- traverse translatePattern (fromList1 ps)
      f <- withLocalNames (labelName <$> qs) (translateExpression e)
      moduleName <- asks kernelEnvironmentModule
      pure [Kernel.OFunction (moduleName <> "." <> name) qs f]
    DConstant name (Constant _ With{} e) -> do
      c <- translateExpression e
      moduleName <- asks kernelEnvironmentModule
      pure [Kernel.OConstant (moduleName <> "." <> name) c]
    DTrait name _ _ ins -> do
      forM ins $
        \(n, t) ->
          traitAccessor name n (translateType t)
    DInstance name t ds -> do
      bs <- forM ds $
        \case
          DFunction n f -> do
            xx1 <- translateDefinition (DFunction (n <> postfix) f)
            pure (n, xx1)
          DConstant n c -> do
            xx1 <- translateDefinition (DConstant (n <> postfix) c)
            pure (n, xx1)
          _ ->
            error "TODO"
      xx <- instanceDictionary2 postfix name t bs
      pure (concatMap snd bs <> [xx])
     where
      postfix = "__$instance_" <> serialize (Trait name t)
    _ ->
      pure []

-- dictExpr :: Expr Lang.Type -> Expr Lang.Type
dictExpr t r =
  Kernel.app
    t
    (Kernel.var (Label (Kernel.typeOf r `Kernel.arrow` t) "$Record"))
    (r :| [])

--instanceDictionary2 :: (MonadReader KernelEnvironment m) => Name -> Name -> Type Parameter Kind -> [(Name, [KernelObject])] -> m KernelObject
instanceDictionary2 postfix name t xyz = do
  moduleName <- asks kernelEnvironmentModule
  pure $
    Kernel.OConstant
      (moduleName <> "." <> "d_" <> name <> postfix)
      (dictExpr d (foldr hello Kernel.nil xyz))
 where
  d = Kernel.TCon name [translateType t]

-- hello :: [Name] -> KernelObject
hello (n, obj) = Kernel.ext n v
 where
  v =
    case obj of
      [z] ->
        Kernel.var (Label (Kernel.typeOf z) (Kernel.objectName z))
      _ ->
        error "Implementation error"

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
