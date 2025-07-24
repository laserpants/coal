{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Compiler.Kernel.TranslateDefinition (translateDefinition) where

import Control.Monad (forM)
import Control.Monad.Reader (MonadReader, asks)
import Data.Data (Data)
import Data.List.Extra (sortOn)
import Lang.Common.List1 (NonEmpty ((:|)), fromList1, (<|))
import Lang.Label (Label (..))
import Lang.Utils (Name, (<$$>))
import Noll.Compiler.Kernel.Environment (TranslateEnvironment (..), withLocalNames)
import Noll.Compiler.Kernel.TranslateExpression (translateExpression, translatePattern)
import Noll.Compiler.Kernel.TranslateType (translateType)
import Noll.Language
import Noll.Language.Module

import qualified Lang.Lowpass.Language as Lowpass

type LowpassObject = Lowpass.Object Lowpass.Type (Lowpass.Expr Lowpass.Type)

translateDefinition :: (Show a, MonadReader TranslateEnvironment m, Data a) => Definition a Kind IndexedType -> m [LowpassObject]
translateDefinition =
  \case
    DAnnotation _ d ->
      translateDefinition d
    DType _ _ ctors ->
      traverse translateConstructor (zip [0 ..] (sortOn constructorName ctors))
    DFunction name (Function _ _ ps e) -> do
      qs <- traverse translatePattern (fromList1 ps)
      f <- withLocalNames (labelName <$> qs) (translateExpression e)
      moduleName <- asks translateEnvironmentModule
      pure [Lowpass.OFunction (moduleName <> "." <> name) qs f]
    DConstant name (Constant _ With{} e) -> do
      c <- translateExpression e
      moduleName <- asks translateEnvironmentModule
      pure [Lowpass.OConstant (moduleName <> "." <> name) c]
    DTrait name _ _ ins ->
      forM ins $
        \(n, t) ->
          traitAccessor name n (translateType t)
    DInstance name t ds ->
      concat <$$> forM ds $
        \case
          DFunction n f -> do
            translateDefinition (DFunction (n <> postfix) f)
          DConstant n c -> do
            translateDefinition (DConstant (n <> postfix) c)
          _ ->
            error "TODO"
     where
      postfix = "__$instance_" <> serialize (Trait name t)
    _ ->
      pure []

traitAccessor :: (MonadReader TranslateEnvironment m) => Name -> Name -> Lowpass.Type -> m LowpassObject
traitAccessor trait fn t = do
  moduleName <- asks translateEnvironmentModule
  pure $
    Lowpass.OFunction
      (moduleName <> "." <> fn)
      [dict]
      ( Lowpass.match
          t
          (Lowpass.var dict)
          ( Lowpass.Clause
              (Label (Lowpass.functionTypeOf dict [row]) "$Record" <| row :| [])
              ( Lowpass.sel
                  (Lowpass.Focus fn var (Label Lowpass.opaque "_"))
                  (Lowpass.var row)
                  (Lowpass.var var)
              )
              :| []
          )
      )
 where
  var = Label t "$f"
  row = Label (Lowpass.RExt fn t Lowpass.opaque) "$r"
  dict = Label (Lowpass.TCon trait [Lowpass.opaque]) "$a"

translateConstructor :: (MonadReader TranslateEnvironment m) => (Int, Constructor Parameter () (Type Parameter ())) -> m LowpassObject
translateConstructor (index, Constructor name _ (Forall _ _ t)) = do
  moduleName <- asks translateEnvironmentModule
  pure (Lowpass.OData (moduleName <> "." <> name) index (translateType t))
