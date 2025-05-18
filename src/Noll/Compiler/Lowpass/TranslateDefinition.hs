{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Compiler.Lowpass.TranslateDefinition (translateDefinition) where

import Control.Monad (forM)
import Control.Monad.Reader (MonadReader, asks)
import Data.Data (Data)
import Data.List.Extra (sortOn)
import Lang.Common.List1 (NonEmpty ((:|)), fromList1, (<|))
import Lang.Label (Label (..))
import Lang.Utils (Name, Set)
import Noll.Compiler.Lowpass.Environment (TranslateEnvironment (..), withLocalNames)
import Noll.Compiler.Lowpass.TranslateExpression (translateExpression, translatePattern)
import Noll.Compiler.Lowpass.TranslateType (translateType)
import Noll.Language
import Noll.Language.Trait (With (..))
import Noll.Module.Constant (Constant (..))
import Noll.Module.Definition
import Noll.Module.Function (Function (..))
import Noll.Utils (hashed)

import qualified Lang.Lowpass.Language as Lowpass

type LowpassObject = Lowpass.Object Lowpass.Type (Lowpass.Expr Lowpass.Type)

translateDefinition :: (MonadReader TranslateEnvironment m, Data a) => Definition a Kind IndexedType -> m [LowpassObject]
translateDefinition =
  \case
    DAnnotation _ d ->
      translateDefinition d
    DType _ _ ctors ->
      traverse translateConstructor (zip [0 ..] (sortOn constructorName ctors))
    DFunction name (Function _ (With _ t) ps e) -> do
      qs <- traverse translatePattern (fromList1 ps)
      f <- withLocalNames (labelName <$> qs) (translateExpression e)
      moduleName <- asks translateEnvironmentModule
      pure [Lowpass.OFunction (moduleName <> "." <> name) qs f]
    DConstant name (Constant _ With{} e) -> do
      c <- translateExpression e
      pure [Lowpass.OConstant name c]
    DTrait name _ _ ins -> do
      moduleName <- asks translateEnvironmentModule
      forM ins $
        \(n, t) ->
          traitAccessor name n (translateType t)
    DInstance _ t ds -> do
      moduleName <- asks translateEnvironmentModule
      bs <- forM ds $ do
        \case
          DFunction name f ->
            translateDefinition (DFunction (name <> postfix) f)
          DConstant name c ->
            translateDefinition (DConstant (name <> postfix) c)
      xx <- instanceDictionary
      pure (concat bs <> [xx])
     where
      postfix = "__$instance." <> hashed t
    _ ->
      pure []

instanceDictionary :: (MonadReader TranslateEnvironment m) => m LowpassObject
instanceDictionary =
  pure $
    Lowpass.OConstant
      undefined
      (
        Lowpass.recordExpr
          (
              Lowpass.ext
                undefined
                undefined
                undefined
          )
      )

xxx :: [Name] -> LowpassObject
xxx = undefined

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
