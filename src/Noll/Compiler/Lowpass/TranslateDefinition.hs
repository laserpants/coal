{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Noll.Compiler.Lowpass.TranslateDefinition (translateDefinition) where

import Control.Monad (forM)
import Control.Monad.Reader (MonadReader, asks)
import Data.Data (Data)
import Data.List.Extra (sortOn)
import Debug.Trace
import Debug.Trace (traceShow)
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
      -- pure [Lowpass.OConstant name c]
      moduleName <- asks translateEnvironmentModule
      pure [Lowpass.OConstant (moduleName <> "." <> name) c]
    DTrait name _ _ ins -> do
      moduleName <- asks translateEnvironmentModule
      forM ins $
        \(n, t) ->
          traitAccessor name n (translateType t)
    --    DInstance name t ds -> do
    --      moduleName <- asks translateEnvironmentModule
    --      bs <- forM ds $ do
    --        \case
    --          DFunction name f -> do
    --            xx1 <- translateDefinition (DFunction (name <> postfix) f)
    --            pure (name, xx1)
    --          DConstant name c -> do
    --            xx1 <- translateDefinition (DConstant (name <> postfix) c)
    --            pure (name, xx1)
    --      xx <- instanceDictionary2 postfix name t bs
    --      pure (concatMap snd bs <> [xx])
    --     where
    --      postfix = "__$instance." <> hashed (Trait name t)
    DInstance name t ds -> do
      moduleName <- asks translateEnvironmentModule
      bs <- forM ds $ do
        \case
          DFunction name f -> do
            xx1 <- translateDefinition (DFunction (name <> postfix) f)
            pure (name, xx1)
          DConstant name c -> do
            xx1 <- translateDefinition (DConstant (name <> postfix) c)
            pure (name, xx1)
      pure (concatMap snd bs)
     where
      --      xx <- instanceDictionary2 postfix name t bs
      --      pure (concatMap snd bs <> [xx])

      postfix = "__$instance." <> hashed (Trait name t)
    _ ->
      pure []

-- dictExpr :: Expr Lang.Type -> Expr Lang.Type
dictExpr t r =
  Lowpass.app
    t
    (Lowpass.var (Label (Lowpass.typeOf r `Lowpass.arrow` t) "$Record"))
    (r :| [])

instanceDictionary :: (MonadReader TranslateEnvironment m) => Name -> Name -> IndexedType -> [(Name, [LowpassObject])] -> m LowpassObject
instanceDictionary postfix name t xyz = do
  moduleName <- asks translateEnvironmentModule
  pure $
    Lowpass.OConstant
      (moduleName <> "." <> name <> postfix)
      (dictExpr d (foldr hello Lowpass.nil xyz))
 where
  d = Lowpass.TCon name [translateType t]

instanceDictionary2 :: (MonadReader TranslateEnvironment m) => Name -> Name -> Type Parameter Kind -> [(Name, [LowpassObject])] -> m LowpassObject
instanceDictionary2 postfix name t xyz = do
  moduleName <- asks translateEnvironmentModule
  pure $
    Lowpass.OConstant
      (moduleName <> "." <> name <> postfix)
      (dictExpr d (foldr hello Lowpass.nil xyz))
 where
  d = Lowpass.TCon name [translateType t]

-- hello :: [Name] -> LowpassObject
hello (n, obj) = Lowpass.ext n v
 where
  v =
    case obj of
      [z] ->
        Lowpass.var (Label (Lowpass.typeOf z) (Lowpass.objectName z))
      _ ->
        error "Implementation error"

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
