{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Kernel.NewTranslate.Pattern (translatePattern) where

import Coal.Common.Label (Label (..))
import Coal.Compiler.Kernel.NewTranslate.Type (translateType)
import Coal.Compiler.Stack (CompilerT)
import qualified Coal.Kernel.Language.Expr as NK (Label (..))
import qualified Coal.Kernel.Language.Type as NKT
import Coal.Language
import Data.Data (Data)

translatePattern :: (Monad m, Data a) => Pattern a k IndexedType -> CompilerT a m (NK.Label NKT.Type)
translatePattern =
  \case
    PAny a t ->
      translatePattern (PVariable a (Label t "_"))
    PVariable _ (Label t name) ->
      pure (NK.Label (translateType t) name)
    PAnnotation _ _ p ->
      translatePattern p
    PLiteral _ p ->
      pure (NK.Label (translateType (typeOf p)) "_")
    PTraitInstance _ t trait ->
      pure (NK.Label (translateType t) (dictionaryLabel trait))
    _ ->
      error "Not implemented"
