{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Kernel.Translate.Pattern (translatePattern) where

import Coal.Common.Label (Label (..))
import Coal.Compiler.Kernel.Translate.Type (translateType)
import Coal.Compiler.Stack (CompilerT)
import qualified Coal.Kernel.Language.Expr as Kernel (Label (..))
import qualified Coal.Kernel.Language.Type as Kernel
import Coal.Language
import Data.Data (Data)

translatePattern :: (Monad m, Data a) => Pattern a k IndexedType -> CompilerT a m (Kernel.Label Kernel.Type)
translatePattern =
  \case
    PAny a t ->
      translatePattern (PVariable a (Label t "_"))
    PVariable _ (Label t name) ->
      pure (Kernel.Label (translateType t) name)
    PAnnotation _ _ p ->
      translatePattern p
    PLiteral _ p ->
      pure (Kernel.Label (translateType (typeOf p)) "_")
    PTraitInstance _ t trait ->
      pure (Kernel.Label (translateType t) (dictionaryLabel trait))
    _ ->
      error "Not implemented"
