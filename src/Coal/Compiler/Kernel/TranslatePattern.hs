{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Kernel.TranslatePattern (translatePattern) where

import Coal.Common.Label (Label (..))
import Coal.Compiler.Kernel.TranslateType (translateType)
import Coal.Compiler.Stack (CompilerT)
import qualified Coal.Kernel.Language as Kernel
import Coal.Language
import Data.Data (Data)

translatePattern :: (Monad m, Data a) => Pattern a IndexedType -> CompilerT a m (Label Kernel.Type)
translatePattern =
  \case
    PAny a t ->
      translatePattern (PVariable a (Label t "_"))
    PVariable _ (Label t name) ->
      pure (Label (translateType t) name)
    PAnnotation _ _ p ->
      translatePattern p
    PLiteral _ p ->
      pure (Label (translateType (typeOf p)) "_")
    PTraitDictionary _ t trait ->
      pure (Label (translateType t) (dictionaryLabel trait))
    _ ->
      error "Not implemented"
