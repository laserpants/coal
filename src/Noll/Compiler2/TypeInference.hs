{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler2.TypeInference where

import Noll.Compiler2.Internal
import Noll.Language
import Noll.Module (Definition (..))
import Noll.SystemF

type CompilerAssumption = Assumption IndexedType

type CompilerConstraint a = Constraint (InferenceRule Kind a) TypeIndex Kind IndexedType

typeDefinitionsC :: [Definition a k IndexedType] -> Compiler2T m ([Definition a Kind IndexedType], [CompilerAssumption])
typeDefinitionsC = undefined

typeDefinitionC :: Definition a k IndexedType -> Compiler2T m (Definition a Kind IndexedType)
typeDefinitionC =
  \case
    _ ->
      undefined
