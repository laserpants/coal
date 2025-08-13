{-# LANGUAGE OverloadedStrings #-}

module Coal.TypeSystem.Constraint.GenerationSpec where

import Coal.Common.Label (Label (..))
import Coal.Language
import Coal.TypeSystem.Constraint.Generation

fixture1 :: Expression () IndexedType
fixture1 =
  EConstructor () (Label (TConstructor KType "Color") "Blue")

collectConstraintsSpec = r
 where
  r = evalConstraintsGenStack (freshIdIn fixture1) ctx (collectConstraints fixture1)
  ctx =
    ConstraintsGenContext
      { constraintsGenContextMonomorphicSet = mempty
      , constraintsGenContextDataConstructorEnv = mempty
      , constraintsGenContextCodataAccessorEnv = mempty
      , constraintsGenContextTypeConstructorEnv = mempty
      }
