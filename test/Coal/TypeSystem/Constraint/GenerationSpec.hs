{-# LANGUAGE OverloadedStrings #-}

module Coal.TypeSystem.Constraint.GenerationSpec where 

import Coal.Language
import Coal.TypeSystem.Constraint.Generation
import Coal.Common.Label (Label (..))

fixture1 :: Expression () IndexedType
fixture1 =
  EConstructor () (Label (TConstructor KType "Color") "Blue")

collectConstraintsSpec = a
  where
    a = runConstraintsGenStack (freshIdIn fixture1) ctx (collectConstraints fixture1)
    ctx = ConstraintsGenContext
      { constraintsGenContextMonomorphicSet = mempty
      , constraintsGenContextDataConstructorEnv = mempty
      , constraintsGenContextCodataAccessorEnv = mempty
      , constraintsGenContextTypeConstructorEnv = mempty
      }
