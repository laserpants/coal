{-# LANGUAGE OverloadedStrings #-}

module Coal.TypeSystem.Constraint.GenerationSpec where

import Coal.Common.Label (Label (..))
import Coal.Language
import Coal.Language.Constructor (Constructor (..))
import Coal.TypeSystem.Constraint.Generation
import Data.Either (lefts, rights)

import qualified Coal.Common.Environment as Environment

fixture1 :: Expression () IndexedType
fixture1 =
  EConstructor () (Label (TConstructor KType "Color") "Blue")

collectConstraintsSpec1 = (ENoDataConstructor () "Blue") `elem` lefts outs
 where
  (_, outs) = evalConstraintsGenStack (freshIdIn fixture1) ctx (collectConstraints fixture1)
  ctx =
    ConstraintsGenContext
      { constraintsGenContextMonomorphicSet = mempty
      , constraintsGenContextDataConstructorEnv = mempty
      , constraintsGenContextCodataAccessorEnv = mempty
      , constraintsGenContextTypeConstructorEnv = mempty
      }

collectConstraintsSpec2 = null (lefts outs)
 where
  (_, outs) = evalConstraintsGenStack (freshIdIn fixture1) ctx (collectConstraints fixture1)
  ctx =
    ConstraintsGenContext
      { constraintsGenContextMonomorphicSet = mempty
      , constraintsGenContextDataConstructorEnv =
          Environment.fromList
            [
              ( "Blue"
              , Constructor "Blue" 0 (Forall mempty [] (TConstructor KType "Color"))
              )
            ]
      , constraintsGenContextCodataAccessorEnv = mempty
      , constraintsGenContextTypeConstructorEnv = mempty
      }
