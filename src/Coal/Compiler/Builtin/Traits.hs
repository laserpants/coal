{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Builtin.Traits (builtinTraits) where

import Coal.Language
import Coal.Language.Module
import qualified Data.Set as Set

builtinTraits :: (Monoid a) => [Definition a k ()]
builtinTraits =
  [ DTrait
      mempty
      "Numeric"
      ( TraitDefinition
          []
          (Parameter () "a")
          [
            ( "from_int32"
            , Forall (Set.fromList [Parameter () "a"]) [] $ TIntrinsic IInt32 `TArrow` TVariable (Parameter () "a")
            )
          ,
            ( "from_int64"
            , Forall (Set.fromList [Parameter () "a"]) [] $ TIntrinsic IInt64 `TArrow` TVariable (Parameter () "a")
            )
          ,
            ( "from_bignum"
            , Forall (Set.fromList [Parameter () "a"]) [] $ TIntrinsic IBignum `TArrow` TVariable (Parameter () "a")
            )
          ,
            ( "negate"
            , Forall (Set.fromList [Parameter () "a"]) [] $ TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a")
            )
          ,
            ( "(+)"
            , Forall (Set.fromList [Parameter () "a"]) [] $ TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a")
            )
          ,
            ( "(-)"
            , Forall (Set.fromList [Parameter () "a"]) [] $ TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a")
            )
          ,
            ( "(*)"
            , Forall (Set.fromList [Parameter () "a"]) [] $ TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a")
            )
          ]
      )
  , DTrait
      mempty
      "Ordered"
      ( TraitDefinition
          []
          (Parameter () "a")
          [
            ( "compare"
            , Forall (Set.fromList [Parameter () "a"]) [] $ TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TConstructor () "Ordering"
            )
          ]
      )
  , DTrait
      mempty
      "Comparable"
      ( TraitDefinition
          []
          (Parameter () "a")
          [
            ( "(==)"
            , Forall (Set.fromList [Parameter () "a"]) [] $ TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TIntrinsic IBool
            )
          ]
      )
  , DTrait
      mempty
      "Divisible"
      ( TraitDefinition
          []
          (Parameter () "a")
          [
            ( "(/)"
            , Forall (Set.fromList [Parameter () "a"]) [] $ TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a")
            )
          ]
      )
  , DTrait
      mempty
      "Modulo"
      ( TraitDefinition
          []
          (Parameter () "a")
          [
            ( "(%)"
            , Forall (Set.fromList [Parameter () "a"]) [] $ TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a")
            )
          ]
      )
  , DTrait
      mempty
      "Semigroup"
      ( TraitDefinition
          []
          (Parameter () "a")
          [
            ( "(<>)"
            , Forall (Set.fromList [Parameter () "a"]) [] $ TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a")
            )
          ]
      )
  ]
