{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Builtin.Traits (
  builtinTraits,
  numeric,
  ordered,
  comparable,
  divisible,
  modulo,
  semigroup,
) where

import Coal.Language
import Coal.Language.Module
import qualified Data.Set as Set

{-# INLINE numeric #-}
numeric :: ParameterizedType -> Trait ParameterizedType
numeric = Trait "Numeric"

{-# INLINE ordered #-}
ordered :: ParameterizedType -> Trait ParameterizedType
ordered = Trait "Ordered"

{-# INLINE comparable #-}
comparable :: ParameterizedType -> Trait ParameterizedType
comparable = Trait "Comparable"

{-# INLINE divisible #-}
divisible :: ParameterizedType -> Trait ParameterizedType
divisible = Trait "Divisible"

{-# INLINE modulo #-}
modulo :: ParameterizedType -> Trait ParameterizedType
modulo = Trait "Modulo"

{-# INLINE semigroup #-}
semigroup :: ParameterizedType -> Trait ParameterizedType
semigroup = Trait "Semigroup"

builtinTraits :: (Monoid a) => [Definition a k ()]
builtinTraits =
  [ DTrait
      mempty
      "Numeric"
      ( TraitDefinition
          mempty
          (Parameter () "a")
          [
            ( "from_int32"
            , Forall (Set.fromList [Parameter () "a"]) mempty $ TIntrinsic IInt32 `TArrow` TVariable (Parameter () "a")
            )
          ,
            ( "from_int64"
            , Forall (Set.fromList [Parameter () "a"]) mempty $ TIntrinsic IInt64 `TArrow` TVariable (Parameter () "a")
            )
          ,
            ( "from_bignum"
            , Forall (Set.fromList [Parameter () "a"]) mempty $ TIntrinsic IBignum `TArrow` TVariable (Parameter () "a")
            )
          ,
            ( "negate"
            , Forall (Set.fromList [Parameter () "a"]) mempty $ TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a")
            )
          ,
            ( "(+)"
            , Forall (Set.fromList [Parameter () "a"]) mempty $ TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a")
            )
          ,
            ( "(-)"
            , Forall (Set.fromList [Parameter () "a"]) mempty $ TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a")
            )
          ,
            ( "(*)"
            , Forall (Set.fromList [Parameter () "a"]) mempty $ TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a")
            )
          ]
      )
  , DTrait
      mempty
      "Ordered"
      ( TraitDefinition
          mempty
          (Parameter () "a")
          [
            ( "compare"
            , Forall (Set.fromList [Parameter () "a"]) mempty $ TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TConstructor () "Ordering"
            )
          ]
      )
  , DTrait
      mempty
      "Comparable"
      ( TraitDefinition
          mempty
          (Parameter () "a")
          [
            ( "(==)"
            , Forall (Set.fromList [Parameter () "a"]) mempty $ TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TIntrinsic IBool
            )
          ]
      )
  , DTrait
      mempty
      "Divisible"
      ( TraitDefinition
          mempty
          (Parameter () "a")
          [
            ( "(/)"
            , Forall (Set.fromList [Parameter () "a"]) mempty $ TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a")
            )
          ]
      )
  , DTrait
      mempty
      "Modulo"
      ( TraitDefinition
          mempty
          (Parameter () "a")
          [
            ( "(%)"
            , Forall (Set.fromList [Parameter () "a"]) mempty $ TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a")
            )
          ]
      )
  , DTrait
      mempty
      "Semigroup"
      ( TraitDefinition
          mempty
          (Parameter () "a")
          [
            ( "(<>)"
            , Forall (Set.fromList [Parameter () "a"]) mempty $ TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a")
            )
          ]
      )
  ]
