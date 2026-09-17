{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Builtin.Traits (
  builtinTraits,
  numericBase,
  numeric,
  ordered,
  comparable,
  divisible,
  modulo,
  semigroup,
) where

import Coal.Language
import qualified Data.Set as Set

{-# INLINE numericBase #-}
numericBase :: ParameterizedType -> Trait ParameterizedType
numericBase = Trait "NumericBase"

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

builtinTraits :: (Monoid a) => [Definition a () ()]
builtinTraits =
  [ DTrait
      mempty
      "NumericBase"
      ( TraitDefinition
          mempty
          "NumericBase"
          []
          (Parameter () "a")
          [ TraitDefinitionInterfaceEntry
              "from_int32"
              (Forall (Set.fromList [Parameter () "a"]) mempty $ TIntrinsic IInt32 `TArrow` TVariable (Parameter () "a"))
          , TraitDefinitionInterfaceEntry
              "from_int64"
              (Forall (Set.fromList [Parameter () "a"]) mempty $ TIntrinsic IInt64 `TArrow` TVariable (Parameter () "a"))
          , TraitDefinitionInterfaceEntry
              "from_bignum"
              (Forall (Set.fromList [Parameter () "a"]) mempty $ TIntrinsic IBignum `TArrow` TVariable (Parameter () "a"))
          , TraitDefinitionInterfaceEntry
              "(+)"
              (Forall (Set.fromList [Parameter () "a"]) mempty $ TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a"))
          , TraitDefinitionInterfaceEntry
              "(*)"
              (Forall (Set.fromList [Parameter () "a"]) mempty $ TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a"))
          ]
      )
  , DTrait
      mempty
      "Numeric"
      ( TraitDefinition
          mempty
          "Numeric"
          [Trait "NumericBase" (Parameter () "a")]
          (Parameter () "a")
          [ TraitDefinitionInterfaceEntry
              "from_negative_int32"
              (Forall (Set.fromList [Parameter () "a"]) mempty $ TIntrinsic IInt32 `TArrow` TVariable (Parameter () "a"))
          , TraitDefinitionInterfaceEntry
              "from_negative_int64"
              (Forall (Set.fromList [Parameter () "a"]) mempty $ TIntrinsic IInt64 `TArrow` TVariable (Parameter () "a"))
          , TraitDefinitionInterfaceEntry
              "from_negative_bignum"
              (Forall (Set.fromList [Parameter () "a"]) mempty $ TIntrinsic IBignum `TArrow` TVariable (Parameter () "a"))
          , TraitDefinitionInterfaceEntry
              "negate"
              (Forall (Set.fromList [Parameter () "a"]) mempty $ TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a"))
          , TraitDefinitionInterfaceEntry
              "(-)"
              (Forall (Set.fromList [Parameter () "a"]) mempty $ TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a"))
          ]
      )
  , DTrait
      mempty
      "Ordered"
      ( TraitDefinition
          mempty
          "Ordered"
          []
          (Parameter () "a")
          [ TraitDefinitionInterfaceEntry
              "compare"
              (Forall (Set.fromList [Parameter () "a"]) mempty $ TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TConstructor () "Ordering")
          ]
      )
  , DTrait
      mempty
      "Comparable"
      ( TraitDefinition
          mempty
          "Comparable"
          []
          (Parameter () "a")
          [ TraitDefinitionInterfaceEntry
              "(==)"
              (Forall (Set.fromList [Parameter () "a"]) mempty $ TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TIntrinsic IBool)
          ]
      )
  , DTrait
      mempty
      "Divisible"
      ( TraitDefinition
          mempty
          "Divisible"
          []
          (Parameter () "a")
          [ TraitDefinitionInterfaceEntry
              "(/)"
              (Forall (Set.fromList [Parameter () "a"]) mempty $ TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a"))
          ]
      )
  , DTrait
      mempty
      "Modulo"
      ( TraitDefinition
          mempty
          "Modulo"
          []
          (Parameter () "a")
          [ TraitDefinitionInterfaceEntry
              "(%)"
              (Forall (Set.fromList [Parameter () "a"]) mempty $ TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a"))
          ]
      )
  , DTrait
      mempty
      "Semigroup"
      ( TraitDefinition
          mempty
          "Semigroup"
          []
          (Parameter () "a")
          [ TraitDefinitionInterfaceEntry
              "(<>)"
              (Forall (Set.fromList [Parameter () "a"]) mempty $ TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a"))
          ]
      )
  ]
