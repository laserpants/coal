{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Builtin.Traits (
  builtinTraits,
  builtinTraits2,
  numeric,
  ordered,
  comparable,
  divisible,
  modulo,
  semigroup,
) where

import Coal.Language
import Coal.Language.Module
import Coal.ProtoLanguage.ProtoDefinition
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

builtinTraits2 :: (Monoid a) => [ProtoDefinition a () ()]
builtinTraits2 =
  [ ProtoDTrait
      mempty
      "Numeric"
      ( ProtoTraitDefinition
          mempty
          "Numeric"
          []
          (Parameter () "a")
          [ ProtoTraitDefinitionInterfaceEntry
              "from_int32"
              (Forall (Set.fromList [Parameter () "a"]) mempty $ TIntrinsic IInt32 `TArrow` TVariable (Parameter () "a"))
          , ProtoTraitDefinitionInterfaceEntry
              "from_int64"
              (Forall (Set.fromList [Parameter () "a"]) mempty $ TIntrinsic IInt64 `TArrow` TVariable (Parameter () "a"))
          , ProtoTraitDefinitionInterfaceEntry
              "from_bignum"
              ( Forall (Set.fromList [Parameter () "a"]) mempty $ TIntrinsic IBignum `TArrow` TVariable (Parameter () "a")
              )
          , ProtoTraitDefinitionInterfaceEntry
              "negate"
              ( Forall (Set.fromList [Parameter () "a"]) mempty $ TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a")
              )
          , ProtoTraitDefinitionInterfaceEntry
              "(+)"
              ( Forall (Set.fromList [Parameter () "a"]) mempty $ TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a")
              )
          , ProtoTraitDefinitionInterfaceEntry
              "(-)"
              ( Forall (Set.fromList [Parameter () "a"]) mempty $ TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a")
              )
          , ProtoTraitDefinitionInterfaceEntry
              "(*)"
              ( Forall (Set.fromList [Parameter () "a"]) mempty $ TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a")
              )
          ]
      )
  , ProtoDTrait
      mempty
      "Ordered"
      ( ProtoTraitDefinition
          mempty
          "Ordered"
          []
          (Parameter () "a")
          [ ProtoTraitDefinitionInterfaceEntry
              "compare"
              ( Forall (Set.fromList [Parameter () "a"]) mempty $ TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TConstructor () "Ordering"
              )
          ]
      )
  , ProtoDTrait
      mempty
      "Comparable"
      ( ProtoTraitDefinition
          mempty
          "Comparable"
          []
          (Parameter () "a")
          [ ProtoTraitDefinitionInterfaceEntry
              "(==)"
              ( Forall (Set.fromList [Parameter () "a"]) mempty $ TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TIntrinsic IBool
              )
          ]
      )
  , ProtoDTrait
      mempty
      "Divisible"
      ( ProtoTraitDefinition
          mempty
          "Divisible"
          []
          (Parameter () "a")
          [ ProtoTraitDefinitionInterfaceEntry
              "(/)"
              ( Forall (Set.fromList [Parameter () "a"]) mempty $ TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a")
              )
          ]
      )
  , ProtoDTrait
      mempty
      "Modulo"
      ( ProtoTraitDefinition
          mempty
          "Modulo"
          []
          (Parameter () "a")
          [ ProtoTraitDefinitionInterfaceEntry
              "(%)"
              ( Forall (Set.fromList [Parameter () "a"]) mempty $ TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a")
              )
          ]
      )
  , ProtoDTrait
      mempty
      "Semigroup"
      ( ProtoTraitDefinition
          mempty
          "Semigroup"
          []
          (Parameter () "a")
          [ ProtoTraitDefinitionInterfaceEntry
              "(<>)"
              ( Forall (Set.fromList [Parameter () "a"]) mempty $ TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a")
              )
          ]
      )
  ]

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
