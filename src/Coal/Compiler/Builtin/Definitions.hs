{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Builtin.Definitions (insertBuiltinDefinitions, names) where

import Coal.Language
import Coal.Language.Module
import Data.List.NonEmpty (NonEmpty (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Extra (Name)

{-# INLINE insertBuiltinDefinitions #-}
insertBuiltinDefinitions :: (Monoid a) => [Definition a k ()] -> [Definition a k ()]
insertBuiltinDefinitions = (definitions <>)

definitions :: (Monoid a) => [Definition a k ()]
definitions =
  [ DImport
      mempty
      (Path ["Builtin$"])
      ( (fst <$> names)
          <> [ "from_int32__$impl_Numeric(Intrinsic(Int32))"
             , "(+)__$impl_Numeric(Intrinsic(Int32))"
             , "(-)__$impl_Numeric(Intrinsic(Int32))"
             , "(*)__$impl_Numeric(Intrinsic(Int32))"
             , "negate__$impl_Numeric(Intrinsic(Int32))"
             , --
               "from_int32__$impl_Numeric(Intrinsic(Int64))"
             , "(+)__$impl_Numeric(Intrinsic(Int64))"
             , "(-)__$impl_Numeric(Intrinsic(Int64))"
             , "(*)__$impl_Numeric(Intrinsic(Int64))"
             , "negate__$impl_Numeric(Intrinsic(Int64))"
             , --
               "from_int32__$impl_Numeric(Intrinsic(Float))"
             , "(+)__$impl_Numeric(Intrinsic(Float))"
             , "(-)__$impl_Numeric(Intrinsic(Float))"
             , "(*)__$impl_Numeric(Intrinsic(Float))"
             , "negate__$impl_Numeric(Intrinsic(Float))"
             , --
               "from_int32__$impl_Numeric(Intrinsic(Double))"
             , "(+)__$impl_Numeric(Intrinsic(Double))"
             , "(-)__$impl_Numeric(Intrinsic(Double))"
             , "(*)__$impl_Numeric(Intrinsic(Double))"
             , "negate__$impl_Numeric(Intrinsic(Double))"
             , --
               "from_int32__$impl_Numeric(Intrinsic(Nat))"
             , "(+)__$impl_Numeric(Intrinsic(Nat))"
             , "(-)__$impl_Numeric(Intrinsic(Nat))"
             , "(*)__$impl_Numeric(Intrinsic(Nat))"
             , "negate__$impl_Numeric(Intrinsic(Nat))"
             , --
               "from_int32__$impl_Numeric(Intrinsic(Bignum))"
             , "(+)__$impl_Numeric(Intrinsic(Bignum))"
             , "(-)__$impl_Numeric(Intrinsic(Bignum))"
             , "(*)__$impl_Numeric(Intrinsic(Bignum))"
             , "negate__$impl_Numeric(Intrinsic(Bignum))"
             , --
               "compare__$impl_Ordered(Intrinsic(Int32))"
             , "compare__$impl_Ordered(Intrinsic(Int64))"
             , "compare__$impl_Ordered(Intrinsic(Nat))"
             , "compare__$impl_Ordered(Intrinsic(Float))"
             , "compare__$impl_Ordered(Intrinsic(Double))"
             , "compare__$impl_Ordered(Intrinsic(Bool))"
             , "compare__$impl_Ordered(Intrinsic(Char))"
             , "compare__$impl_Ordered(Intrinsic(Bignum))"
             , --
               "(==)__$impl_Comparable(Intrinsic(Int32))"
             , "(==)__$impl_Comparable(Intrinsic(Int64))"
             , "(==)__$impl_Comparable(Intrinsic(Nat))"
             , "(==)__$impl_Comparable(Intrinsic(Float))"
             , "(==)__$impl_Comparable(Intrinsic(Double))"
             , "(==)__$impl_Comparable(Intrinsic(Bool))"
             , "(==)__$impl_Comparable(Intrinsic(Char))"
             , "(==)__$impl_Comparable(Intrinsic(Bignum))"
             , --
               "(/)__$impl_Divisible(Intrinsic(Float))"
             , "(/)__$impl_Divisible(Intrinsic(Double))"
             , --
               "(%)__$impl_Mod(Intrinsic(Int32))"
             , "(%)__$impl_Mod(Intrinsic(Int64))"
             , --
               "(<>)__$impl_Semigroup(Intrinsic(String))"
             , "(<>)__$impl_Semigroup(Application(Constructor(List))(Variable(Parameter(a))))"
             ]
      )
  , DTrait
      mempty
      "Numeric"
      ( TraitDef
          []
          (Parameter KType "a")
          [
            ( "from_int32"
            , TIntrinsic IInt32 `TArrow` TVariable (Parameter () "a")
            )
          ,
            ( "negate"
            , TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a")
            )
          ,
            ( "(+)"
            , TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a")
            )
          ,
            ( "(-)"
            , TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a")
            )
          ,
            ( "(*)"
            , TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a")
            )
          ]
      )
  , DTrait
      mempty
      "Ordered"
      ( TraitDef
          []
          (Parameter KType "a")
          [
            ( "compare"
            , TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TConstructor () "Ordering"
            )
          ]
      )
  , DTrait
      mempty
      "Comparable"
      ( TraitDef
          []
          (Parameter KType "a")
          [
            ( "(==)"
            , TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TIntrinsic IBool
            )
          ]
      )
  , DTrait
      mempty
      "Divisible"
      ( TraitDef
          []
          (Parameter KType "a")
          [
            ( "(/)"
            , TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a")
            )
          ]
      )
  , DTrait
      mempty
      "Mod"
      ( TraitDef
          []
          (Parameter KType "a")
          [
            ( "(%)"
            , TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a")
            )
          ]
      )
  , DTrait
      mempty
      "Semigroup"
      ( TraitDef
          []
          (Parameter KType "a")
          [
            ( "(<>)"
            , TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a")
            )
          ]
      )
  , DType
      mempty
      "Ordering"
      ( TypeDef
          []
          [ DataConstructor "LessThan" 0 (Forall mempty [] (TConstructor () "Ordering"))
          , DataConstructor "GreaterThan" 0 (Forall mempty [] (TConstructor () "Ordering"))
          , DataConstructor "EqualTo" 0 (Forall mempty [] (TConstructor () "Ordering"))
          ]
      )
  , DType
      mempty
      "Option"
      ( TypeDef
          [Parameter () "a"]
          [ DataConstructor "Some" 1 (Forall (Set.fromList [Parameter () "a"]) [] (TVariable (Parameter () "a") `TArrow` TApplication () (TConstructor () "Option") (TVariable (Parameter () "a") :| [])))
          , DataConstructor "None" 0 (Forall (Set.fromList [Parameter () "a"]) [] (TApplication () (TConstructor () "Option") (TVariable (Parameter () "a") :| [])))
          ]
      )
  , DType
      mempty
      "IO"
      (TypeDef [Parameter () "a"] [])
  ]

names :: [(Name, IndexedScheme)]
names =
  [
    ( "operator$__not"
    , Forall mempty [] (TIntrinsic IBool `TArrow` TIntrinsic IBool)
    )
  ,
    ( "not"
    , Forall mempty [] (TIntrinsic IBool `TArrow` TIntrinsic IBool)
    )
  ,
    ( "operator$__reverse_composition"
    , Forall
        (Set.fromList [TypeIndex KType 0, TypeIndex KType 1, TypeIndex KType 2])
        []
        ( (TVariable (TypeIndex KType 1) `TArrow` TVariable (TypeIndex KType 2))
            `TArrow` (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 1))
            `TArrow` TVariable (TypeIndex KType 0)
            `TArrow` TVariable (TypeIndex KType 2)
        )
    )
  ,
    ( "operator$__reverse_application"
    , Forall
        (Set.fromList [TypeIndex KType 0, TypeIndex KType 1])
        []
        ( TVariable (TypeIndex KType 0)
            `TArrow` (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 1))
            `TArrow` TVariable (TypeIndex KType 1)
        )
    )
  ,
    ( "always"
    , Forall
        (Set.fromList [TypeIndex KType 0, TypeIndex KType 1])
        []
        ( TVariable (TypeIndex KType 0)
            `TArrow` TVariable (TypeIndex KType 1)
            `TArrow` TVariable (TypeIndex KType 0)
        )
    )
  ,
    ( "operator$__list_concatenation"
    , Forall
        (Set.fromList [TypeIndex KType 0])
        []
        ( listType (TVariable (TypeIndex KType 0))
            `TArrow` listType (TVariable (TypeIndex KType 0))
            `TArrow` listType (TVariable (TypeIndex KType 0))
        )
    )
  ,
    ( "trace_int32"
    , Forall
        (Set.fromList [TypeIndex KType 0])
        []
        ( TIntrinsic IInt32 `TArrow` TApplication KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| [])
        )
    )
  ,
    ( "trace_int64"
    , Forall
        (Set.fromList [TypeIndex KType 0])
        []
        ( TIntrinsic IInt64 `TArrow` TApplication KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| [])
        )
    )
  ,
    ( "trace_bignum"
    , Forall
        (Set.fromList [TypeIndex KType 0])
        []
        ( TIntrinsic IBignum `TArrow` TApplication KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| [])
        )
    )
  ,
    ( "trace_string"
    , Forall
        (Set.fromList [TypeIndex KType 0])
        []
        ( TIntrinsic IString `TArrow` TApplication KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| [])
        )
    )
  ,
    ( "trace_bool"
    , Forall
        (Set.fromList [TypeIndex KType 0])
        []
        ( TIntrinsic IBool `TArrow` TApplication KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| [])
        )
    )
  ,
    ( "trace_char"
    , Forall
        (Set.fromList [TypeIndex KType 0])
        []
        ( TIntrinsic IChar `TArrow` TApplication KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| [])
        )
    )
  ,
    ( "trace_float"
    , Forall
        (Set.fromList [TypeIndex KType 0])
        []
        ( TIntrinsic IFloat `TArrow` TApplication KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| [])
        )
    )
  ,
    ( "trace_double"
    , Forall
        (Set.fromList [TypeIndex KType 0])
        []
        ( TIntrinsic IDouble `TArrow` TApplication KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| [])
        )
    )
  ,
    ( "operator$__string_concatenation"
    , Forall
        mempty
        []
        (TIntrinsic IString `TArrow` TIntrinsic IString `TArrow` TIntrinsic IString)
    )
  ,
    ( "int32_to_string"
    , Forall
        mempty
        []
        ( TIntrinsic IInt32 `TArrow` TIntrinsic IString
        )
    )
  ,
    ( "float_to_string"
    , Forall
        mempty
        []
        ( TIntrinsic IFloat `TArrow` TIntrinsic IString
        )
    )
  ,
    ( "double_to_string"
    , Forall
        mempty
        []
        ( TIntrinsic IDouble `TArrow` TIntrinsic IString
        )
    )
  ,
    ( "unpack_nat"
    , Forall
        mempty
        []
        ( TIntrinsic INat `TArrow` TIntrinsic IInt32
        )
    )
  ,
    ( "pack_nat"
    , Forall
        mempty
        []
        ( TIntrinsic IInt32 `TArrow` TIntrinsic INat
        )
    )
  ,
    ( "from_int32"
    , Forall
        (Set.fromList [TypeIndex KType 0] :: Set (TypeIndex Kind))
        [Trait "Numeric" (TVariable (TypeIndex KType 0))]
        (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 0))
    )
  ,
    ( "negate"
    , Forall
        (Set.fromList [TypeIndex KType 0] :: Set (TypeIndex Kind))
        [Trait "Numeric" (TVariable (TypeIndex KType 0))]
        (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0))
    )
  ,
    ( "compare"
    , Forall
        (Set.fromList [TypeIndex KType 0] :: Set (TypeIndex Kind))
        [Trait "Ordered" (TVariable (TypeIndex KType 0))]
        ( TVariable (TypeIndex KType 0)
            `TArrow` TVariable (TypeIndex KType 0)
            `TArrow` TConstructor KType "Ordering"
        )
    )
  ,
    ( "string_to_list"
    , Forall
        mempty
        []
        (TIntrinsic IString `TArrow` listType (TIntrinsic IChar))
    )
  ,
    ( "string_head"
    , Forall
        mempty
        []
        (TIntrinsic IString `TArrow` TIntrinsic IChar)
    )
  ,
    ( "string_tail"
    , Forall
        mempty
        []
        (TIntrinsic IString `TArrow` TIntrinsic IString)
    )
  ,
    ( "string_reverse"
    , Forall
        mempty
        []
        (TIntrinsic IString `TArrow` TIntrinsic IString)
    )
  ,
    ( "string_remove_whitespace"
    , Forall
        mempty
        []
        (TIntrinsic IString `TArrow` TIntrinsic IString)
    )
  ,
    ( "string_length"
    , Forall
        mempty
        []
        (TIntrinsic IString `TArrow` TIntrinsic IInt32)
    )
  ,
    ( "(<)"
    , Forall
        (Set.fromList [TypeIndex KType 0] :: Set (TypeIndex Kind))
        [Trait "Ordered" (TVariable (TypeIndex KType 0))]
        (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool)
    )
  ,
    ( "(>)"
    , Forall
        (Set.fromList [TypeIndex KType 0] :: Set (TypeIndex Kind))
        [Trait "Ordered" (TVariable (TypeIndex KType 0))]
        (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool)
    )
  ,
    ( "(<=)"
    , Forall
        (Set.fromList [TypeIndex KType 0] :: Set (TypeIndex Kind))
        [Trait "Ordered" (TVariable (TypeIndex KType 0))]
        (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool)
    )
  ,
    ( "(>=)"
    , Forall
        (Set.fromList [TypeIndex KType 0] :: Set (TypeIndex Kind))
        [Trait "Ordered" (TVariable (TypeIndex KType 0))]
        (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool)
    )
  ,
    ( "(^)"
    , Forall
        (Set.fromList [TypeIndex KType 0] :: Set (TypeIndex Kind))
        [Trait "Numeric" (TVariable (TypeIndex KType 0))]
        (TVariable (TypeIndex KType 0) `TArrow` TIntrinsic INat `TArrow` TVariable (TypeIndex KType 0))
    )
  ]
