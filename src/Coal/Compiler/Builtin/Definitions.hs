{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Builtin.Definitions (insertBuiltinDefinitions, names) where

import Coal.Language
import Coal.Language.Module
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set
import Extras (Name, for)

{-# INLINE insertBuiltinDefinitions #-}
insertBuiltinDefinitions :: (Monoid a) => [Definition a k ()] -> [Definition a k ()]
insertBuiltinDefinitions = (definitions <>)

definitions :: (Monoid a) => [Definition a k ()]
definitions =
  [ DImport
      mempty
      (Path ["Builtin$"])
      ( for names fst
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
    , forall0 (TIntrinsic IBool ~> TIntrinsic IBool)
    )
  ,
    ( "not"
    , forall0 (TIntrinsic IBool ~> TIntrinsic IBool)
    )
  ,
    ( "operator$__reverse_composition"
    , forall3 $ \t0 t1 t2 -> (t1 ~> t2) ~> (t0 ~> t1) ~> t0 ~> t2
    )
  ,
    ( "operator$__reverse_application"
    , forall2 $ \t0 t1 -> t0 ~> (t0 ~> t1) ~> t1
    )
  ,
    ( "always"
    , forall2 $ \t0 t1 -> t0 ~> t1 ~> t0
    )
  ,
    ( "operator$__list_concatenation"
    , forall1 $ \t0 -> listType t0 ~> listType t0 ~> listType t0
    )
  ,
    ( "trace_int32"
    , forall0 (TIntrinsic IInt32 ~> TApplication KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| []))
    )
  ,
    ( "trace_int64"
    , forall0 (TIntrinsic IInt64 ~> TApplication KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| []))
    )
  ,
    ( "trace_bignum"
    , forall0 (TIntrinsic IBignum ~> TApplication KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| []))
    )
  ,
    ( "trace_string"
    , forall0 (TIntrinsic IString ~> TApplication KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| []))
    )
  ,
    ( "trace_bool"
    , forall0 (TIntrinsic IBool ~> TApplication KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| []))
    )
  ,
    ( "trace_char"
    , forall0 (TIntrinsic IChar ~> TApplication KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| []))
    )
  ,
    ( "trace_float"
    , forall0 (TIntrinsic IFloat ~> TApplication KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| []))
    )
  ,
    ( "trace_double"
    , forall0 (TIntrinsic IDouble ~> TApplication KType (TConstructor (KArrow KType KType) "IO") (TIntrinsic IUnit :| []))
    )
  ,
    ( "operator$__string_concatenation"
    , forall0 (TIntrinsic IString ~> TIntrinsic IString ~> TIntrinsic IString)
    )
  ,
    ( "int32_to_string"
    , forall0 (TIntrinsic IInt32 `TArrow` TIntrinsic IString)
    )
  ,
    ( "float_to_string"
    , forall0 (TIntrinsic IFloat ~> TIntrinsic IString)
    )
  ,
    ( "double_to_string"
    , forall0 (TIntrinsic IDouble ~> TIntrinsic IString)
    )
  ,
    ( "unpack_nat"
    , forall0 (TIntrinsic INat ~> TIntrinsic IInt32)
    )
  ,
    ( "pack_nat"
    , forall0 (TIntrinsic IInt32 ~> TIntrinsic INat)
    )
  ,
    ( "from_int32"
    , forall1' ( \t0 -> ( [Trait "Numeric" t0] , TIntrinsic IInt32 `TArrow` t0))
    )
  ,
    ( "negate"
    , forall1' (\t0 -> ([Trait "Numeric" t0], t0 `TArrow` t0))
    )
  ,
    ( "compare"
    , forall1' ( \t0 -> ( [Trait "Ordered" t0] , t0 ~> t0 ~> TConstructor KType "Ordering"))
    )
  ,
    ( "string_to_list"
    , forall0 (TIntrinsic IString ~> listType (TIntrinsic IChar))
    )
  ,
    ( "string_head"
    , forall0 (TIntrinsic IString ~> TIntrinsic IChar)
    )
  ,
    ( "string_tail"
    , forall0 (TIntrinsic IString ~> TIntrinsic IString)
    )
  ,
    ( "string_reverse"
    , forall0 (TIntrinsic IString ~> TIntrinsic IString)
    )
  ,
    ( "string_remove_whitespace"
    , forall0 (TIntrinsic IString ~> TIntrinsic IString)
    )
  ,
    ( "string_length"
    , forall0 (TIntrinsic IString ~> TIntrinsic IInt32)
    )
  ,
    ( "(<)"
    , forall1' (\t0 -> ([Trait "Ordered" t0], t0 ~> t0 ~> TIntrinsic IBool))
    )
  ,
    ( "(>)"
    , forall1' (\t0 -> ([Trait "Ordered" t0], t0 ~> t0 ~> TIntrinsic IBool))
    )
  ,
    ( "(<=)"
    , forall1' (\t0 -> ([Trait "Ordered" t0], t0 ~> t0 ~> TIntrinsic IBool))
    )
  ,
    ( "(>=)"
    , forall1' (\t0 -> ([Trait "Ordered" t0], t0 ~> t0 ~> TIntrinsic IBool))
    )
  ,
    ( "(^)"
    , forall1' (\t0 -> ([Trait "Numeric" t0], t0 ~> TIntrinsic INat ~> t0))
    )
  ]
