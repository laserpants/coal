{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Builtin.Definitions (
  module Coal.Compiler.Builtin.Functions,
  insertBuiltinDefinitions,
) where

import Coal.Compiler.Builtin.Functions (functions)
import Coal.Language
import Coal.Language.Module
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set
import Extras (Name, for)

{-# INLINE insertBuiltinDefinitions #-}
insertBuiltinDefinitions :: (Monoid a) => [Definition a k ()] -> [Definition a k ()]
insertBuiltinDefinitions = (definitions <>)

{-# INLINE functionNames #-}
functionNames :: [Name]
functionNames = for functions fst

traitInstances :: [Name]
traitInstances =
  [ "from_int32__$impl_Numeric(Intrinsic(Int32))"
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
    "(%)__$impl_Modulo(Intrinsic(Int32))"
  , "(%)__$impl_Modulo(Intrinsic(Int64))"
  , --
    "(<>)__$impl_Semigroup(Intrinsic(String))"
  , "(<>)__$impl_Semigroup(Application(Constructor(List))(Variable(Parameter(a))))"
  ]

definitions :: (Monoid a) => [Definition a k ()]
definitions =
  [ DImport
      mempty
      (Path ["Builtin$"])
      (functionNames <> traitInstances)
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
      "Modulo"
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
