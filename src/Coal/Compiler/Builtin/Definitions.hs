{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Builtin.Definitions (
  module Coal.Compiler.Builtin.Functions,
  insertBuiltinDefinitions,
  insertExtraDefinitions,
  builtinTraitInstances,
) where

import Coal.Compiler.Builtin.Functions (builtinFunctions)
import Coal.Compiler.Builtin.Traits (builtinTraits)
import Coal.Language
import Coal.Language.Module
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set
import Extras (Name, for)

{-# INLINE insertBuiltinDefinitions #-}
insertBuiltinDefinitions :: (Monoid a) => [Definition a k ()] -> [Definition a k ()]
insertBuiltinDefinitions = (builtinDefinitions <>)

{-# INLINE insertExtraDefinitions #-}
insertExtraDefinitions :: (Monoid a) => [Definition a k ()] -> [Definition a k ()]
insertExtraDefinitions = (extraDefinitions <>)

builtinFunctionNames :: [Name]
builtinFunctionNames = for builtinFunctions fst

builtinTraitInstances :: [Name]
builtinTraitInstances =
  [ "from_bignum__$impl_Numeric(Intrinsic(Int32))"
  , "from_int32__$impl_Numeric(Intrinsic(Int32))"
  , "from_int64__$impl_Numeric(Intrinsic(Int32))"
  , "(+)__$impl_Numeric(Intrinsic(Int32))"
  , "(-)__$impl_Numeric(Intrinsic(Int32))"
  , "(*)__$impl_Numeric(Intrinsic(Int32))"
  , "negate__$impl_Numeric(Intrinsic(Int32))"
  , --
    "from_bignum__$impl_Numeric(Intrinsic(Int64))"
  , "from_int32__$impl_Numeric(Intrinsic(Int64))"
  , "from_int64__$impl_Numeric(Intrinsic(Int64))"
  , "(+)__$impl_Numeric(Intrinsic(Int64))"
  , "(-)__$impl_Numeric(Intrinsic(Int64))"
  , "(*)__$impl_Numeric(Intrinsic(Int64))"
  , "negate__$impl_Numeric(Intrinsic(Int64))"
  , --
    "from_bignum__$impl_Numeric(Intrinsic(Float))"
  , "from_int32__$impl_Numeric(Intrinsic(Float))"
  , "from_int64__$impl_Numeric(Intrinsic(Float))"
  , "(+)__$impl_Numeric(Intrinsic(Float))"
  , "(-)__$impl_Numeric(Intrinsic(Float))"
  , "(*)__$impl_Numeric(Intrinsic(Float))"
  , "negate__$impl_Numeric(Intrinsic(Float))"
  , --
    "from_bignum__$impl_Numeric(Intrinsic(Double))"
  , "from_int32__$impl_Numeric(Intrinsic(Double))"
  , "from_int64__$impl_Numeric(Intrinsic(Double))"
  , "(+)__$impl_Numeric(Intrinsic(Double))"
  , "(-)__$impl_Numeric(Intrinsic(Double))"
  , "(*)__$impl_Numeric(Intrinsic(Double))"
  , "negate__$impl_Numeric(Intrinsic(Double))"
  , --
    "from_bignum__$impl_Numeric(Intrinsic(Nat))"
  , "from_int32__$impl_Numeric(Intrinsic(Nat))"
  , "from_int64__$impl_Numeric(Intrinsic(Nat))"
  , "(+)__$impl_Numeric(Intrinsic(Nat))"
  , "(-)__$impl_Numeric(Intrinsic(Nat))"
  , "(*)__$impl_Numeric(Intrinsic(Nat))"
  , "negate__$impl_Numeric(Intrinsic(Nat))"
  , --
    "from_bignum__$impl_Numeric(Intrinsic(Bignum))"
  , "from_int32__$impl_Numeric(Intrinsic(Bignum))"
  , "from_int64__$impl_Numeric(Intrinsic(Bignum))"
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
  , "compare__$impl_Ordered(Intrinsic(String))"
  , "compare__$impl_Ordered(Application(Application(Constructor(#Tuple2))(Variable(Parameter(a))))(Variable(Parameter(b))))"
  , --
    "(==)__$impl_Comparable(Intrinsic(Int32))"
  , "(==)__$impl_Comparable(Intrinsic(Int64))"
  , "(==)__$impl_Comparable(Intrinsic(Nat))"
  , "(==)__$impl_Comparable(Intrinsic(Float))"
  , "(==)__$impl_Comparable(Intrinsic(Double))"
  , "(==)__$impl_Comparable(Intrinsic(Bool))"
  , "(==)__$impl_Comparable(Intrinsic(Char))"
  , "(==)__$impl_Comparable(Intrinsic(Bignum))"
  , "(==)__$impl_Comparable(Intrinsic(String))"
  , "(==)__$impl_Comparable(Application(Application(Constructor(#Tuple2))(Variable(Parameter(a))))(Variable(Parameter(b))))"
  , --
    "(/)__$impl_Divisible(Intrinsic(Float))"
  , "(/)__$impl_Divisible(Intrinsic(Double))"
  , --
    "(%)__$impl_Modulo(Intrinsic(Int32))"
  , "(%)__$impl_Modulo(Intrinsic(Int64))"
  , "(%)__$impl_Modulo(Intrinsic(Bignum))"
  , --
    "(<>)__$impl_Semigroup(Intrinsic(String))"
  , "(<>)__$impl_Semigroup(Application(Constructor(List))(Variable(Parameter(a))))"
  ]

-- Needed to support do-notation
extraDefinitions :: (Monoid a) => [Definition a k ()]
extraDefinitions =
  [ DImport mempty (Path ["Coal", "Monad"]) [ImportTrait mempty "Monad" ["bind"]]
  , DImport mempty (Path ["Coal", "Applicative"]) [ImportTrait mempty "Applicative" ["pure"]]
  ]

builtinDefinitions :: (Monoid a) => [Definition a k ()]
builtinDefinitions =
  [ DImport
      mempty
      (Path ["Builtin$"])
      (for (builtinFunctionNames <> builtinTraitInstances) (ImportName mempty))
  , DType
      mempty
      "Ordering"
      ( TypeDefinition
          []
          [ DataConstructor "LessThan" 0 (Forall mempty [] (TConstructor () "Ordering"))
          , DataConstructor "GreaterThan" 0 (Forall mempty [] (TConstructor () "Ordering"))
          , DataConstructor "EqualTo" 0 (Forall mempty [] (TConstructor () "Ordering"))
          ]
      )
  , DType
      mempty
      "Option"
      ( TypeDefinition
          [Parameter () "a"]
          [ DataConstructor "Some" 1 (Forall (Set.fromList [Parameter () "a"]) [] (TVariable (Parameter () "a") `TArrow` applyTypeArgs () (TConstructor () "Option") (TVariable (Parameter () "a") :| [])))
          , DataConstructor "None" 0 (Forall (Set.fromList [Parameter () "a"]) [] (applyTypeArgs () (TConstructor () "Option") (TVariable (Parameter () "a") :| [])))
          ]
      )
  , DType
      mempty
      "Result"
      ( TypeDefinition
          [Parameter () "a", Parameter () "b"]
          [ DataConstructor "Ok" 1 (Forall (Set.fromList [Parameter () "a"]) [] (TVariable (Parameter () "a") `TArrow` applyTypeArgs () (TConstructor () "Result") (TVariable (Parameter () "a") :| [TVariable (Parameter () "b")])))
          , DataConstructor "Err" 1 (Forall (Set.fromList [Parameter () "b"]) [] (TVariable (Parameter () "b") `TArrow` applyTypeArgs () (TConstructor () "Result") (TVariable (Parameter () "a") :| [TVariable (Parameter () "b")])))
          ]
      )
  , DType
      mempty
      "IO"
      (TypeDefinition [Parameter () "a"] [])
  ]
    <> builtinTraits
