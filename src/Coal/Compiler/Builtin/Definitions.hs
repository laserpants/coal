{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Builtin.Definitions (
  module Coal.Compiler.Builtin.Functions,
  insertBuiltinDefinitions,
  builtinTraitInstances,
) where

import Coal.Compiler.Builtin.Functions (builtinFunctions)
import Coal.Language
import Coal.Language.Module
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set
import Extras (Name, for)

{-# INLINE insertBuiltinDefinitions #-}
insertBuiltinDefinitions :: (Monoid a) => [Definition a k ()] -> [Definition a k ()]
insertBuiltinDefinitions = (builtinDefinitions <>)

{-# INLINE builtinFunctionNames #-}
builtinFunctionNames :: [Name]
builtinFunctionNames = for builtinFunctions fst

builtinTraitInstances :: [Name]
builtinTraitInstances =
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
  , "compare__$impl_Ordered(Intrinsic(String))"
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

builtinDefinitions :: (Monoid a) => [Definition a k ()]
builtinDefinitions =
  [ DImport
      mempty
      (Path ["Builtin$"])
      (for (builtinFunctionNames <> builtinTraitInstances) (ImportName mempty))
  , DTrait
      mempty
      "Numeric"
      ( TraitDef
          []
          (Parameter KType "a")
          [
            ( "from_int32"
            , Forall (Set.fromList [Parameter () "a"]) [] $ TIntrinsic IInt32 `TArrow` TVariable (Parameter () "a")
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
      ( TraitDef
          []
          (Parameter KType "a")
          [
            ( "compare"
            , Forall (Set.fromList [Parameter () "a"]) [] $ TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TConstructor () "Ordering"
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
            , Forall (Set.fromList [Parameter () "a"]) [] $ TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TIntrinsic IBool
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
            , Forall (Set.fromList [Parameter () "a"]) [] $ TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a")
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
            , Forall (Set.fromList [Parameter () "a"]) [] $ TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a")
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
            , Forall (Set.fromList [Parameter () "a"]) [] $ TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a")
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
          [ DataConstructor "Some" 1 (Forall (Set.fromList [Parameter () "a"]) [] (TVariable (Parameter () "a") `TArrow` applyTypeArgs () (TConstructor () "Option") (TVariable (Parameter () "a") :| [])))
          , DataConstructor "None" 0 (Forall (Set.fromList [Parameter () "a"]) [] (applyTypeArgs () (TConstructor () "Option") (TVariable (Parameter () "a") :| [])))
          ]
      )
  , DType
      mempty
      "IO"
      (TypeDef [Parameter () "a"] [])
  ]
