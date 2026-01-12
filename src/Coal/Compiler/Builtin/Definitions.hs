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

numericTrait :: ParameterizedType -> Trait ParameterizedType
numericTrait = Trait "Numeric"

orderedTrait :: ParameterizedType -> Trait ParameterizedType
orderedTrait = Trait "Ordered"

comparableTrait :: ParameterizedType -> Trait ParameterizedType
comparableTrait = Trait "Comparable"

divisibleTrait :: ParameterizedType -> Trait ParameterizedType
divisibleTrait = Trait "Divisible"

moduloTrait :: ParameterizedType -> Trait ParameterizedType
moduloTrait = Trait "Modulo"

semigroupTrait :: ParameterizedType -> Trait ParameterizedType
semigroupTrait = Trait "Semigroup"

builtinTraitInstances :: [Name]
builtinTraitInstances =
  [ -- Numeric
    instanceLabel (numericTrait (TIntrinsic IInt32)) "from_bignum"
  , instanceLabel (numericTrait (TIntrinsic IInt32)) "from_int32"
  , instanceLabel (numericTrait (TIntrinsic IInt32)) "from_int64"
  , instanceLabel (numericTrait (TIntrinsic IInt32)) "(+)"
  , instanceLabel (numericTrait (TIntrinsic IInt32)) "(-)"
  , instanceLabel (numericTrait (TIntrinsic IInt32)) "(*)"
  , instanceLabel (numericTrait (TIntrinsic IInt32)) "negate"
  , --
    instanceLabel (numericTrait (TIntrinsic IInt64)) "from_bignum"
  , instanceLabel (numericTrait (TIntrinsic IInt64)) "from_int32"
  , instanceLabel (numericTrait (TIntrinsic IInt64)) "from_int64"
  , instanceLabel (numericTrait (TIntrinsic IInt64)) "(+)"
  , instanceLabel (numericTrait (TIntrinsic IInt64)) "(-)"
  , instanceLabel (numericTrait (TIntrinsic IInt64)) "(*)"
  , instanceLabel (numericTrait (TIntrinsic IInt64)) "negate"
  , --
    instanceLabel (numericTrait (TIntrinsic IFloat)) "from_bignum"
  , instanceLabel (numericTrait (TIntrinsic IFloat)) "from_int32"
  , instanceLabel (numericTrait (TIntrinsic IFloat)) "from_int64"
  , instanceLabel (numericTrait (TIntrinsic IFloat)) "(+)"
  , instanceLabel (numericTrait (TIntrinsic IFloat)) "(-)"
  , instanceLabel (numericTrait (TIntrinsic IFloat)) "(*)"
  , instanceLabel (numericTrait (TIntrinsic IFloat)) "negate"
  , --
    instanceLabel (numericTrait (TIntrinsic IDouble)) "from_bignum"
  , instanceLabel (numericTrait (TIntrinsic IDouble)) "from_int32"
  , instanceLabel (numericTrait (TIntrinsic IDouble)) "from_int64"
  , instanceLabel (numericTrait (TIntrinsic IDouble)) "(+)"
  , instanceLabel (numericTrait (TIntrinsic IDouble)) "(-)"
  , instanceLabel (numericTrait (TIntrinsic IDouble)) "(*)"
  , instanceLabel (numericTrait (TIntrinsic IDouble)) "negate"
  , --
    instanceLabel (numericTrait (TIntrinsic INat)) "from_bignum"
  , instanceLabel (numericTrait (TIntrinsic INat)) "from_int32"
  , instanceLabel (numericTrait (TIntrinsic INat)) "from_int64"
  , instanceLabel (numericTrait (TIntrinsic INat)) "(+)"
  , instanceLabel (numericTrait (TIntrinsic INat)) "(-)"
  , instanceLabel (numericTrait (TIntrinsic INat)) "(*)"
  , instanceLabel (numericTrait (TIntrinsic INat)) "negate"
  , --
    instanceLabel (numericTrait (TIntrinsic IBignum)) "from_bignum"
  , instanceLabel (numericTrait (TIntrinsic IBignum)) "from_int32"
  , instanceLabel (numericTrait (TIntrinsic IBignum)) "from_int64"
  , instanceLabel (numericTrait (TIntrinsic IBignum)) "(+)"
  , instanceLabel (numericTrait (TIntrinsic IBignum)) "(-)"
  , instanceLabel (numericTrait (TIntrinsic IBignum)) "(*)"
  , instanceLabel (numericTrait (TIntrinsic IBignum)) "negate"
  , -- Ordered
    instanceLabel (orderedTrait (TIntrinsic IInt32)) "compare"
  , instanceLabel (orderedTrait (TIntrinsic IInt64)) "compare"
  , instanceLabel (orderedTrait (TIntrinsic INat)) "compare"
  , instanceLabel (orderedTrait (TIntrinsic IFloat)) "compare"
  , instanceLabel (orderedTrait (TIntrinsic IDouble)) "compare"
  , instanceLabel (orderedTrait (TIntrinsic IBool)) "compare"
  , instanceLabel (orderedTrait (TIntrinsic IChar)) "compare"
  , instanceLabel (orderedTrait (TIntrinsic IBignum)) "compare"
  , instanceLabel (orderedTrait (TIntrinsic IString)) "compare"
  , instanceLabel (orderedTrait (TApplication () (TApplication () (TConstructor () "#Tuple2") (TVariable (Parameter () "a"))) (TVariable (Parameter () "b")))) "compare"
  , -- Comparable
    instanceLabel (comparableTrait (TIntrinsic IInt32)) "(==)"
  , instanceLabel (comparableTrait (TIntrinsic IInt64)) "(==)"
  , instanceLabel (comparableTrait (TIntrinsic INat)) "(==)"
  , instanceLabel (comparableTrait (TIntrinsic IFloat)) "(==)"
  , instanceLabel (comparableTrait (TIntrinsic IDouble)) "(==)"
  , instanceLabel (comparableTrait (TIntrinsic IBool)) "(==)"
  , instanceLabel (comparableTrait (TIntrinsic IChar)) "(==)"
  , instanceLabel (comparableTrait (TIntrinsic IBignum)) "(==)"
  , instanceLabel (comparableTrait (TIntrinsic IString)) "(==)"
  , instanceLabel (comparableTrait (TApplication () (TApplication () (TConstructor () "#Tuple2") (TVariable (Parameter () "a"))) (TVariable (Parameter () "b")))) "(==)"
  , -- Divisible
    instanceLabel (divisibleTrait (TIntrinsic IFloat)) "(/)"
  , instanceLabel (divisibleTrait (TIntrinsic IDouble)) "(/)"
  , -- Modulo
    instanceLabel (moduloTrait (TIntrinsic IInt32)) "(%)"
  , instanceLabel (moduloTrait (TIntrinsic IInt64)) "(%)"
  , instanceLabel (moduloTrait (TIntrinsic IBignum)) "(%)"
  , -- Semigroup
    instanceLabel (semigroupTrait (TIntrinsic IString)) "(<>)"
  , instanceLabel (semigroupTrait (TApplication () (TConstructor () "List") (TVariable (Parameter () "a")))) "(<>)"
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
