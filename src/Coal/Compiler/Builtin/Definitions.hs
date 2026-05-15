{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Builtin.Definitions (
  module Coal.Compiler.Builtin.Functions,
  insertBuiltinDefinitions,
  insertExtraDefinitions,
  builtinTraitInstances,
) where

import Coal.Compiler.Builtin.Functions (builtinFunctions)
import Coal.Compiler.Builtin.Traits (builtinTraits)
import qualified Coal.Compiler.Builtin.Traits as Trait
import Coal.Language
import Coal.Language.Module.Import (Import (..))
import Coal.Language.Module.Path (Path (Path))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set
import Extras (Name, for)

{-# INLINE insertBuiltinDefinitions #-}
insertBuiltinDefinitions :: (Monoid a) => [Definition a () ()] -> [Definition a () ()]
insertBuiltinDefinitions = (builtinDefinitions <>)

{-# INLINE insertExtraDefinitions #-}
insertExtraDefinitions :: (Monoid a) => [Definition a () ()] -> [Definition a () ()]
insertExtraDefinitions = (extraDefinitions <>)

builtinFunctionNames :: [Name]
builtinFunctionNames = for builtinFunctions fst

builtinTraitInstances :: [Name]
builtinTraitInstances =
  [ -- Numeric
    instanceLabel (Trait.numeric (TIntrinsic IInt32)) "from_bignum"
  , instanceLabel (Trait.numeric (TIntrinsic IInt32)) "from_int32"
  , instanceLabel (Trait.numeric (TIntrinsic IInt32)) "from_int64"
  , instanceLabel (Trait.numeric (TIntrinsic IInt32)) "(+)"
  , instanceLabel (Trait.numeric (TIntrinsic IInt32)) "(-)"
  , instanceLabel (Trait.numeric (TIntrinsic IInt32)) "(*)"
  , instanceLabel (Trait.numeric (TIntrinsic IInt32)) "negate"
  , --
    instanceLabel (Trait.numeric (TIntrinsic IInt64)) "from_bignum"
  , instanceLabel (Trait.numeric (TIntrinsic IInt64)) "from_int32"
  , instanceLabel (Trait.numeric (TIntrinsic IInt64)) "from_int64"
  , instanceLabel (Trait.numeric (TIntrinsic IInt64)) "(+)"
  , instanceLabel (Trait.numeric (TIntrinsic IInt64)) "(-)"
  , instanceLabel (Trait.numeric (TIntrinsic IInt64)) "(*)"
  , instanceLabel (Trait.numeric (TIntrinsic IInt64)) "negate"
  , --
    instanceLabel (Trait.numeric (TIntrinsic IFloat)) "from_bignum"
  , instanceLabel (Trait.numeric (TIntrinsic IFloat)) "from_int32"
  , instanceLabel (Trait.numeric (TIntrinsic IFloat)) "from_int64"
  , instanceLabel (Trait.numeric (TIntrinsic IFloat)) "(+)"
  , instanceLabel (Trait.numeric (TIntrinsic IFloat)) "(-)"
  , instanceLabel (Trait.numeric (TIntrinsic IFloat)) "(*)"
  , instanceLabel (Trait.numeric (TIntrinsic IFloat)) "negate"
  , --
    instanceLabel (Trait.numeric (TIntrinsic IDouble)) "from_bignum"
  , instanceLabel (Trait.numeric (TIntrinsic IDouble)) "from_int32"
  , instanceLabel (Trait.numeric (TIntrinsic IDouble)) "from_int64"
  , instanceLabel (Trait.numeric (TIntrinsic IDouble)) "(+)"
  , instanceLabel (Trait.numeric (TIntrinsic IDouble)) "(-)"
  , instanceLabel (Trait.numeric (TIntrinsic IDouble)) "(*)"
  , instanceLabel (Trait.numeric (TIntrinsic IDouble)) "negate"
  , --
    instanceLabel (Trait.numeric (TIntrinsic INat)) "from_bignum"
  , instanceLabel (Trait.numeric (TIntrinsic INat)) "from_int32"
  , instanceLabel (Trait.numeric (TIntrinsic INat)) "from_int64"
  , instanceLabel (Trait.numeric (TIntrinsic INat)) "(+)"
  , instanceLabel (Trait.numeric (TIntrinsic INat)) "(-)"
  , instanceLabel (Trait.numeric (TIntrinsic INat)) "(*)"
  , instanceLabel (Trait.numeric (TIntrinsic INat)) "negate"
  , --
    instanceLabel (Trait.numeric (TIntrinsic IBignum)) "from_bignum"
  , instanceLabel (Trait.numeric (TIntrinsic IBignum)) "from_int32"
  , instanceLabel (Trait.numeric (TIntrinsic IBignum)) "from_int64"
  , instanceLabel (Trait.numeric (TIntrinsic IBignum)) "(+)"
  , instanceLabel (Trait.numeric (TIntrinsic IBignum)) "(-)"
  , instanceLabel (Trait.numeric (TIntrinsic IBignum)) "(*)"
  , instanceLabel (Trait.numeric (TIntrinsic IBignum)) "negate"
  , -- Ordered
    instanceLabel (Trait.ordered (TIntrinsic IInt32)) "compare"
  , instanceLabel (Trait.ordered (TIntrinsic IInt64)) "compare"
  , instanceLabel (Trait.ordered (TIntrinsic INat)) "compare"
  , instanceLabel (Trait.ordered (TIntrinsic IFloat)) "compare"
  , instanceLabel (Trait.ordered (TIntrinsic IDouble)) "compare"
  , instanceLabel (Trait.ordered (TIntrinsic IBool)) "compare"
  , instanceLabel (Trait.ordered (TIntrinsic IChar)) "compare"
  , instanceLabel (Trait.ordered (TIntrinsic IBignum)) "compare"
  , instanceLabel (Trait.ordered (TIntrinsic IString)) "compare"
  , instanceLabel (Trait.ordered (TApplication () (TApplication () (TConstructor () "#Tuple2") (TVariable (Parameter () "a"))) (TVariable (Parameter () "b")))) "compare"
  , -- Comparable
    instanceLabel (Trait.comparable (TIntrinsic IInt32)) "(==)"
  , instanceLabel (Trait.comparable (TIntrinsic IInt64)) "(==)"
  , instanceLabel (Trait.comparable (TIntrinsic INat)) "(==)"
  , instanceLabel (Trait.comparable (TIntrinsic IFloat)) "(==)"
  , instanceLabel (Trait.comparable (TIntrinsic IDouble)) "(==)"
  , instanceLabel (Trait.comparable (TIntrinsic IBool)) "(==)"
  , instanceLabel (Trait.comparable (TIntrinsic IChar)) "(==)"
  , instanceLabel (Trait.comparable (TIntrinsic IBignum)) "(==)"
  , instanceLabel (Trait.comparable (TIntrinsic IString)) "(==)"
  , instanceLabel (Trait.comparable (TApplication () (TApplication () (TConstructor () "#Tuple2") (TVariable (Parameter () "a"))) (TVariable (Parameter () "b")))) "(==)"
  , -- Divisible
    instanceLabel (Trait.divisible (TIntrinsic IFloat)) "(/)"
  , instanceLabel (Trait.divisible (TIntrinsic IDouble)) "(/)"
  , -- Modulo
    instanceLabel (Trait.modulo (TIntrinsic IInt32)) "(%)"
  , instanceLabel (Trait.modulo (TIntrinsic IInt64)) "(%)"
  , instanceLabel (Trait.modulo (TIntrinsic IBignum)) "(%)"
  , -- Semigroup
    instanceLabel (Trait.semigroup (TIntrinsic IString)) "(<>)"
  , instanceLabel (Trait.semigroup (TApplication () (TConstructor () "List") (TVariable (Parameter () "a")))) "(<>)"
  ]

-- Needed to support do-notation
extraDefinitions :: (Monoid a) => [Definition a () ()]
extraDefinitions =
  [ DImport mempty (Path ["Coal", "Monad"]) [TypeImport mempty "Monad" ["bind"]]
  , DImport mempty (Path ["Coal", "Applicative"]) [TypeImport mempty "Applicative" ["pure"]]
  ]

builtinDefinitions :: (Monoid a) => [Definition a () ()]
builtinDefinitions =
  [ DImport
      mempty
      (Path ["Builtin$"])
      (for (builtinFunctionNames <> builtinTraitInstances) (NameImport mempty))
  , DType
      mempty
      "Ordering"
      ( TypeDefinition
          []
          [ DataConstructor "LessThan" 0 (Forall mempty mempty (TConstructor () "Ordering"))
          , DataConstructor "GreaterThan" 0 (Forall mempty mempty (TConstructor () "Ordering"))
          , DataConstructor "EqualTo" 0 (Forall mempty mempty (TConstructor () "Ordering"))
          ]
      )
  , DType
      mempty
      "Option"
      ( TypeDefinition
          [Parameter () "a"]
          [ DataConstructor "Some" 1 (Forall (Set.fromList [Parameter () "a"]) mempty (TVariable (Parameter () "a") `TArrow` applyTypeArgs () (TConstructor () "Option") (TVariable (Parameter () "a") :| mempty)))
          , DataConstructor "None" 0 (Forall (Set.fromList [Parameter () "a"]) mempty (applyTypeArgs () (TConstructor () "Option") (TVariable (Parameter () "a") :| mempty)))
          ]
      )
  , DType
      mempty
      "Result"
      ( TypeDefinition
          [Parameter () "a", Parameter () "b"]
          [ DataConstructor "Ok" 1 (Forall (Set.fromList [Parameter () "a"]) mempty (TVariable (Parameter () "a") `TArrow` applyTypeArgs () (TConstructor () "Result") (TVariable (Parameter () "a") :| [TVariable (Parameter () "b")])))
          , DataConstructor "Err" 1 (Forall (Set.fromList [Parameter () "b"]) mempty (TVariable (Parameter () "b") `TArrow` applyTypeArgs () (TConstructor () "Result") (TVariable (Parameter () "a") :| [TVariable (Parameter () "b")])))
          ]
      )
  , DType
      mempty
      "IO"
      (TypeDefinition [Parameter () "a"] [])
  , DType
      mempty
      "Machine"
      ( TypeDefinition
          [Parameter () "s", Parameter () "i", Parameter () "o"]
          [ DataConstructor
              "Machine"
              1
              ( Forall
                  (Set.fromList [Parameter () "s", Parameter () "i", Parameter () "o"])
                  mempty
                  ( TRecord
                      ( TRow
                          ( RExtend
                              "state"
                              (TVariable (Parameter () "s"))
                              ( RExtend
                                  "step"
                                  ( TVariable (Parameter () "i")
                                      `TArrow` TVariable (Parameter () "s")
                                      `TArrow` applyTypeArgs
                                        ()
                                        (TConstructor () "Machine")
                                        ( TVariable (Parameter () "s")
                                            :| [ TVariable (Parameter () "i")
                                               , TVariable (Parameter () "o")
                                               ]
                                        )
                                  )
                                  ( RExtend
                                      "view"
                                      (TVariable (Parameter () "s") `TArrow` TVariable (Parameter () "o"))
                                      RNil
                                  )
                              )
                          )
                      )
                      `TArrow` applyTypeArgs
                        ()
                        (TConstructor () "Machine")
                        ( TVariable (Parameter () "s")
                            :| [ TVariable (Parameter () "i")
                               , TVariable (Parameter () "o")
                               ]
                        )
                  )
              )
          ]
      )
  ]
    <> builtinTraits
