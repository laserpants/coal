{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Builtin.Instances (builtinInstances) where

import Coal.Compiler.Build (InstanceEntry (InstanceEntry))
import Coal.Language.Type (IndexedType, Parameter (Parameter), Type (..), TypeIndex (TypeIndex), applyTypeArgs)
import Coal.Language.Type.Intrinsic (Intrinsic (..))
import Coal.Language.Type.Kind (Kind (KArrow, KType))
import Coal.Language.Type.Scheme (Scheme (Forall))
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Map.Strict as Map
import Extras (Name)

builtinInstances :: (Monoid a) => [(Name, IndexedType, InstanceEntry a)]
builtinInstances =
  [
    ( "Numeric"
    , TIntrinsic IInt32
    , InstanceEntry
        mempty
        (TIntrinsic IInt32)
        (TIntrinsic IInt32)
        ( Map.fromList
            [
              ( "from_int32"
              , Forall mempty [] (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32)
              )
            ,
              ( "negate"
              , Forall mempty [] (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32)
              )
            ,
              ( "(+)"
              , Forall mempty [] (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IInt32)
              )
            ,
              ( "(-)"
              , Forall mempty [] (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IInt32)
              )
            ,
              ( "(*)"
              , Forall mempty [] (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IInt32)
              )
            ]
        )
    )
  ,
    ( "Numeric"
    , TIntrinsic IInt64
    , InstanceEntry
        mempty
        (TIntrinsic IInt64)
        (TIntrinsic IInt64)
        ( Map.fromList
            [
              ( "from_int32"
              , Forall mempty [] (TIntrinsic IInt32 `TArrow` TIntrinsic IInt64)
              )
            ,
              ( "negate"
              , Forall mempty [] (TIntrinsic IInt64 `TArrow` TIntrinsic IInt64)
              )
            ,
              ( "(+)"
              , Forall mempty [] (TIntrinsic IInt64 `TArrow` TIntrinsic IInt64 `TArrow` TIntrinsic IInt64)
              )
            ,
              ( "(-)"
              , Forall mempty [] (TIntrinsic IInt64 `TArrow` TIntrinsic IInt64 `TArrow` TIntrinsic IInt64)
              )
            ,
              ( "(*)"
              , Forall mempty [] (TIntrinsic IInt64 `TArrow` TIntrinsic IInt64 `TArrow` TIntrinsic IInt64)
              )
            ]
        )
    )
  ,
    ( "Numeric"
    , TIntrinsic IFloat
    , InstanceEntry
        mempty
        (TIntrinsic IFloat)
        (TIntrinsic IFloat)
        ( Map.fromList
            [
              ( "from_int32"
              , Forall mempty [] (TIntrinsic IInt32 `TArrow` TIntrinsic IFloat)
              )
            ,
              ( "negate"
              , Forall mempty [] (TIntrinsic IFloat `TArrow` TIntrinsic IFloat)
              )
            ,
              ( "(+)"
              , Forall mempty [] (TIntrinsic IFloat `TArrow` TIntrinsic IFloat `TArrow` TIntrinsic IFloat)
              )
            ,
              ( "(-)"
              , Forall mempty [] (TIntrinsic IFloat `TArrow` TIntrinsic IFloat `TArrow` TIntrinsic IFloat)
              )
            ,
              ( "(*)"
              , Forall mempty [] (TIntrinsic IFloat `TArrow` TIntrinsic IFloat `TArrow` TIntrinsic IFloat)
              )
            ]
        )
    )
  ,
    ( "Numeric"
    , TIntrinsic IDouble
    , InstanceEntry
        mempty
        (TIntrinsic IDouble)
        (TIntrinsic IDouble)
        ( Map.fromList
            [
              ( "from_int32"
              , Forall mempty [] (TIntrinsic IInt32 `TArrow` TIntrinsic IDouble)
              )
            ,
              ( "negate"
              , Forall mempty [] (TIntrinsic IDouble `TArrow` TIntrinsic IDouble)
              )
            ,
              ( "(+)"
              , Forall mempty [] (TIntrinsic IDouble `TArrow` TIntrinsic IDouble `TArrow` TIntrinsic IDouble)
              )
            ,
              ( "(-)"
              , Forall mempty [] (TIntrinsic IDouble `TArrow` TIntrinsic IDouble `TArrow` TIntrinsic IDouble)
              )
            ,
              ( "(*)"
              , Forall mempty [] (TIntrinsic IDouble `TArrow` TIntrinsic IDouble `TArrow` TIntrinsic IDouble)
              )
            ]
        )
    )
  ,
    ( "Numeric"
    , TIntrinsic INat
    , InstanceEntry
        mempty
        (TIntrinsic INat)
        (TIntrinsic INat)
        ( Map.fromList
            [
              ( "from_int32"
              , Forall mempty [] (TIntrinsic IInt32 `TArrow` TIntrinsic INat)
              )
            ,
              ( "negate"
              , Forall mempty [] (TIntrinsic INat `TArrow` TIntrinsic INat)
              )
            ,
              ( "(+)"
              , Forall mempty [] (TIntrinsic INat `TArrow` TIntrinsic INat `TArrow` TIntrinsic INat)
              )
            ,
              ( "(-)"
              , Forall mempty [] (TIntrinsic INat `TArrow` TIntrinsic INat `TArrow` TIntrinsic INat)
              )
            ,
              ( "(*)"
              , Forall mempty [] (TIntrinsic INat `TArrow` TIntrinsic INat `TArrow` TIntrinsic INat)
              )
            ]
        )
    )
  ,
    ( "Numeric"
    , TIntrinsic IBignum
    , InstanceEntry
        mempty
        (TIntrinsic IBignum)
        (TIntrinsic IBignum)
        ( Map.fromList
            [
              ( "from_int32"
              , Forall mempty [] (TIntrinsic IInt32 `TArrow` TIntrinsic IBignum)
              )
            ,
              ( "negate"
              , Forall mempty [] (TIntrinsic IBignum `TArrow` TIntrinsic IBignum)
              )
            ,
              ( "(+)"
              , Forall mempty [] (TIntrinsic IBignum `TArrow` TIntrinsic IBignum `TArrow` TIntrinsic IBignum)
              )
            ,
              ( "(-)"
              , Forall mempty [] (TIntrinsic IBignum `TArrow` TIntrinsic IBignum `TArrow` TIntrinsic IBignum)
              )
            ,
              ( "(*)"
              , Forall mempty [] (TIntrinsic IBignum `TArrow` TIntrinsic IBignum `TArrow` TIntrinsic IBignum)
              )
            ]
        )
    )
  ,
    ( "Ordered"
    , TIntrinsic IInt32
    , InstanceEntry
        mempty
        (TIntrinsic IInt32)
        (TIntrinsic IInt32)
        ( Map.fromList
            [
              ( "compare"
              , Forall mempty [] (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TConstructor KType "Ordering")
              )
            ]
        )
    )
  ,
    ( "Ordered"
    , TIntrinsic IInt64
    , InstanceEntry
        mempty
        (TIntrinsic IInt64)
        (TIntrinsic IInt64)
        ( Map.fromList
            [
              ( "compare"
              , Forall mempty [] (TIntrinsic IInt64 `TArrow` TIntrinsic IInt64 `TArrow` TConstructor KType "Ordering")
              )
            ]
        )
    )
  ,
    ( "Ordered"
    , TIntrinsic IBool
    , InstanceEntry
        mempty
        (TIntrinsic IBool)
        (TIntrinsic IBool)
        ( Map.fromList
            [
              ( "compare"
              , Forall mempty [] (TIntrinsic IBool `TArrow` TIntrinsic IBool `TArrow` TConstructor KType "Ordering")
              )
            ]
        )
    )
  ,
    ( "Ordered"
    , TIntrinsic INat
    , InstanceEntry
        mempty
        (TIntrinsic INat)
        (TIntrinsic INat)
        ( Map.fromList
            [
              ( "compare"
              , Forall mempty [] (TIntrinsic INat `TArrow` TIntrinsic INat `TArrow` TConstructor KType "Ordering")
              )
            ]
        )
    )
  ,
    ( "Ordered"
    , TIntrinsic IFloat
    , InstanceEntry
        mempty
        (TIntrinsic IFloat)
        (TIntrinsic IFloat)
        ( Map.fromList
            [
              ( "compare"
              , Forall mempty [] (TIntrinsic IFloat `TArrow` TIntrinsic IFloat `TArrow` TConstructor KType "Ordering")
              )
            ]
        )
    )
  ,
    ( "Ordered"
    , TIntrinsic IDouble
    , InstanceEntry
        mempty
        (TIntrinsic IDouble)
        (TIntrinsic IDouble)
        ( Map.fromList
            [
              ( "compare"
              , Forall mempty [] (TIntrinsic IDouble `TArrow` TIntrinsic IDouble `TArrow` TConstructor KType "Ordering")
              )
            ]
        )
    )
  ,
    ( "Ordered"
    , TIntrinsic IChar
    , InstanceEntry
        mempty
        (TIntrinsic IChar)
        (TIntrinsic IChar)
        ( Map.fromList
            [
              ( "compare"
              , Forall mempty [] (TIntrinsic IChar `TArrow` TIntrinsic IChar `TArrow` TConstructor KType "Ordering")
              )
            ]
        )
    )
  ,
    ( "Ordered"
    , TIntrinsic IBignum
    , InstanceEntry
        mempty
        (TIntrinsic IBignum)
        (TIntrinsic IBignum)
        ( Map.fromList
            [
              ( "compare"
              , Forall mempty [] (TIntrinsic IBignum `TArrow` TIntrinsic IBignum `TArrow` TConstructor KType "Ordering")
              )
            ]
        )
    )
  ,
    ( "Comparable"
    , TIntrinsic IInt32
    , InstanceEntry
        mempty
        (TIntrinsic IInt32)
        (TIntrinsic IInt32)
        ( Map.fromList
            [
              ( "(==)"
              , Forall mempty [] (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IBool)
              )
            ]
        )
    )
  ,
    ( "Comparable"
    , TIntrinsic IInt64
    , InstanceEntry
        mempty
        (TIntrinsic IInt64)
        (TIntrinsic IInt64)
        ( Map.fromList
            [
              ( "(==)"
              , Forall mempty [] (TIntrinsic IInt64 `TArrow` TIntrinsic IInt64 `TArrow` TIntrinsic IBool)
              )
            ]
        )
    )
  ,
    ( "Comparable"
    , TIntrinsic IBool
    , InstanceEntry
        mempty
        (TIntrinsic IBool)
        (TIntrinsic IBool)
        ( Map.fromList
            [
              ( "(==)"
              , Forall mempty [] (TIntrinsic IBool `TArrow` TIntrinsic IBool `TArrow` TIntrinsic IBool)
              )
            ]
        )
    )
  ,
    ( "Comparable"
    , TIntrinsic INat
    , InstanceEntry
        mempty
        (TIntrinsic INat)
        (TIntrinsic INat)
        ( Map.fromList
            [
              ( "(==)"
              , Forall mempty [] (TIntrinsic INat `TArrow` TIntrinsic INat `TArrow` TIntrinsic IBool)
              )
            ]
        )
    )
  ,
    ( "Comparable"
    , TIntrinsic IFloat
    , InstanceEntry
        mempty
        (TIntrinsic IFloat)
        (TIntrinsic IFloat)
        ( Map.fromList
            [
              ( "(==)"
              , Forall mempty [] (TIntrinsic IFloat `TArrow` TIntrinsic IFloat `TArrow` TIntrinsic IBool)
              )
            ]
        )
    )
  ,
    ( "Comparable"
    , TIntrinsic IDouble
    , InstanceEntry
        mempty
        (TIntrinsic IDouble)
        (TIntrinsic IDouble)
        ( Map.fromList
            [
              ( "(==)"
              , Forall mempty [] (TIntrinsic IDouble `TArrow` TIntrinsic IDouble `TArrow` TIntrinsic IBool)
              )
            ]
        )
    )
  ,
    ( "Comparable"
    , TIntrinsic IChar
    , InstanceEntry
        mempty
        (TIntrinsic IChar)
        (TIntrinsic IChar)
        ( Map.fromList
            [
              ( "(==)"
              , Forall mempty [] (TIntrinsic IChar `TArrow` TIntrinsic IChar `TArrow` TIntrinsic IBool)
              )
            ]
        )
    )
  ,
    ( "Comparable"
    , TIntrinsic IBignum
    , InstanceEntry
        mempty
        (TIntrinsic IBignum)
        (TIntrinsic IBignum)
        ( Map.fromList
            [
              ( "(==)"
              , Forall mempty [] (TIntrinsic IBignum `TArrow` TIntrinsic IBignum `TArrow` TIntrinsic IBool)
              )
            ]
        )
    )
  ,
    ( "Comparable"
    , TIntrinsic IString
    , InstanceEntry
        mempty
        (TIntrinsic IString)
        (TIntrinsic IString)
        ( Map.fromList
            [
              ( "(==)"
              , Forall mempty [] (TIntrinsic IString `TArrow` TIntrinsic IString `TArrow` TIntrinsic IBool)
              )
            ]
        )
    )
  ,
    ( "Divisible"
    , TIntrinsic IFloat
    , InstanceEntry
        mempty
        (TIntrinsic IFloat)
        (TIntrinsic IFloat)
        ( Map.fromList
            [
              ( "(/)"
              , Forall mempty [] (TIntrinsic IFloat `TArrow` TIntrinsic IFloat `TArrow` TIntrinsic IInt32)
              )
            ]
        )
    )
  ,
    ( "Divisible"
    , TIntrinsic IDouble
    , InstanceEntry
        mempty
        (TIntrinsic IDouble)
        (TIntrinsic IDouble)
        ( Map.fromList
            [
              ( "(/)"
              , Forall mempty [] (TIntrinsic IDouble `TArrow` TIntrinsic IDouble `TArrow` TIntrinsic IInt32)
              )
            ]
        )
    )
  ,
    ( "Modulo"
    , TIntrinsic IInt32
    , InstanceEntry
        mempty
        (TIntrinsic IInt32)
        (TIntrinsic IInt32)
        ( Map.fromList
            [
              ( "(%)"
              , Forall mempty [] (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IInt32)
              )
            ]
        )
    )
  ,
    ( "Modulo"
    , TIntrinsic IInt64
    , InstanceEntry
        mempty
        (TIntrinsic IInt64)
        (TIntrinsic IInt64)
        ( Map.fromList
            [
              ( "(%)"
              , Forall mempty [] (TIntrinsic IInt64 `TArrow` TIntrinsic IInt64 `TArrow` TIntrinsic IInt64)
              )
            ]
        )
    )
  ,
    ( "Semigroup"
    , TIntrinsic IString
    , InstanceEntry
        mempty
        (TIntrinsic IString)
        (TIntrinsic IString)
        ( Map.fromList
            [
              ( "(<>)"
              , Forall mempty [] (TIntrinsic IString `TArrow` TIntrinsic IString `TArrow` TIntrinsic IString)
              )
            ]
        )
    )
  ,
    ( "Semigroup"
    , applyTypeArgs KType (TConstructor (KArrow KType KType) "List") (TVariable (TypeIndex KType 0) :| [])
    , InstanceEntry
        mempty
        (applyTypeArgs () (TConstructor () "List") (TVariable (Parameter () "a") :| []))
        (applyTypeArgs KType (TConstructor (KArrow KType KType) "List") (TVariable (TypeIndex KType 0) :| []))
        ( Map.fromList
            [
              ( "(<>)"
              , Forall
                  mempty
                  []
                  ( applyTypeArgs KType (TConstructor (KArrow KType KType) "List") (TVariable (TypeIndex KType 0) :| [])
                      `TArrow` applyTypeArgs KType (TConstructor (KArrow KType KType) "List") (TVariable (TypeIndex KType 0) :| [])
                      `TArrow` applyTypeArgs KType (TConstructor (KArrow KType KType) "List") (TVariable (TypeIndex KType 0) :| [])
                  )
              )
            ]
        )
    )
  ]
