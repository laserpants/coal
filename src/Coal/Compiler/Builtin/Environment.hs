{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Builtin.Environment (
  typeConstructors,
  dataConstructors,
  instances,
  traits,
  codataAccessors,
) where

import Coal.Common.Environment (Environment (..))
import Coal.Common.Name (Dictionary)
import Coal.Language
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Extras (Name)

typeConstructors :: [(Name, Kind)]
typeConstructors =
  [
    ( "List"
    , KArrow KType KType
    )
  ]

dataConstructors :: [(Name, (DataConstructor TypeIndex Kind IndexedType, Set Name))]
dataConstructors =
  [
    ( "Succ"
    ,
      ( DataConstructor
          "Succ"
          1
          (Forall mempty [] (TIntrinsic INat `TArrow` TIntrinsic INat))
      , Set.fromList ["Succ", "Zero"]
      )
    )
  ,
    ( "Zero"
    ,
      ( DataConstructor
          "Zero"
          0
          (Forall mempty [] (TIntrinsic INat))
      , Set.fromList ["Succ", "Zero"]
      )
    )
  ]

instances :: [(Name, Map IndexedType (Type Parameter (), Dictionary IndexedScheme))]
instances =
  [
    ( "Numeric"
    , Map.fromList
        [
          ( TIntrinsic IInt32
          ,
            ( TIntrinsic IInt32
            , Map.fromList
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
          ( TIntrinsic IInt64
          ,
            ( TIntrinsic IInt64
            , Map.fromList
                [
                  ( "from_int32"
                  , Forall mempty [] (TIntrinsic IInt64 `TArrow` TIntrinsic IInt64)
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
          ( TIntrinsic IFloat
          ,
            ( TIntrinsic IFloat
            , Map.fromList
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
          ( TIntrinsic IDouble
          ,
            ( TIntrinsic IDouble
            , Map.fromList
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
          ( TIntrinsic INat
          ,
            ( TIntrinsic INat
            , Map.fromList
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
          ( TIntrinsic IBignum
          ,
            ( TIntrinsic IBignum
            , Map.fromList
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
        ]
    )
  ,
    ( "Ordered"
    , Map.fromList
        [
          ( TIntrinsic IInt32
          ,
            ( TIntrinsic IInt32
            , Map.fromList
                [
                  ( "compare"
                  , Forall mempty [] (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TConstructor KType "Ordering")
                  )
                ]
            )
          )
        ,
          ( TIntrinsic IInt64
          ,
            ( TIntrinsic IInt64
            , Map.fromList
                [
                  ( "compare"
                  , Forall mempty [] (TIntrinsic IInt64 `TArrow` TIntrinsic IInt64 `TArrow` TConstructor KType "Ordering")
                  )
                ]
            )
          )
        ,
          ( TIntrinsic IBool
          ,
            ( TIntrinsic IBool
            , Map.fromList
                [
                  ( "compare"
                  , Forall mempty [] (TIntrinsic IBool `TArrow` TIntrinsic IBool `TArrow` TConstructor KType "Ordering")
                  )
                ]
            )
          )
        ,
          ( TIntrinsic INat
          ,
            ( TIntrinsic INat
            , Map.fromList
                [
                  ( "compare"
                  , Forall mempty [] (TIntrinsic INat `TArrow` TIntrinsic INat `TArrow` TConstructor KType "Ordering")
                  )
                ]
            )
          )
        ,
          ( TIntrinsic IFloat
          ,
            ( TIntrinsic IFloat
            , Map.fromList
                [
                  ( "compare"
                  , Forall mempty [] (TIntrinsic IFloat `TArrow` TIntrinsic IFloat `TArrow` TConstructor KType "Ordering")
                  )
                ]
            )
          )
        ,
          ( TIntrinsic IDouble
          ,
            ( TIntrinsic IDouble
            , Map.fromList
                [
                  ( "compare"
                  , Forall mempty [] (TIntrinsic IDouble `TArrow` TIntrinsic IDouble `TArrow` TConstructor KType "Ordering")
                  )
                ]
            )
          )
        ,
          ( TIntrinsic IChar
          ,
            ( TIntrinsic IChar
            , Map.fromList
                [
                  ( "compare"
                  , Forall mempty [] (TIntrinsic IChar `TArrow` TIntrinsic IChar `TArrow` TConstructor KType "Ordering")
                  )
                ]
            )
          )
        ,
          ( TIntrinsic IBignum
          ,
            ( TIntrinsic IBignum
            , Map.fromList
                [
                  ( "compare"
                  , Forall mempty [] (TIntrinsic IBignum `TArrow` TIntrinsic IBignum `TArrow` TConstructor KType "Ordering")
                  )
                ]
            )
          )
        ]
    )
  ,
    ( "Comparable"
    , Map.fromList
        [
          ( TIntrinsic IInt32
          ,
            ( TIntrinsic IInt32
            , Map.fromList
                [
                  ( "(==)"
                  , Forall mempty [] (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IBool)
                  )
                ]
            )
          )
        ,
          ( TIntrinsic IInt64
          ,
            ( TIntrinsic IInt64
            , Map.fromList
                [
                  ( "(==)"
                  , Forall mempty [] (TIntrinsic IInt64 `TArrow` TIntrinsic IInt64 `TArrow` TIntrinsic IBool)
                  )
                ]
            )
          )
        ,
          ( TIntrinsic IBool
          ,
            ( TIntrinsic IBool
            , Map.fromList
                [
                  ( "(==)"
                  , Forall mempty [] (TIntrinsic IBool `TArrow` TIntrinsic IBool `TArrow` TIntrinsic IBool)
                  )
                ]
            )
          )
        ,
          ( TIntrinsic INat
          ,
            ( TIntrinsic INat
            , Map.fromList
                [
                  ( "(==)"
                  , Forall mempty [] (TIntrinsic INat `TArrow` TIntrinsic INat `TArrow` TIntrinsic IBool)
                  )
                ]
            )
          )
        ,
          ( TIntrinsic IFloat
          ,
            ( TIntrinsic IFloat
            , Map.fromList
                [
                  ( "(==)"
                  , Forall mempty [] (TIntrinsic IFloat `TArrow` TIntrinsic IFloat `TArrow` TIntrinsic IBool)
                  )
                ]
            )
          )
        ,
          ( TIntrinsic IDouble
          ,
            ( TIntrinsic IDouble
            , Map.fromList
                [
                  ( "(==)"
                  , Forall mempty [] (TIntrinsic IDouble `TArrow` TIntrinsic IDouble `TArrow` TIntrinsic IBool)
                  )
                ]
            )
          )
        ,
          ( TIntrinsic IChar
          ,
            ( TIntrinsic IChar
            , Map.fromList
                [
                  ( "(==)"
                  , Forall mempty [] (TIntrinsic IChar `TArrow` TIntrinsic IChar `TArrow` TIntrinsic IBool)
                  )
                ]
            )
          )
        ,
          ( TIntrinsic IBignum
          ,
            ( TIntrinsic IBignum
            , Map.fromList
                [
                  ( "(==)"
                  , Forall mempty [] (TIntrinsic IBignum `TArrow` TIntrinsic IBignum `TArrow` TIntrinsic IBool)
                  )
                ]
            )
          )
        ]
    )
  ,
    ( "Divisible"
    , Map.fromList
        [
          ( TIntrinsic IFloat
          ,
            ( TIntrinsic IFloat
            , Map.fromList
                [
                  ( "(/)"
                  , Forall mempty [] (TIntrinsic IFloat `TArrow` TIntrinsic IFloat `TArrow` TIntrinsic IInt32)
                  )
                ]
            )
          )
        ,
          ( TIntrinsic IDouble
          ,
            ( TIntrinsic IDouble
            , Map.fromList
                [
                  ( "(/)"
                  , Forall mempty [] (TIntrinsic IDouble `TArrow` TIntrinsic IDouble `TArrow` TIntrinsic IInt32)
                  )
                ]
            )
          )
        ]
    )
  ,
    ( "Mod"
    , Map.fromList
        [
          ( TIntrinsic IInt32
          ,
            ( TIntrinsic IInt32
            , Map.fromList
                [
                  ( "(%)"
                  , Forall mempty [] (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IInt32)
                  )
                ]
            )
          )
        ,
          ( TIntrinsic IInt64
          ,
            ( TIntrinsic IInt64
            , Map.fromList
                [
                  ( "(%)"
                  , Forall mempty [] (TIntrinsic IInt64 `TArrow` TIntrinsic IInt64 `TArrow` TIntrinsic IInt64)
                  )
                ]
            )
          )
        ]
    )
  ,
    ( "Semigroup"
    , Map.fromList
        [
          ( TIntrinsic IString
          ,
            ( TIntrinsic IString
            , Map.fromList
                [
                  ( "(<>)"
                  , Forall mempty [] (TIntrinsic IString `TArrow` TIntrinsic IString `TArrow` TIntrinsic IString)
                  )
                ]
            )
          )
        ,
          ( TApplication KType (TConstructor (KArrow KType KType) "List") (TVariable (TypeIndex KType 0) :| [])
          ,
            ( TApplication () (TConstructor () "List") (TVariable (Parameter () "a") :| [])
            , Map.fromList
                [
                  ( "(<>)"
                  , Forall
                      mempty
                      []
                      ( TApplication KType (TConstructor (KArrow KType KType) "List") (TVariable (TypeIndex KType 0) :| [])
                          `TArrow` TApplication KType (TConstructor (KArrow KType KType) "List") (TVariable (TypeIndex KType 0) :| [])
                          `TArrow` TApplication KType (TConstructor (KArrow KType KType) "List") (TVariable (TypeIndex KType 0) :| [])
                      )
                  )
                ]
            )
          )
        ]
    )
  ]

traits :: [(Name, (Parameter Kind, TypeIndex Kind, Environment IndexedScheme))]
traits =
  []

codataAccessors :: [(Name, CodataAccessor TypeIndex Kind IndexedType)]
codataAccessors =
  []
