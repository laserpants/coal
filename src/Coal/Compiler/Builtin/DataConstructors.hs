{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Builtin.DataConstructors (builtinDataConstructors) where

import Coal.Compiler.Build
import Coal.Language
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set
import Extras (Name)

builtinDataConstructors :: (Monoid a) => [(Name, DataConstructorEntry a)]
builtinDataConstructors =
  [
    ( "Zero"
    , DataConstructorEntry
        mempty
        "Zero"
        ( DataConstructor
            "Zero"
            0
            (Forall mempty [] (TIntrinsic INat))
        )
        (Set.fromList ["Succ", "Zero"])
    )
  ,
    ( "Succ"
    , DataConstructorEntry
        mempty
        "Succ"
        ( DataConstructor
            "Succ"
            1
            (Forall mempty [] (TIntrinsic INat `TArrow` TIntrinsic INat))
        )
        (Set.fromList ["Succ", "Zero"])
    )
  ,
    ( "Process"
    , DataConstructorEntry
        mempty
        "Process"
        ( DataConstructor
            "Process"
            1
            ( Forall
                (Set.fromList [TypeIndex KType 0, TypeIndex KType 1])
                []
                ( TRecord
                    ( TRow
                        ( RExtend
                            "state"
                            (TVariable (TypeIndex KType 0))
                            ( RExtend
                                "step"
                                ( TVariable (TypeIndex KType 1)
                                    `TArrow` TVariable (TypeIndex KType 0)
                                    `TArrow` applyTypeArgs
                                      KType
                                      (TConstructor (KArrow KType (KArrow KType KType)) "Process")
                                      (TVariable (TypeIndex KType 0) :| [TVariable (TypeIndex KType 1)])
                                )
                                RNil
                            )
                        )
                    )
                    `TArrow` applyTypeArgs
                      KType
                      (TConstructor (KArrow KType (KArrow KType KType)) "Process")
                      (TVariable (TypeIndex KType 0) :| [TVariable (TypeIndex KType 1)])
                )
            )
        )
        (Set.fromList ["Process"])
    )
  ]
