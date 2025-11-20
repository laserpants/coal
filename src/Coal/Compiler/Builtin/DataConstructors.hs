{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Builtin.DataConstructors (builtinDataConstructors) where

import Coal.Compiler.Build
import Coal.Language
import qualified Data.Set as Set
import Extras (Name)

builtinDataConstructors :: (Monoid a) => [(Name, DataConstructorInfo a)]
builtinDataConstructors =
  [
    ( "Zero"
    , DataConstructorInfo
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
    , DataConstructorInfo
        mempty
        "Succ"
        ( DataConstructor
            "Succ"
            1
            (Forall mempty [] (TIntrinsic INat `TArrow` TIntrinsic INat))
        )
        (Set.fromList ["Succ", "Succ"])
    )
  ]
