{-# LANGUAGE OverloadedStrings #-}

module Noll.Compiler.Dictionaries where

import Lang.Common.List1 (List1, NonEmpty ((:|)), (<|))
import Lang.Label (Label (..))
import Noll.Language.Expression (Expression (..))
import Noll.Language.Trait
import Noll.Language.Type
import Noll.Language.Type.Kind

listPair =
  TApplication
    KType
    (TConstructor KType "list")
    ( TApplication
        KType
        (TConstructor (KType `KArrow` KType `KArrow` KType) "pair")
        ( TVariable (TypeIndex KType 0)
            <| TVariable (TypeIndex KType 1)
            :| []
        )
        :| []
    )

orderedDict t =
  TApplication
    KTrait
    (TConstructor (KType `KArrow` KTrait) "Ordered")
    (t :| [])

sample1 =
  EApplication
    ()
    (TConstructor KType "Ordering")
    ( EVariable
        ()
        (Label (listPair `TArrow` listPair `TArrow` TConstructor KType "Ordering") "compare")
    )
    ( EVariable () (Label listPair "xs")
        <| EVariable () (Label listPair "ys")
        :| []
    )

sample1Result1 =
  EApplication
    ()
    (TConstructor KType "Ordering")
    -- (EVariable () (Label (listPair `TArrow` listPair `TArrow` TConstructor KType "Ordering") "compare"))
    ( EApplication
        ()
        undefined
        (EVariable () (Label (listPair `TArrow` listPair `TArrow` TConstructor KType "Ordering") "compare"))
        (EDictionary () (orderedDict listPair) (Trait "Ordered" listPair) :| [])
    )
    ( EVariable () (Label listPair "xs")
        <| EVariable () (Label listPair "ys")
        :| []
    )

-- translator :: function -> args -> [traits] -> ([traits], expr)

-- appl1 = translator (compare fun) [xs, ys] [Ordered (pair(a, b))]
