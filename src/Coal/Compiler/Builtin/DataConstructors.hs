{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Builtin.DataConstructors (builtinDataConstructors) where

import Coal.Compiler.Build.NameEntry
import Coal.Language
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set

builtinDataConstructors :: (Monoid a) => [DataConstructorEntry a]
builtinDataConstructors =
  [ DataConstructorEntry
      { dataConstructorEntryMetaData = mempty
      , dataConstructorEntryName = "Zero"
      , dataConstructorEntryConstructor =
          DataConstructor
            "Zero"
            0
            (Forall mempty mempty (TIntrinsic INat))
      , dataConstructorEntryConstructorSet =
          Set.fromList ["Succ", "Zero"]
      }
  , DataConstructorEntry
      { dataConstructorEntryMetaData = mempty
      , dataConstructorEntryName = "Succ"
      , dataConstructorEntryConstructor =
          DataConstructor
            "Succ"
            1
            (Forall mempty mempty (TIntrinsic INat `TArrow` TIntrinsic INat))
      , dataConstructorEntryConstructorSet =
          Set.fromList ["Succ", "Zero"]
      }
  , DataConstructorEntry
      { dataConstructorEntryMetaData = mempty
      , dataConstructorEntryName = "Process"
      , dataConstructorEntryConstructor =
          DataConstructor
            "Process"
            1
            ( forall2
                ( \a b ->
                    TRecord
                      ( TRow
                          ( RExtend
                              "state"
                              a
                              ( RExtend
                                  "step"
                                  ( b
                                      `TArrow` a
                                      `TArrow` applyTypeArgs KType (TConstructor (KArrow KType (KArrow KType KType)) "Process") (a :| [b])
                                  )
                                  RNil
                              )
                          )
                      )
                      `TArrow` applyTypeArgs KType (TConstructor (KArrow KType (KArrow KType KType)) "Process") (a :| [b])
                )
            )
      , -- ( Forall
        --    (Set.fromList [TypeIndex KType 0, TypeIndex KType 1])
        --    mempty
        --    ( TRecord
        --        ( TRow
        --            ( RExtend
        --                "state"
        --                (TVariable (TypeIndex KType 0))
        --                ( RExtend
        --                    "step"
        --                    ( TVariable (TypeIndex KType 1)
        --                        `TArrow` TVariable (TypeIndex KType 0)
        --                        `TArrow` applyTypeArgs
        --                          KType
        --                          (TConstructor (KArrow KType (KArrow KType KType)) "Process")
        --                          (TVariable (TypeIndex KType 0) :| [TVariable (TypeIndex KType 1)])
        --                    )
        --                    RNil
        --                )
        --            )
        --        )
        --        `TArrow` applyTypeArgs
        --          KType
        --          (TConstructor (KArrow KType (KArrow KType KType)) "Process")
        --          (TVariable (TypeIndex KType 0) :| [TVariable (TypeIndex KType 1)])
        --    )
        -- )
        dataConstructorEntryConstructorSet =
          Set.fromList ["Process"]
      }
  ]
