{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Builtin.DataConstructors (builtinDataConstructors) where

import Coal.Compiler.Build.NameEntry (DataConstructorEntry (..))
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
      , dataConstructorEntryName = "Machine"
      , dataConstructorEntryConstructor =
          DataConstructor
            "Machine"
            1
            ( forall2
                ( \i o ->
                    TRecord
                      ( TRow
                          ( RExtend
                              "state"
                              (TVariable (TypeIndex KType 9999999999))
                              ( RExtend
                                  "step"
                                  ( i
                                      `TArrow` TVariable (TypeIndex KType 9999999999)
                                      `TArrow` applyTypeArgs KType (TConstructor (KArrow KType (KArrow KType KType)) "Machine") (i :| [o])
                                  )
                                  ( RExtend
                                      "view"
                                      (TVariable (TypeIndex KType 9999999999) `TArrow` o)
                                      RNil
                                  )
                              )
                          )
                      )
                      `TArrow` applyTypeArgs KType (TConstructor (KArrow KType (KArrow KType KType)) "Machine") (i :| [o])
                )
            )
      , dataConstructorEntryConstructorSet =
          Set.fromList ["Machine"]
      }
  ]
