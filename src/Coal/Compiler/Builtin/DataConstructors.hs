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
      , dataConstructorEntryConstructorSet =
          Set.fromList ["Process"]
      }
  , DataConstructorEntry
      { dataConstructorEntryMetaData = mempty
      , dataConstructorEntryName = "Machine"
      , dataConstructorEntryConstructor =
          DataConstructor
            "Machine"
            1
            ( forall3
                ( \s i o ->
                    TRecord
                      ( TRow
                          ( RExtend
                              "state"
                              s
                              ( RExtend
                                  "step"
                                  ( i
                                      `TArrow` s
                                      `TArrow` applyTypeArgs KType (TConstructor (KArrow KType (KArrow KType (KArrow KType KType))) "Machine") (s :| [i, o])
                                  )
                                  ( RExtend
                                      "view"
                                      (s `TArrow` o)
                                      RNil
                                  )
                              )
                          )
                      )
                      `TArrow` applyTypeArgs KType (TConstructor (KArrow KType (KArrow KType (KArrow KType KType))) "Machine") (s :| [i, o])
                )
            )
      , dataConstructorEntryConstructorSet =
          Set.fromList ["Machine"]
      }
  ]
