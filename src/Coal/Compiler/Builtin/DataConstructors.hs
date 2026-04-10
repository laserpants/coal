{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Builtin.DataConstructors (builtinDataConstructors2) where

import Coal.Language
import Coal.ProtoCompiler.ProtoBuild.ProtoNameEntry
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set

builtinDataConstructors2 :: (Monoid a) => [ProtoDataConstructorEntry a]
builtinDataConstructors2 =
  [ ProtoDataConstructorEntry
      { protoOdataConstructorEntryMetaData = mempty
      , protoOdataConstructorEntryName = "Zero"
      , protoOdataConstructorEntryConstructor =
          DataConstructor
            "Zero"
            0
            (Forall mempty mempty (TIntrinsic INat))
      , protoOdataConstructorEntryConstructorSet =
          Set.fromList ["Succ", "Zero"]
      }
  , ProtoDataConstructorEntry
      { protoOdataConstructorEntryMetaData = mempty
      , protoOdataConstructorEntryName = "Succ"
      , protoOdataConstructorEntryConstructor =
          DataConstructor
            "Succ"
            1
            (Forall mempty mempty (TIntrinsic INat `TArrow` TIntrinsic INat))
      , protoOdataConstructorEntryConstructorSet =
          Set.fromList ["Succ", "Zero"]
      }
  , ProtoDataConstructorEntry
      { protoOdataConstructorEntryMetaData = mempty
      , protoOdataConstructorEntryName = "Process"
      , protoOdataConstructorEntryConstructor =
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
        protoOdataConstructorEntryConstructorSet =
          Set.fromList ["Process"]
      }
  ]

-- builtinDataConstructors :: (Monoid a) => [(Name, DataConstructorEntry a)]
-- builtinDataConstructors =
--  [
--    ( "Zero"
--    , DataConstructorEntry
--        mempty
--        "Zero"
--        ( DataConstructor
--            "Zero"
--            0
--            (Forall mempty mempty (TIntrinsic INat))
--        )
--        (Set.fromList ["Succ", "Zero"])
--    )
--  ,
--    ( "Succ"
--    , DataConstructorEntry
--        mempty
--        "Succ"
--        ( DataConstructor
--            "Succ"
--            1
--            (Forall mempty mempty (TIntrinsic INat `TArrow` TIntrinsic INat))
--        )
--        (Set.fromList ["Succ", "Zero"])
--    )
--  ,
--    ( "Process"
--    , DataConstructorEntry
--        mempty
--        "Process"
--        ( DataConstructor
--            "Process"
--            1
--            ( Forall
--                (Set.fromList [TypeIndex KType 0, TypeIndex KType 1])
--                mempty
--                ( TRecord
--                    ( TRow
--                        ( RExtend
--                            "state"
--                            (TVariable (TypeIndex KType 0))
--                            ( RExtend
--                                "step"
--                                ( TVariable (TypeIndex KType 1)
--                                    `TArrow` TVariable (TypeIndex KType 0)
--                                    `TArrow` applyTypeArgs
--                                      KType
--                                      (TConstructor (KArrow KType (KArrow KType KType)) "Process")
--                                      (TVariable (TypeIndex KType 0) :| [TVariable (TypeIndex KType 1)])
--                                )
--                                RNil
--                            )
--                        )
--                    )
--                    `TArrow` applyTypeArgs
--                      KType
--                      (TConstructor (KArrow KType (KArrow KType KType)) "Process")
--                      (TVariable (TypeIndex KType 0) :| [TVariable (TypeIndex KType 1)])
--                )
--            )
--        )
--        (Set.fromList ["Process"])
--    )
--  ]
