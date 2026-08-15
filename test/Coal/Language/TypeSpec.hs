{-# LANGUAGE OverloadedStrings #-}

module Coal.Language.TypeSpec (typeArgsSpec, typeApplicationSpec) where

import Coal.Language
import Data.List.NonEmpty
import Test.Hspec

typeArgsSpec :: Spec
typeArgsSpec =
  describe "Type.typeArgs" $ do
    it "extracts a single type argument" $
      -- Foo<nat>
      typeArgs
        ( TApplication
            KType
            (TConstructor (KArrow KType KType) "Foo")
            (TIntrinsic INat) ::
            IndexedType
        )
        `shouldBe` ( TConstructor (KArrow KType KType) "Foo"
                   , TIntrinsic INat :| []
                   )

    it "extracts two type arguments" $
      -- Foo2<nat, int32>
      typeArgs
        ( TApplication
            KType
            ( TApplication
                (KArrow KType KType)
                (TConstructor (KArrow KType (KArrow KType KType)) "Foo2")
                (TIntrinsic INat) ::
                IndexedType
            )
            (TIntrinsic IInt32) ::
            IndexedType
        )
        `shouldBe` ( TConstructor (KArrow KType (KArrow KType KType)) "Foo2"
                   , TIntrinsic INat :| [TIntrinsic IInt32]
                   )

    it "extracts three type arguments" $
      -- Foo3<nat, string, int32,>
      typeArgs
        ( TApplication
            KType
            ( TApplication
                (KArrow KType KType)
                ( TApplication
                    (KArrow KType (KArrow KType KType))
                    (TConstructor (KArrow KType (KArrow KType (KArrow KType KType))) "Foo3")
                    (TIntrinsic INat) ::
                    IndexedType
                )
                (TIntrinsic IString)
            )
            (TIntrinsic IInt32) ::
            IndexedType
        )
        `shouldBe` ( TConstructor (KArrow KType (KArrow KType (KArrow KType KType))) "Foo3"
                   , TIntrinsic INat :| [TIntrinsic IString, TIntrinsic IInt32]
                   )

typeApplicationSpec :: Spec
typeApplicationSpec =
  describe "Type.applyTypeArgs" $ do
    it "applies a single type argument" $
      -- Foo<nat>
      applyTypeArgs KType (TConstructor (KArrow KType KType) "Foo") (TIntrinsic INat :| [])
        `shouldBe` ( TApplication
                       KType
                       (TConstructor (KArrow KType KType) "Foo")
                       (TIntrinsic INat) ::
                       IndexedType
                   )

    it "applies two type arguments" $
      -- Foo2<nat, int32>
      applyTypeArgs KType (TConstructor (KArrow KType (KArrow KType KType)) "Foo2") (TIntrinsic INat :| [TIntrinsic IInt32])
        `shouldBe` ( TApplication
                       KType
                       ( TApplication
                           (KArrow KType KType)
                           (TConstructor (KArrow KType (KArrow KType KType)) "Foo2")
                           (TIntrinsic INat) ::
                           IndexedType
                       )
                       (TIntrinsic IInt32) ::
                       IndexedType
                   )

    it "applies three type arguments" $
      -- Foo3<nat, string, int32,>
      applyTypeArgs KType (TConstructor (KArrow KType (KArrow KType (KArrow KType KType))) "Foo3") (TIntrinsic INat :| [TIntrinsic IString, TIntrinsic IInt32])
        `shouldBe` ( TApplication
                       KType
                       ( TApplication
                           (KArrow KType KType)
                           ( TApplication
                               (KArrow KType (KArrow KType KType))
                               (TConstructor (KArrow KType (KArrow KType (KArrow KType KType))) "Foo3")
                               (TIntrinsic INat) ::
                               IndexedType
                           )
                           (TIntrinsic IString)
                       )
                       (TIntrinsic IInt32) ::
                       IndexedType
                   )
