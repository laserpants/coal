{-# LANGUAGE OverloadedStrings #-}

module Coal.Language.TypeSpec where

import Coal.Language
import Data.List.NonEmpty
import Test.Hspec

typeArgsSpec :: Spec
typeArgsSpec =
  describe "Type.typeArgs" $ do
    it "" $
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

    it "" $
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

    it "" $
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
    it "" $
      -- Foo<nat>
      applyTypeArgs KType (TConstructor (KArrow KType KType) "Foo") (TIntrinsic INat :| [])
        `shouldBe` ( TApplication
                      KType
                      (TConstructor (KArrow KType KType) "Foo")
                      (TIntrinsic INat) ::
                      IndexedType
                   )

    it "" $
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

    it "" $
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
