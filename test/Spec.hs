{-# LANGUAGE OverloadedStrings #-}

import qualified Coal.Common.Environment as Environment
import Coal.Compiler.PatternMatching.AnomalyDetectionSpec (patternAnomaliesSpec)
import Coal.Kernel.Spec (kernelSpec)
import Coal.Language.Type
import Coal.Language.Type.Kind (Kind (KArrow, KType))
import Coal.Language.TypeSpec (typeApplicationSpec, typeArgsSpec)
import Coal.TypeSystem.Parameterized (ToIndexed (toIndexed))
import Coal.TypeSystemSpec (typeSystemSpec)
import Control.Monad.Reader (ReaderT (runReaderT))
import Control.Monad.State (State, runState)
import E2E.Spec (e2eSpec)
import Test.Hspec (SpecWith, describe, hspec)

spec :: SpecWith ()
spec =
  describe "Unit tests" $ do
    typeSystemSpec
    typeArgsSpec
    typeApplicationSpec
    patternAnomaliesSpec

main :: IO ()
main =
  hspec $ do
    spec
    kernelSpec
    --    buildSpec
    e2eSpec

--    e2eKernelSpec

fooa = TArrow (TApplication KType (TApplication (KArrow KType KType) (TConstructor (KArrow KType (KArrow KType KType)) "Writer") (TVariable (TypeIndex{typeIndexKind = KType, typeIndexId = 645}))) (TVariable (TypeIndex{typeIndexKind = KType, typeIndexId = 647}))) (TArrow (TArrow (TVariable (TypeIndex{typeIndexKind = KType, typeIndexId = 647})) (TApplication KType (TApplication (KArrow KType KType) (TConstructor (KArrow KType (KArrow KType KType)) "Writer") (TVariable (TypeIndex{typeIndexKind = KType, typeIndexId = 645}))) (TVariable (TypeIndex{typeIndexKind = KType, typeIndexId = 646})))) (TApplication KType (TApplication (KArrow KType KType) (TConstructor (KArrow KType (KArrow KType KType)) "Writer") (TVariable (TypeIndex{typeIndexKind = KType, typeIndexId = 645}))) (TVariable (TypeIndex{typeIndexKind = KType, typeIndexId = 646}))))
foob = TArrow (TApplication KType (TApplication (KArrow KType KType) (TConstructor (KArrow KType (KArrow KType KType)) "Writer") (TVariable (TypeIndex{typeIndexKind = KType, typeIndexId = 4389}))) (TVariable (TypeIndex{typeIndexKind = KType, typeIndexId = 0}))) (TArrow (TArrow (TVariable (TypeIndex{typeIndexKind = KType, typeIndexId = 0})) (TApplication KType (TApplication (KArrow KType KType) (TConstructor (KArrow KType (KArrow KType KType)) "Writer") (TVariable (TypeIndex{typeIndexKind = KType, typeIndexId = 4390}))) (TVariable (TypeIndex{typeIndexKind = KType, typeIndexId = 1})))) (TApplication KType (TApplication (KArrow KType KType) (TConstructor (KArrow KType (KArrow KType KType)) "Writer") (TVariable (TypeIndex{typeIndexKind = KType, typeIndexId = 4391}))) (TVariable (TypeIndex{typeIndexKind = KType, typeIndexId = 1}))))

fooc = TArrow (TApplication KType (TApplication (KArrow KType KType) (TConstructor (KArrow KType (KArrow KType KType)) "Writer") (TVariable (TypeIndex{typeIndexKind = KType, typeIndexId = 4389}))) (TVariable (TypeIndex{typeIndexKind = KType, typeIndexId = 0}))) (TArrow (TArrow (TVariable (TypeIndex{typeIndexKind = KType, typeIndexId = 0})) (TApplication KType (TApplication (KArrow KType KType) (TConstructor (KArrow KType (KArrow KType KType)) "Writer") (TVariable (TypeIndex{typeIndexKind = KType, typeIndexId = 4389}))) (TVariable (TypeIndex{typeIndexKind = KType, typeIndexId = 1})))) (TApplication KType (TApplication (KArrow KType KType) (TConstructor (KArrow KType (KArrow KType KType)) "Writer") (TVariable (TypeIndex{typeIndexKind = KType, typeIndexId = 4389}))) (TVariable (TypeIndex{typeIndexKind = KType, typeIndexId = 1}))))

bbb = Environment.fromList [("a", TypeIndex{typeIndexKind = KType, typeIndexId = 4600}), ("b", TypeIndex{typeIndexKind = KType, typeIndexId = 4601})]

food :: State Int IndexedType
food =
  flip runReaderT bbb $
    toIndexed $
      TArrow
        ( TApplication
            KType
            ( TApplication
                (KArrow KType KType)
                (TConstructor (KArrow KType (KArrow KType KType)) "Writer")
                (TVariable (Parameter{parameterKind = KType, parameterName = "w"}))
            )
            (TVariable (Parameter{parameterKind = KType, parameterName = "a"}))
        )
        ( TArrow
            ( TArrow
                (TVariable (Parameter{parameterKind = KType, parameterName = "a"}))
                ( TApplication
                    KType
                    ( TApplication
                        (KArrow KType KType)
                        (TConstructor (KArrow KType (KArrow KType KType)) "Writer")
                        (TVariable (Parameter{parameterKind = KType, parameterName = "w"}))
                    )
                    (TVariable (Parameter{parameterKind = KType, parameterName = "b"}))
                )
            )
            ( TApplication
                KType
                ( TApplication
                    (KArrow KType KType)
                    (TConstructor (KArrow KType (KArrow KType KType)) "Writer")
                    (TVariable (Parameter{parameterKind = KType, parameterName = "w"}))
                )
                (TVariable (Parameter{parameterKind = KType, parameterName = "b"}))
            )
        )

res =
  TArrow
    ( TApplication
        KType
        ( TApplication
            (KArrow KType KType)
            (TConstructor (KArrow KType (KArrow KType KType)) "Writer")
            (TVariable (TypeIndex{typeIndexKind = KType, typeIndexId = 10000000}))
        )
        (TVariable (TypeIndex{typeIndexKind = KType, typeIndexId = 5}))
    )
    ( TArrow
        ( TArrow
            (TVariable (TypeIndex{typeIndexKind = KType, typeIndexId = 5}))
            ( TApplication
                KType
                ( TApplication
                    (KArrow KType KType)
                    (TConstructor (KArrow KType (KArrow KType KType)) "Writer")
                    (TVariable (TypeIndex{typeIndexKind = KType, typeIndexId = 10000001}))
                )
                (TVariable (TypeIndex{typeIndexKind = KType, typeIndexId = 6}))
            )
        )
        ( TApplication
            KType
            ( TApplication
                (KArrow KType KType)
                (TConstructor (KArrow KType (KArrow KType KType)) "Writer")
                (TVariable (TypeIndex{typeIndexKind = KType, typeIndexId = 10000002}))
            )
            (TVariable (TypeIndex{typeIndexKind = KType, typeIndexId = 6}))
        )
    )

fooe = runState food 10000000

-- foox :: IndexedType
-- foox = t
-- where
--  (t, _, _) =
--    runSolver 100 $
--      instantiate
--        ( Forall
--            ( Set.fromList
--                [ TypeIndex (KArrow KType KType) 0
--                , TypeIndex KType 1
--                , TypeIndex KType 2
--                ]
--            )
--            []
--            ( ( TApplication
--                  KType
--                  (TVariable (TypeIndex (KArrow KType KType) 0))
--                  (TVariable (TypeIndex KType 1))
--              )
--                `TArrow` ( (TVariable (TypeIndex KType 1))
--                            `TArrow` ( TApplication
--                                        KType
--                                        (TVariable (TypeIndex (KArrow KType KType) 0))
--                                        (TVariable (TypeIndex KType 2))
--                                     )
--                         )
--                `TArrow` ( TApplication
--                            KType
--                            (TVariable (TypeIndex (KArrow KType KType) 0))
--                            (TVariable (TypeIndex KType 2))
--                         )
--            )
--        )
