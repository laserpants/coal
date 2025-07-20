{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Noll.Compiler2Spec where

import Control.Monad ((>=>))
import Control.Monad.Identity (runIdentity)
import Control.Monad.State (get)
import Data.Set (Set)
import Data.Text (Text)
import Debug.Trace
import Lang.Common.Environment (Environment)
import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Lang.Lowpass.Language (Module (..), Object (..), opaque)
import Lang.Utils (Dictionary, Name, forM, forM_)
import Noll.Compiler.Dictionaries
import Noll.Compiler2
import Noll.Compiler2.Internal
import Noll.Language.Trait
import Noll.Parser.Module
import Text.Megaparsec (runParser)
import Text.RawString.QQ

import Data.Map.Strict (Map)

-- import Noll.Compiler2Examples.Test02 (bazz)

import Noll.Language (Constructor (..), IndexedType (..), Intrinsic (..), Kind (..), Parameter (..), Row (..), Scheme (..), Type (..), TypeIndex (..))
import Noll.Module (Constant (..), Definition (..), Function (..), Module (..))
import Noll.SystemF
import Noll.SystemFSpec.TestRunner
import Test.Hspec (Spec, describe, it)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Lang.Common.Environment as Environment
import qualified Lang.Lowpass.Compiler as Lowpass
import qualified Lang.Lowpass.Compiler.Utils as Lowpass
import qualified Lang.Lowpass.Language as Lowpass
import qualified Noll.Set.Test01
import qualified Noll.Set10.Test01
import qualified Noll.Set11.Test01
import qualified Noll.Set12.Test01
import qualified Noll.Set20.Test01
import qualified Noll.Set21.Test01
import qualified Noll.Set22.Test01
import qualified Noll.Set23.Test01
import qualified Noll.Set24.Test01
import qualified Noll.Set7.Test01
import qualified Noll.Set8.Test01
import qualified Noll.Set9.Test01

import qualified Data.Text as Text

spec :: Spec
spec =
  describe "Noll.Compiler2" $ do
    it "" $ do
      1 == 2

compiler2TestEnvironment =
  Compiler2Environment
    { compiler2DataConstructorEnv = env1
    , compiler2TypeConstructorEnv = env2
    , compiler2TraitEnvironment = env3
    , compiler2AliasEnv = env5
    , compiler2DictionaryEnvironment = env6
    }

env1 =
  Environment.fromList
    [
      ( "LessThan"
      , Constructor
          "LessThan"
          0
          (Forall mempty [] (TConstructor KType "Ordering"))
      )
    ,
      ( "GreaterThan"
      , Constructor
          "GreaterThan"
          0
          (Forall mempty [] (TConstructor KType "Ordering"))
      )
    ,
      ( "EqualTo"
      , Constructor
          "EqualTo"
          0
          (Forall mempty [] (TConstructor KType "Ordering"))
      )
    ,
      ( "Node"
      , Constructor
          "Node"
          3
          ( Forall
              (Set.fromList [TypeIndex KType 0])
              []
              ( tvariable0
                  `TArrow` tree0
                  `TArrow` tree0
                  `TArrow` tree0
              )
          )
      )
    ,
      ( "Leaf"
      , Constructor
          "Leaf"
          0
          ( Forall
              (Set.fromList [TypeIndex KType 0])
              []
              tree0
          )
      )
    ,
      ( "Succ"
      , Constructor
          "Succ"
          1
          (Forall mempty [] (TIntrinsic INat `TArrow` TIntrinsic INat))
      )
    ,
      ( "Zero"
      , Constructor
          "Zero"
          0
          (Forall mempty [] (TIntrinsic INat))
      )
      --    ,
      --      ( "$Succ"
      --      , Constructor
      --          "$Succ"
      --          1
      --          (Forall mempty [] (TIntrinsic IInt32 `TArrow` TConstructor KType "$Nat"))
      --      )
      --    ,
      --      ( "$Zero"
      --      , Constructor
      --          "$Zero"
      --          0
      --          (Forall mempty [] (TConstructor KType "$Nat"))
      --      )
    ]

env2 =
  Environment.fromList
    [
      ( "Tree"
      , KArrow KType KType
      )
    ,
      ( "Ordering"
      , KType
      )
      --    , ( "$Nat"
      --      , KType
      --      )
    ]

env3 =
  Environment.fromList
    [
      ( "Numeric"
      ,
        ( TypeIndex KType 0
        , Environment.fromList
            [
              ( "from_int32"
              , Forall
                  (Set.fromList [TypeIndex KType 0])
                  []
                  ( TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 0)
                  )
              )
            ]
        )
      )
    ,
      ( "Ordered"
      ,
        ( TypeIndex KType 0
        , Environment.fromList
            [
              ( "compare"
              , Forall
                  (Set.fromList [TypeIndex KType 0])
                  []
                  ( TVariable (TypeIndex KType 0)
                      `TArrow` TVariable (TypeIndex KType 0)
                      `TArrow` TConstructor KType "Ordering"
                  )
              )
            ]
        )
      )
    ]

--env4 =
--  Environment.fromList
--    [
--      ( "Numeric"
--      ,
--        ( TypeIndex KType 0
--        , Environment.fromList
--            [
--              ( "from_int32"
--              , Forall
--                  (Set.fromList [TypeIndex KType 0])
--                  []
--                  ( TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 0)
--                  )
--              )
--            ]
--        )
--      )
--    ,
--      ( "Ordered"
--      ,
--        ( TypeIndex KType 0
--        , Environment.fromList
--            [
--              ( "compare"
--              , Forall
--                  (Set.fromList [TypeIndex KType 0])
--                  []
--                  ( TVariable (TypeIndex KType 0)
--                      `TArrow` TVariable (TypeIndex KType 0)
--                      `TArrow` TConstructor KType "Ordering"
--                  )
--              )
--            ]
--        )
--      )
--    ]

env5 =
  Environment.fromList
    [
      ( "Predicate"
      ,
        ( ["a"]
        , TVariable (Parameter () "a") `TArrow` TIntrinsic IBool
        )
      )
    ,
      ( "Range"
      ,
        ( ["a"]
        , TIntrinsic
            ( IRecord
                ( TRow
                    ( RExtend
                        "max"
                        (TVariable (Parameter () "a"))
                        ( RExtend
                            "min"
                            (TVariable (Parameter () "a"))
                            RNil
                        )
                    )
                )
            )
        )
      )
    ]

env6 = DictionaryEnvironment yy xx

xx :: Environment (Map IndexedType (Dictionary (Scheme TypeIndex Kind IndexedType)))
xx =
  Environment.fromList
    [
      ( "Numeric"
      , Map.fromList
          [
            ( TIntrinsic IInt32
            , Map.fromList
                [
                  ( "from_int32"
                  , Forall mempty [] (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32)
                  )
                ]
            )
          ,
            ( TIntrinsic INat
            , Map.fromList
                [
                  ( "from_int32"
                  , Forall mempty [] (TIntrinsic IInt32 `TArrow` TIntrinsic INat)
                  )
                ]
            )
          ]
      )
    ,
      ( "Ordered"
      , Map.fromList
          [
            ( TIntrinsic IInt32
            , Map.fromList
                [
                  ( "compare"
                  , Forall (Set.fromList mempty) [] (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TConstructor KType "Ordering")
                  )
                ]
            )
          ]
      )
    ,
      ( "Traceable"
      , Map.fromList
          [
            ( TIntrinsic IString
            , Map.fromList
                [
                  ( "trace"
                  , Forall mempty [] (TIntrinsic IString `TArrow` TIntrinsic IString)
                  )
                ]
            )
          ,
            ( TIntrinsic IInt32
            , Map.fromList
                [
                  ( "trace"
                  , Forall mempty [] (TIntrinsic IInt32 `TArrow` TIntrinsic IString)
                  )
                ]
            )
          ,
            ( TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)])
            , Map.fromList
                [
                  ( "trace"
                  , Forall
                      (Set.fromList [TypeIndex KType 0, TypeIndex KType 1])
                      [ Trait "Traceable" (TVariable (TypeIndex KType 0))
                      , Trait "Traceable" (TVariable (TypeIndex KType 1))
                      ]
                      (TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)]) `TArrow` TIntrinsic IString)
                  )
                ]
            )
          ,
            ( TIntrinsic (IList (TVariable (TypeIndex KType 0)))
            , Map.fromList
                [
                  ( "trace"
                  , Forall
                      (Set.fromList [TypeIndex KType 0])
                      [ Trait "Traceable" (TVariable (TypeIndex KType 0))
                      ]
                      (TIntrinsic (IList (TVariable (TypeIndex KType 0))) `TArrow` TIntrinsic IString)
                  )
                ]
            )
          ]
      )
    ]

yy :: Environment (Scheme TypeIndex Kind IndexedType)
yy =
  Environment.fromList
    [
      ( "trace"
      , Forall
          (Set.fromList [TypeIndex KType 0] :: Set (TypeIndex Kind))
          [Trait "Traceable" (TVariable (TypeIndex KType 0))]
          (TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IString)
      )
    ,
      ( "pair_to_string"
      , Forall
          (Set.fromList [TypeIndex KType 0, TypeIndex KType 1] :: Set (TypeIndex Kind))
          [ Trait "Traceable" (TVariable (TypeIndex KType 0))
          , Trait "Traceable" (TVariable (TypeIndex KType 1))
          ]
          (TIntrinsic (ITuple [TVariable (TypeIndex KType 0), TVariable (TypeIndex KType 1)]) `TArrow` TIntrinsic IString)
      )
    ,
      ( "list_to_string"
      , Forall
          (Set.fromList [TypeIndex KType 0] :: Set (TypeIndex Kind))
          [ Trait "Traceable" (TVariable (TypeIndex KType 0))
          ]
          (TIntrinsic (IList (TVariable (TypeIndex KType 0))) `TArrow` TIntrinsic IString)
      )
    ,
      ( "from_int32"
      , Forall
          (Set.fromList [TypeIndex KType 0] :: Set (TypeIndex Kind))
          [Trait "Numeric" (TVariable (TypeIndex KType 0))]
          (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 0))
      )
    ,
      ( "greater_than"
      , Forall
          (Set.fromList [TypeIndex KType 0] :: Set (TypeIndex Kind))
          [Trait "Ordered" (TVariable (TypeIndex KType 0))]
          (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool)
      )
    ,
      ( "less_than_or_equal_to"
      , Forall
          (Set.fromList [TypeIndex KType 0] :: Set (TypeIndex Kind))
          [Trait "Ordered" (TVariable (TypeIndex KType 0))]
          (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool)
      )
    ,
      ( "compare"
      , Forall
          (Set.fromList [TypeIndex KType 0] :: Set (TypeIndex Kind))
          [Trait "Ordered" (TVariable (TypeIndex KType 0))]
          (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TConstructor KType "Ordering")
      )
    ,
      ( "from_list"
      , Forall
          (Set.fromList [TypeIndex KType 0] :: Set (TypeIndex Kind))
          --           [Trait "Numeric" (TVariable (TypeIndex KType 0)), Trait "Ordered" (TVariable (TypeIndex KType 0))]
          [Trait "Ordered" (TVariable (TypeIndex KType 0)), Trait "Numeric" (TVariable (TypeIndex KType 0))]
          (TIntrinsic (IList (TVariable (TypeIndex KType 0))) `TArrow` (TApplication KType (TConstructor (KArrow KType KType) "Tree") (TVariable (TypeIndex KType 0) :| [])))
      )
    ,
      ( "in_range"
      , Forall
          (Set.fromList [TypeIndex KType 0] :: Set (TypeIndex Kind))
          -- [Trait "Numeric" (TVariable (TypeIndex KType 0)), Trait "Ordered" (TVariable (TypeIndex KType 0))]
          [Trait "Ordered" (TVariable (TypeIndex KType 0)), Trait "Numeric" (TVariable (TypeIndex KType 0))]
          -- [Trait "Ordered" (TVariable (TypeIndex KType 0))]
          ( TIntrinsic (IRecord (TRow (RExtend "max" (TVariable (TypeIndex KType 0)) (RExtend "min" (TVariable (TypeIndex KType 0)) RNil))))
              `TArrow` TVariable (TypeIndex KType 0)
              `TArrow` TIntrinsic IBool
          )
      )
    ,
      ( "sort"
      , Forall
          (Set.fromList [TypeIndex KType 0] :: Set (TypeIndex Kind))
          [Trait "Numeric" (TVariable (TypeIndex KType 0)), Trait "Ordered" (TVariable (TypeIndex KType 0))]
          (TIntrinsic (IList (TVariable (TypeIndex KType 0))) `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 0))))
      )
    ]

tree0 :: IndexedType
tree0 =
  TApplication
    KType
    (TConstructor (KArrow KType KType) "Tree")
    (TVariable (TypeIndex KType 0) :| [])

tvariable0 :: IndexedType
tvariable0 = TVariable (TypeIndex KType 0)

tvariable1 :: IndexedType
tvariable1 = TVariable (TypeIndex KType 1)

bool :: IndexedType
bool = TIntrinsic IBool

-- abc1 :: Module () Kind IndexedType
-- abc1 = fst (runIdentity (runCompiler2T compiler2TestEnvironment ((typePass >=> mainPass) Noll.Set7.Test01.moduleMain)))

abc2 :: Lowpass.Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type)
abc2 = fst (runIdentity (runCompiler2T compiler2TestEnvironment (compileModule Noll.Set7.Test01.moduleMain)))

abc3 :: IO ()
abc3 = Lowpass.testModules =<< Lowpass.compileModules [moduleCore1, abc2]

--

abc4 :: Lowpass.Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type)
abc4 = fst (runIdentity (runCompiler2T compiler2TestEnvironment (compileModule Noll.Set8.Test01.moduleMain)))

abc5 :: IO ()
abc5 = Lowpass.testModules =<< Lowpass.compileModules [moduleCore1, abc4]

--

abc6 :: Lowpass.Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type)
abc6 = fst (runIdentity (runCompiler2T compiler2TestEnvironment (compileModule Noll.Set9.Test01.moduleUtilities)))

abc7 :: Lowpass.Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type)
abc7 = fst (runIdentity (runCompiler2T compiler2TestEnvironment (compileModule Noll.Set9.Test01.moduleMain)))

abc8 :: IO ()
abc8 = Lowpass.testModules =<< Lowpass.compileModules [moduleCore1, abc6, abc7]

--

abc9 :: Lowpass.Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type)
abc9 = fst (runIdentity (runCompiler2T compiler2TestEnvironment (compileModule Noll.Set10.Test01.moduleUtilities)))

abc10 :: Lowpass.Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type)
abc10 = fst (runIdentity (runCompiler2T compiler2TestEnvironment (compileModule Noll.Set10.Test01.moduleMain)))

abc11 :: IO ()
abc11 = Lowpass.testModules =<< Lowpass.compileModules [moduleCore1, abc9, abc10]

--

abc12 :: Lowpass.Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type)
abc12 = fst (runIdentity (runCompiler2T compiler2TestEnvironment (compileModule Noll.Set11.Test01.moduleMain)))

abc13 :: IO ()
abc13 = Lowpass.testModules =<< Lowpass.compileModules [moduleCore1, abc12]

--

abc14 :: Lowpass.Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type)
abc14 = fst (runIdentity (runCompiler2T compiler2TestEnvironment (compileModule_ Noll.Set12.Test01.moduleUtilities)))

abc15 :: Lowpass.Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type)
abc15 = fst (runIdentity (runCompiler2T compiler2TestEnvironment (compileModule_ Noll.Set12.Test01.moduleMain)))

abc16 :: IO ()
abc16 = Lowpass.testModules =<< Lowpass.compileModules [moduleCore1, abc14, abc15]

--

abc17 :: Lowpass.Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type)
abc17 = fst (runIdentity (runCompiler2T compiler2TestEnvironment (compileModule Noll.Set20.Test01.moduleUtilities)))

abc18 :: Lowpass.Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type)
abc18 = fst (runIdentity (runCompiler2T compiler2TestEnvironment prog))
 where
  prog = do
    insertNamesC
      [
        ( "factorial"
        , Forall mempty [] (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32)
        )
      ,
        ( "unpack_nat"
        , Forall mempty [] (TIntrinsic INat `TArrow` TIntrinsic IInt32)
        )
      ]
    compileModule Noll.Set20.Test01.moduleMain

abc19 :: IO ()
abc19 = Lowpass.testModules =<< Lowpass.compileModules [moduleCore1, abc17, abc18]

abc20 :: [Noll.Module.Module () Kind ()] -> IO ()
abc20 mods = do
  traceShowM r
  ms5 <- Lowpass.compileModules (moduleCore1 : fst r)
  Lowpass.testModules ms5
  pure ()
 where
  r = runIdentity (runCompiler2T compiler2TestEnvironment steps)
  steps = do
    -- TODO: Topological sort
    --
    insertNamesC
      [
        ( "factorial"
        , Forall mempty [] (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32)
        )
      ,
        ( "unpack_nat"
        , Forall mempty [] (TIntrinsic INat `TArrow` TIntrinsic IInt32)
        )
      ]
    ms2 <- forM mods $
      \m -> do
        s <- get
        traceShowM s
        typePass m
    --            let zz = m1 :: Noll.Module.Module () Kind IndexedType
    ms3 <- traverse mainPass ms2
    ms4 <- traverse lowpassTranslationC ms3
    pure ms4

abc21 :: IO ()
abc21 = abc20 Noll.Set20.Test01.prog10_01

--

abc22 :: [String] -> IO ()
abc22 files = do
  ms <- traverse readFile files
  let x = fmap parsing ms
  let r = runIdentity (runCompiler2T compiler2TestEnvironment (steps x))
  ms5 <- Lowpass.compileModules (moduleCore1 : fst r)
  Lowpass.testModules ms5
 where
  steps mods = do
    traceShow mods $ do
      -- TODO: Topological sort
      --
      insertNamesC
        [
          ( "unpack_nat"
          , Forall mempty [] (TIntrinsic INat `TArrow` TIntrinsic IInt32)
          )
        ]
      ms2 <- traverse typePass mods
      ms3 <- traverse mainPass ms2
      traverse lowpassTranslationC ms3
  parsing m =
    case runParser parseModule "" (Text.pack m) of
      Left e ->
        error (show e)
      Right q ->
        q

abc23 =
  abc22
    [ "./test/Noll/Fixtures/01/Utilities.coal"
    , "./test/Noll/Fixtures/01/Main.coal"
    ]

--

abc24 :: Lowpass.Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type)
abc24 = fst (runIdentity (runCompiler2T compiler2TestEnvironment prog))
 where
  prog = do
    insertNamesC
      [
        ( "flatten"
        , Forall (Set.fromList [TypeIndex KType 0]) [] (tree0 `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 0))))
        )
      ,
        ( "from_list"
        , Forall (Set.fromList [TypeIndex KType 0]) [] (TIntrinsic (IList (TVariable (TypeIndex KType 0))) `TArrow` tree0)
        )
      ,
        ( "compare"
        , Forall (Set.fromList [TypeIndex KType 0]) [] (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TConstructor KType "Ordering")
        )
      ]
    compileModule Noll.Set21.Test01.moduleMain

abc25 :: IO ()
abc25 = Lowpass.testModules =<< Lowpass.compileModules [moduleCore1, abc24]

abc26 :: Lowpass.Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type)
abc26 = fst (runIdentity (runCompiler2T compiler2TestEnvironment prog))
 where
  prog = do
    insertNamesC
      [
        ( "flatten"
        , Forall (Set.fromList [TypeIndex KType 0]) [] (tree0 `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 0))))
        )
      ,
        ( "from_list"
        , Forall (Set.fromList [TypeIndex KType 0]) [] (TIntrinsic (IList (TVariable (TypeIndex KType 0))) `TArrow` tree0)
        )
      ,
        ( "compare"
        , Forall (Set.fromList [TypeIndex KType 0]) [] (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TConstructor KType "Ordering")
        )
      ]
    compileModule Noll.Set22.Test01.moduleMain

abc27 :: IO ()
abc27 = Lowpass.testModules =<< Lowpass.compileModules [moduleCore1, abc26]

abc28 :: Lowpass.Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type)
abc28 = fst (runIdentity (runCompiler2T compiler2TestEnvironment prog))
 where
  prog = do
    insertNamesC
      [
        ( "flatten"
        , Forall (Set.fromList [TypeIndex KType 0]) [] (tree0 `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 0))))
        )
      ,
        ( "from_list"
        , Forall (Set.fromList [TypeIndex KType 0]) [] (TIntrinsic (IList (TVariable (TypeIndex KType 0))) `TArrow` tree0)
        )
      ,
        ( "compare"
        , Forall (Set.fromList [TypeIndex KType 0]) [] (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TConstructor KType "Ordering")
        )
      ,
        ( "greater_than"
        , Forall (Set.fromList [TypeIndex KType 0]) [] (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool)
        )
      ,
        ( "less_than_or_equal_to"
        , Forall (Set.fromList [TypeIndex KType 0]) [] (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool)
        )
      ,
        ( "operator__not"
        , Forall mempty [] (TIntrinsic IBool `TArrow` TIntrinsic IBool)
        )
      ,
        ( "in_range"
        , Forall (Set.fromList [TypeIndex KType 0]) [] (TIntrinsic (IRecord (TRow (RExtend "max" (TVariable (TypeIndex KType 0)) (RExtend "min" (TVariable (TypeIndex KType 0)) RNil)))) `TArrow` TVariable (TypeIndex KType 0) `TArrow` TIntrinsic IBool)
        )
      ,
        ( "from_int32"
        , Forall (Set.fromList [TypeIndex KType 0]) [] (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 0))
        )
      ]
    compileModule_ Noll.Set23.Test01.moduleMain

abc29 :: IO ()
abc29 = Lowpass.testModules =<< Lowpass.compileModules [moduleCore1, abc28]

abc30 :: Lowpass.Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type)
abc30 = fst (runIdentity (runCompiler2T compiler2TestEnvironment prog))
 where
  prog = do
    insertNamesC
      [
        ( "operator__not"
        , Forall mempty [] (TIntrinsic IBool `TArrow` TIntrinsic IBool)
        )
      ,
        ( "not"
        , Forall mempty [] (TIntrinsic IBool `TArrow` TIntrinsic IBool)
        )
      ,
        ( "operator__reverse_composition"
        , Forall
            (Set.fromList [TypeIndex KType 0, TypeIndex KType 1, TypeIndex KType 2])
            []
            ( (TVariable (TypeIndex KType 1) `TArrow` TVariable (TypeIndex KType 2))
                `TArrow` (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 1))
                `TArrow` TVariable (TypeIndex KType 0)
                `TArrow` TVariable (TypeIndex KType 2)
            )
        )
      ,
        ( "operator__reverse_application"
        , Forall
            (Set.fromList [TypeIndex KType 0, TypeIndex KType 1])
            []
            ( TVariable (TypeIndex KType 0)
                `TArrow` (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 1))
                `TArrow` TVariable (TypeIndex KType 1)
            )
        )
      ,
        ( "always"
        , Forall
            (Set.fromList [TypeIndex KType 0, TypeIndex KType 1])
            []
            ( TVariable (TypeIndex KType 0)
                `TArrow` TVariable (TypeIndex KType 1)
                `TArrow` TVariable (TypeIndex KType 0)
            )
        )
      ,
        ( "operator__list_concatenation"
        , Forall
            (Set.fromList [TypeIndex KType 0])
            []
            ( TIntrinsic (IList (TVariable (TypeIndex KType 0)))
                `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 0)))
                `TArrow` TIntrinsic (IList (TVariable (TypeIndex KType 0)))
            )
        )
      ,
        ( "trace_int32"
        , Forall
            (Set.fromList [TypeIndex KType 0])
            []
            ( TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 0)
            )
        )
      ,
        ( "trace_string"
        , Forall
            (Set.fromList [TypeIndex KType 0])
            []
            ( TIntrinsic IString `TArrow` TVariable (TypeIndex KType 0)
            )
        )
      ,
        ( "trace_bool"
        , Forall
            (Set.fromList [TypeIndex KType 0])
            []
            ( TIntrinsic IBool `TArrow` TVariable (TypeIndex KType 0)
            )
        )
      ,
        ( "operator__string_concatenation"
        , Forall
            mempty
            []
            (TIntrinsic IString `TArrow` TIntrinsic IString `TArrow` TIntrinsic IString)
        )
      ,
        ( "int32_to_string"
        , Forall
            mempty
            []
            ( TIntrinsic IInt32 `TArrow` TIntrinsic IString
            )
        )
      , --       ( "pair_to_string"
        --        , Forall
        --            (Set.fromList [TypeIndex KType 0])
        --            []
        --            ( undefined
        --                `TArrow` undefined
        --                `TArrow` undefined
        --            )
        --        )
        --      , ( "list_to_string"
        --        , Forall
        --            (Set.fromList [TypeIndex KType 0])
        --            []
        --            ( undefined
        --                `TArrow` undefined
        --                `TArrow` undefined
        --            )
        --        )
        --      , ( "trace"
        --        , Forall
        --            (Set.fromList [TypeIndex KType 0])
        --            []
        --            ( undefined
        --                `TArrow` undefined
        --                `TArrow` undefined
        --            )
        --        )

        ( "unpack_nat"
        , Forall
            mempty
            []
            ( TIntrinsic INat `TArrow` TIntrinsic IInt32
            )
        )
      ,
        ( "pack_nat"
        , Forall
            mempty
            []
            ( TIntrinsic IInt32 `TArrow` TIntrinsic INat
            )
        )
      ,
        ( "from_int32"
        , Forall (Set.fromList [TypeIndex KType 0]) [] (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 0))
        )
      ]
    compileModule_ Noll.Set24.Test01.moduleMain

abc31 :: IO ()
abc31 = Lowpass.testModules =<< Lowpass.compileModules [moduleCore1, abc30]

-- abc25 =
--  abc22
--    [ "./test/Noll/Fixtures/02/Utilities.coal"
--    , "./test/Noll/Fixtures/02/Main.coal"
--    ]

--

-- abc24 :: Lowpass.Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type)
-- abc24 = fst (runIdentity (runCompiler2T compiler2TestEnvironment (compileModule Noll.Set.Test01.moduleUtils2)))

-- abc15 :: Lowpass.Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type)
-- abc15 = fst (runIdentity (runCompiler2T compiler2TestEnvironment (compileModule_ Noll.Set12.Test01.moduleMain)))
--
-- abc16 :: IO ()
-- abc16 = Lowpass.testModules =<< Lowpass.compileModules [moduleCore1, abc14, abc15]

moduleCore1 :: Lowpass.Module Lowpass.Type Name (Lowpass.Expr Lowpass.Type)
moduleCore1 = Lowpass.unsafeParseExpr <$> moduleCore

moduleCore :: Lowpass.Module Lowpass.Type Name Text
moduleCore =
  Lowpass.Module
    { moduleName = "Core$"
    , moduleImports =
        []
    , moduleObjects =
        [ OFunction
            "Core$.operator__not"
            [ Label Lowpass.bool "a"
            ]
            [r| 
                  if (a : bool) then false else true
              |]
        , OFunction
            "Core$.not"
            [ Label Lowpass.bool "a"
            ]
            [r| 
                  if (a : bool) then false else true
              |]
        , OFunction
            "Core$.operator__reverse_composition"
            [ Label (Lowpass.opaque `Lowpass.arrow` Lowpass.opaque) "f"
            , Label (Lowpass.opaque `Lowpass.arrow` Lowpass.opaque) "g"
            , Label Lowpass.opaque "x"
            ]
            [r| 
                  @<*>(f : */*, @<*>(g : */*, x : *))
              |]
        , OFunction
            "Core$.operator__reverse_application"
            [ Label Lowpass.opaque "x"
            , Label (Lowpass.opaque `Lowpass.arrow` Lowpass.opaque) "f"
            ]
            [r| 
                  @<*>(f : */*, x : *)
              |]
        , OFunction
            "Core$.always"
            [ Label Lowpass.opaque "a"
            , Label Lowpass.opaque "_"
            ]
            [r|   
                  a : *
              |]
        , OFunction
            "Core$.operator__list_concatenation"
            [ Label (Lowpass.TCon "list" [Lowpass.opaque]) "xs"
            , Label (Lowpass.TCon "list" [Lowpass.opaque]) "ys"
            ]
            [r| 
                  match<list(*)>(xs : list(*)) {
                    | ( $Cons : */list(*)/list(*)
                      , z : *
                      , zs : list(*)
                      ) =>
                        @<list(*)>
                          ( $Cons : */list(*)/list(*)
                          , z : *
                          , @<list(*)>
                              ( Core$.operator__list_concatenation : list(*)/list(*)/list(*)
                              , zs : list(*)
                              , ys : list(*)
                              )
                          )
                    | ( $Nil : list(*)
                      ) =>
                        ys : list(*)
                  }
              |]
        , OFunction
            "Core$.trace_int32"
            [ Label Lowpass.int32 "n"
            ]
            [r|
                  #(print_int32 : int32/*, n : int32) (fn(a : *) => a : *)
              |]
        , OFunction
            "Core$.trace_string"
            [ Label Lowpass.string "s"
            ]
            [r|
                  #(print_string : string/*, s : string) (fn(a : *) => a : *)
              |]
        , OFunction
            "Core$.trace_bool"
            [ Label Lowpass.string "b"
            ]
            [r|
                  #(print_bool : bool/*, b : bool) (fn(a : *) => a : *)
              |]
        , OFunction
            "Core$.operator__string_concatenation"
            [ Label Lowpass.string "s"
            , Label Lowpass.string "t"
            ]
            [r|
                  #(string_concat : string/string/string, s : string, t : string) (fn(r : string) => r : string)
              |]
        , OFunction
            "Core$.int32_to_string"
            [ Label Lowpass.int32 "n"
            ]
            [r| 
                  #(int32_to_string : int32/string, n : int32) (fn(r : string) => r : string)
              |]
        , OFunction
            "Core$.pair_to_string"
            [ Label (Lowpass.TCon "Traceable" [Lowpass.TOpq]) "$dict1"
            , Label (Lowpass.TCon "Traceable" [Lowpass.TOpq]) "$dict2"
            , Label (Lowpass.TCon "$Tuple2" [Lowpass.TOpq, Lowpass.TOpq]) "p"
            ]
            [r| 
                  match<string>
                    ( p : $Tuple2(*,*) ) { 
                      | ( $Tuple2 : */*/$Tuple2(*,*)
                        , a : *
                        , b : *
                        ) =>
                          @<string>
                            ( Core$.operator__string_concatenation : string/string/string
                            , @<string>
                                ( Core$.operator__string_concatenation : string/string/string
                                , "("
                                , @<string>
                                    ( Core$.operator__string_concatenation : string/string/string
                                    , @<string>
                                        ( Core$.operator__string_concatenation : string/string/string
                                        , @<string>
                                            ( Core$.trace : Traceable(*)/*/string
                                            , $dict1 : Traceable(*)
                                            , a : *
                                            )
                                        , ","
                                        )
                                    , @<string>
                                        ( Core$.trace : Traceable(*)/*/string
                                        , $dict2 : Traceable(*)
                                        , b : *
                                        )
                                    )
                                )
                            , ")"
                            )
                    }
              |]
        , OFunction
            "Core$.list_to_string"
            [ Label (Lowpass.TCon "Traceable" [Lowpass.TOpq]) "$dict1"
            , Label (Lowpass.TCon "list" [Lowpass.TOpq]) "ls"
            ]
            [r| 
                  let
                    f : bool/list(*)/string =
                      fn(first : bool, l : list(*)) =>
                        match<string>
                          ( l : list(*)
                          ) {
                            | ( $Cons : */list(*)/list(*)
                              , x : *
                              , xs : list(*)
                              ) =>
                                @<string>
                                  ( Core$.operator__string_concatenation : string/string/string
                                  , if (first : bool) then "" else ","
                                  , @<string>
                                      ( Core$.operator__string_concatenation : string/string/string
                                      , @<string>
                                          ( Core$.trace : Traceable(*)/*/string
                                          , $dict1 : Traceable(*)
                                          , x : *
                                          )
                                      , @<string>
                                          ( f : list(*)/string
                                          , false
                                          , xs : list(*)
                                          )
                                      )
                                  )
                            | ( $Nil : list(*)
                              ) =>
                                ""
                          }
                    in
                      @<string>
                        ( Core$.operator__string_concatenation : string/string/string
                        , @<string>
                            ( Core$.operator__string_concatenation : string/string/string
                            , "["
                            , @<string>
                                ( f : list(*)/string
                                , true
                                , ls : list(*)
                                )
                            )
                        , "]"
                        )
              |]
        , OFunction
            "Core$.trace"
            [ Label (Lowpass.TCon "Traceable" [opaque]) "$a"
            ]
            [r| 
                  match<*>($a : Traceable(*)) {
                    | ( $Record : { trace : * | * }/Traceable(*)
                      , $r : { trace : * | * }
                      ) =>
                        select
                          { trace = $f : * | _ : * } =
                            $r : { trace : * | * }
                          in
                            $f : *
                  }
              |]
        , OFunction
            "Core$.unpack_nat"
            [ Label (Lowpass.TCon "$Nat" []) "nat"
            ]
            [r| 
                  match<int32>(nat: $Nat) {
                    | ( $Succ : int32/$Nat
                      , succ : int32
                      ) =>
                        [+ int32](succ : int32, 1)
                    | ( $Zero : $Nat
                      ) =>
                        0
                  }
              |]
        , OFunction
            "Core$.pack_nat"
            [ Label Lowpass.int32 "n"
            ]
            [r| 
                  if ([== int32](n : int32, 0))
                    then
                      $Zero : $Nat
                    else
                      @<$Nat>
                        ( $Succ : int32/$Nat
                        , [- int32](n : int32, 1)
                        )
              |]
        , OFunction
            "Core$.from_int32"
            [ Label (Lowpass.TCon "Numeric" [opaque]) "$a"
            ]
            [r| 
                  match<int32/*>($a : Numeric(*)) {
                    | ( $Record : { from_int32 : int32/* | * }/Numeric(*)
                      , $r : { from_int32 : int32/* | * }
                      ) =>
                        select
                          { from_int32 = $f : int32/* | _ : * } =
                            $r : { from_int32 : int32/* | * }
                          in
                            $f : int32/*
                  }
              |]
        , OFunction
            "Core$.from_int32__$instance_Numeric(Intrinsic(Int32))"
            [ Label Lowpass.int32 "n"
            ]
            [r| 
                  n : int32
              |]
        , OFunction
            "Core$.from_int32__$instance_Numeric(Intrinsic(Nat))"
            [ Label Lowpass.int32 "n"
            ]
            [r| 
                  @<$Nat>
                    ( Core$.pack_nat : int32/$Nat
                    , n : int32
                    )
              |]
        ]
    }
