{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Coal.CompilerSpec where

import Control.Monad ((>=>))
import Control.Monad.Identity (runIdentity)
import Control.Monad.Reader (local)
import Control.Monad.State (get)
import Data.Map.Strict (Map)
import Data.Set (Set)
import Data.Text (Text)
import Debug.Trace
import Coal.Common.Environment (Environment)
import Coal.Common.List1 (NonEmpty (..), (<|))
import Coal.Common.Label (Label (..))
import Coal.Kernel.Language (Object (..), moduleImports, moduleName, moduleObjects, opaque)
import Extra (Dictionary, Name, forM, forM_)
import Coal.Compiler.Transform.Dictionaries
import Coal.Compiler
import Coal.Compiler.Environment
import Coal.Compiler.Stack
import Coal.Language.Trait
import Coal.Parser.Module
import Text.Megaparsec (runParser)
import Text.RawString.QQ

-- import Coal.CompilerExamples.Test02 (bazz)

import Coal.Language (Constructor (..), IndexedType (..), Intrinsic (..), Kind (..), Parameter (..), Row (..), Scheme (..), Type (..), TypeIndex (..))
import Coal.Language.Module (Constant (..), Definition (..), Function (..), Module (..))
import Coal.TypeSystem
import Coal.TypeSystemSpec.TestRunner
import Test.Hspec (Spec, describe, it)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Coal.Common.Environment as Environment
import qualified Coal.Kernel.Compiler as Kernel
import qualified Coal.Kernel.Compiler.Utils as Kernel
import qualified Coal.Kernel.Language as Kernel
import qualified Coal.Set.Test01
import qualified Coal.Set10.Test01
import qualified Coal.Set11.Test01
import qualified Coal.Set12.Test01
import qualified Coal.Set20.Test01
import qualified Coal.Set21.Test01
import qualified Coal.Set22.Test01
import qualified Coal.Set23.Test01
import qualified Coal.Set24.Test01
import qualified Coal.Set7.Test01
import qualified Coal.Set8.Test01
import qualified Coal.Set9.Test01

import qualified Data.Text as Text

spec :: Spec
spec =
  describe "Coal.Compiler" $ do
    it "" $ do
      1 == 2

compilerTestEnvironment =
  CompilerEnvironment
    { compilerDataConstructorEnvironment = env1
    , compilerTypeConstructorEnvironment = env2
    , compilerTraitEnvironment = env3
    , compilerAliasEnvironment = env5
    , compilerInstanceEnvironment = env6
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

-- env4 =
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

env6 :: Environment (Map IndexedType (Dictionary (Scheme TypeIndex Kind IndexedType)))
env6 =
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
                  , Forall mempty [] (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TConstructor KType "Ordering")
                  )
                ]
            )
          ]
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
-- abc1 = fst (runIdentity (runCompilerT compilerTestEnvironment ((typePass >=> mainPass) Coal.Set7.Test01.moduleMain)))

abc2 :: Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type)
abc2 = fst (runIdentity (runCompilerT compilerTestEnvironment (compileModule Coal.Set7.Test01.moduleMain)))

abc3 :: IO ()
abc3 = Kernel.testModules =<< Kernel.compileModules [moduleCore1, abc2]

--

abc4 :: Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type)
abc4 = fst (runIdentity (runCompilerT compilerTestEnvironment (compileModule Coal.Set8.Test01.moduleMain)))

abc5 :: IO ()
abc5 = Kernel.testModules =<< Kernel.compileModules [moduleCore1, abc4]

--

abc6 :: Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type)
abc6 = fst (runIdentity (runCompilerT compilerTestEnvironment (compileModule Coal.Set9.Test01.moduleUtilities)))

abc7 :: Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type)
abc7 = fst (runIdentity (runCompilerT compilerTestEnvironment (compileModule Coal.Set9.Test01.moduleMain)))

abc8 :: IO ()
abc8 = Kernel.testModules =<< Kernel.compileModules [moduleCore1, abc6, abc7]

--

abc9 :: Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type)
abc9 = fst (runIdentity (runCompilerT compilerTestEnvironment (compileModule Coal.Set10.Test01.moduleUtilities)))

abc10 :: Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type)
abc10 = fst (runIdentity (runCompilerT compilerTestEnvironment (compileModule Coal.Set10.Test01.moduleMain)))

abc11 :: IO ()
abc11 = Kernel.testModules =<< Kernel.compileModules [moduleCore1, abc9, abc10]

--

abc12 :: Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type)
abc12 = fst (runIdentity (runCompilerT compilerTestEnvironment (compileModule Coal.Set11.Test01.moduleMain)))

abc13 :: IO ()
abc13 = Kernel.testModules =<< Kernel.compileModules [moduleCore1, abc12]

--

abc14 :: Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type)
abc14 = fst (runIdentity (runCompilerT compilerTestEnvironment (compileModule_ Coal.Set12.Test01.moduleUtilities)))

abc15 :: Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type)
abc15 = fst (runIdentity (runCompilerT compilerTestEnvironment (compileModule_ Coal.Set12.Test01.moduleMain)))

abc16 :: IO ()
abc16 = Kernel.testModules =<< Kernel.compileModules [moduleCore1, abc14, abc15]

--

abc17 :: Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type)
abc17 = fst (runIdentity (runCompilerT compilerTestEnvironment (compileModule Coal.Set20.Test01.moduleUtilities)))

abc18 :: Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type)
abc18 = fst (runIdentity (runCompilerT compilerTestEnvironment prog))
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
    compileModule Coal.Set20.Test01.moduleMain

abc19 :: IO ()
abc19 = Kernel.testModules =<< Kernel.compileModules [moduleCore1, abc17, abc18]

abc20 :: [Coal.Language.Module.Module () Kind ()] -> IO ()
abc20 mods = do
  traceShowM r
  ms5 <- Kernel.compileModules (moduleCore1 : fst r)
  Kernel.testModules ms5
  pure ()
 where
  r = runIdentity (runCompilerT compilerTestEnvironment steps)
  steps = do
    -- TODO: Topological sort
    --
    insertNamesC
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
      ,
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
        , Forall
            (Set.fromList [TypeIndex KType 0] :: Set (TypeIndex Kind))
            [Trait "Numeric" (TVariable (TypeIndex KType 0))]
            (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 0))
        )
      ]

    ms2 <- forM mods $
      \m -> do
        s <- get
        --        traceShowM s
        typePass m
    --            let zz = m1 :: Coal.Language.Module.Module () Kind IndexedType
    ms3 <- traverse mainPass ms2
    ms4 <- traverse kernelTranslationC ms3
    pure ms4

abc21 :: IO ()
abc21 = abc20 Coal.Set20.Test01.prog10_01

--

abc22 :: [String] -> IO ()
abc22 files = do
  ms <- traverse readFile files
  let x = fmap parsing ms
  let r = runIdentity (runCompilerT compilerTestEnvironment (steps x))
  ms5 <- Kernel.compileModules (moduleCore1 : fst r)
  Kernel.testModules ms5
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
      traverse kernelTranslationC ms3
  parsing m =
    case runParser parseModule "" (Text.pack m) of
      Left e ->
        error (show e)
      Right q ->
        q

abc23 =
  abc22
    [ "./test/Coal/Fixtures/01/Utilities.coal"
    , "./test/Coal/Fixtures/01/Main.coal"
    ]

--

abc24 :: Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type)
abc24 = fst (runIdentity (runCompilerT compilerTestEnvironment prog))
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
    compileModule Coal.Set21.Test01.moduleMain

abc25 :: IO ()
abc25 = Kernel.testModules =<< Kernel.compileModules [moduleCore1, abc24]

abc26 :: Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type)
abc26 = fst (runIdentity (runCompilerT compilerTestEnvironment prog))
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
    compileModule Coal.Set22.Test01.moduleMain

abc27 :: IO ()
abc27 = Kernel.testModules =<< Kernel.compileModules [moduleCore1, abc26]

abc28 :: Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type)
abc28 = fst (runIdentity (runCompilerT compilerTestEnvironment prog))
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
    compileModule_ Coal.Set23.Test01.moduleMain

abc29 :: IO ()
abc29 = Kernel.testModules =<< Kernel.compileModules [moduleCore1, abc28]

abc30 :: Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type)
abc30 = fst (runIdentity (runCompilerT compilerTestEnvironment prog))
 where
  prog = do
    insertNamesC
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
      ,
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
        , Forall
            (Set.fromList [TypeIndex KType 0] :: Set (TypeIndex Kind))
            [Trait "Numeric" (TVariable (TypeIndex KType 0))]
            (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 0))
        )
      ]
    compileModule_ Coal.Set24.Test01.moduleMain

abc31 :: IO ()
abc31 = Kernel.testModules =<< Kernel.compileModules [moduleCore1, abc30]

abc32 :: [String] -> IO ()
abc32 files = do
  ms <- traverse readFile files
  let x = fmap parsing ms
  let r = runIdentity (runCompilerT emptyCompilerEnvironment (steps x))
  ms5 <- Kernel.compileModules (moduleCore1 : fst r)
  Kernel.testModules ms5
 where
  --  env = buildEnvironment
  --      CompilerEnvironment
  --        { compilerDataConstructorEnvironment = env1
  --        , compilerTypeConstructorEnvironment = env2
  --        , compilerTraitEnvironment = env3
  --        , compilerAliasEnvironment = env5
  --        , compilerInstanceEnvironment = env6
  --        }
  steps mods = do
    traceShow mods $ do
      -- TODO: Topological sort
      --
      forM mods $
        \mod@(Module _ _ dfs) -> do
          -- ms2 <- traverse typePass mods
          insertNamesC
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
            ,
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
              , Forall
                  (Set.fromList [TypeIndex KType 0] :: Set (TypeIndex Kind))
                  [Trait "Numeric" (TVariable (TypeIndex KType 0))]
                  (TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 0))
              )
            ]
          local (\_ -> buildEnvironment dfs) (compileModule_ mod)
  parsing m =
    case runParser parseModule "" (Text.pack m) of
      Left e ->
        error (show e)
      Right q ->
        q

abc33 =
  abc32
    [ "./test/Coal/Fixtures/03/Main.coal"
    ]

-- abc25 =
--  abc22
--    [ "./test/Coal/Fixtures/02/Utilities.coal"
--    , "./test/Coal/Fixtures/02/Main.coal"
--    ]

--

-- abc24 :: Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type)
-- abc24 = fst (runIdentity (runCompilerT compilerTestEnvironment (compileModule Coal.Set.Test01.moduleUtils2)))

-- abc15 :: Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type)
-- abc15 = fst (runIdentity (runCompilerT compilerTestEnvironment (compileModule_ Coal.Set12.Test01.moduleMain)))
--
-- abc16 :: IO ()
-- abc16 = Kernel.testModules =<< Kernel.compileModules [moduleCore1, abc14, abc15]

moduleCore1 :: Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type)
moduleCore1 = Kernel.unsafeParseExpr <$> moduleCore

moduleCore :: Kernel.Module Kernel.Type Name Text
moduleCore =
  Kernel.Module
    { moduleName = "Core$"
    , moduleImports =
        []
    , moduleObjects =
        [ OFunction
            "Core$.operator__not"
            [ Label Kernel.bool "a"
            ]
            [r| 
                  if (a : bool) then false else true
              |]
        , OFunction
            "Core$.not"
            [ Label Kernel.bool "a"
            ]
            [r| 
                  if (a : bool) then false else true
              |]
        , OFunction
            "Core$.operator__reverse_composition"
            [ Label (Kernel.opaque `Kernel.arrow` Kernel.opaque) "f"
            , Label (Kernel.opaque `Kernel.arrow` Kernel.opaque) "g"
            , Label Kernel.opaque "x"
            ]
            [r| 
                  @<*>(f : */*, @<*>(g : */*, x : *))
              |]
        , OFunction
            "Core$.operator__reverse_application"
            [ Label Kernel.opaque "x"
            , Label (Kernel.opaque `Kernel.arrow` Kernel.opaque) "f"
            ]
            [r| 
                  @<*>(f : */*, x : *)
              |]
        , OFunction
            "Core$.always"
            [ Label Kernel.opaque "a"
            , Label Kernel.opaque "_"
            ]
            [r|   
                  a : *
              |]
        , OFunction
            "Core$.operator__list_concatenation"
            [ Label (Kernel.TCon "list" [Kernel.opaque]) "xs"
            , Label (Kernel.TCon "list" [Kernel.opaque]) "ys"
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
            [ Label Kernel.int32 "n"
            ]
            [r|
                  #(print_int32 : int32/*, n : int32) (fn(a : *) => a : *)
              |]
        , OFunction
            "Core$.trace_string"
            [ Label Kernel.string "s"
            ]
            [r|
                  #(print_string : string/*, s : string) (fn(a : *) => a : *)
              |]
        , OFunction
            "Core$.trace_bool"
            [ Label Kernel.string "b"
            ]
            [r|
                  #(print_bool : bool/*, b : bool) (fn(a : *) => a : *)
              |]
        , OFunction
            "Core$.operator__string_concatenation"
            [ Label Kernel.string "s"
            , Label Kernel.string "t"
            ]
            [r|
                  #(string_concat : string/string/string, s : string, t : string) (fn(r : string) => r : string)
              |]
        , OFunction
            "Core$.int32_to_string"
            [ Label Kernel.int32 "n"
            ]
            [r| 
                  #(int32_to_string : int32/string, n : int32) (fn(r : string) => r : string)
              |]
        , OFunction
            "Core$.pair_to_string"
            [ Label (Kernel.TCon "Traceable" [Kernel.TOpq]) "$dict1"
            , Label (Kernel.TCon "Traceable" [Kernel.TOpq]) "$dict2"
            , Label (Kernel.TCon "$Tuple2" [Kernel.TOpq, Kernel.TOpq]) "p"
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
            [ Label (Kernel.TCon "Traceable" [Kernel.TOpq]) "$dict1"
            , Label (Kernel.TCon "list" [Kernel.TOpq]) "ls"
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
            [ Label (Kernel.TCon "Traceable" [opaque]) "$a"
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
            [ Label (Kernel.TCon "$Nat" []) "nat"
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
            [ Label Kernel.int32 "n"
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
            [ Label (Kernel.TCon "Numeric" [opaque]) "$a"
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
            [ Label Kernel.int32 "n"
            ]
            [r| 
                  n : int32
              |]
        , OFunction
            "Core$.from_int32__$instance_Numeric(Intrinsic(Nat))"
            [ Label Kernel.int32 "n"
            ]
            [r| 
                  @<$Nat>
                    ( Core$.pack_nat : int32/$Nat
                    , n : int32
                    )
              |]
        ]
    }
