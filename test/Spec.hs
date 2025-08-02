{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

import Coal.Ast.Metadata (Metadata (..))
import Coal.Common.Environment (Environment (..))
import Coal.Common.Label (Label (..))
import Coal.Common.List1 (NonEmpty (..))
import Coal.Common.Name (Dictionary, Name)
import Coal.Compiler (kernelTranslationC, mainPass, typeCheckingPass)
import Coal.Compiler.Environment
import Coal.Compiler.Stack
import Coal.Compiler.TypeInference.Errors
import Coal.Kernel.Compiler (compileModules)
import Coal.Kernel.LLVM.IRConstruct (IRConstruct (..))
import Coal.Kernel.LLVM.IREncodable (irEncode)
import Coal.Kernel.LLVM.IRInterpreter.Monad
import Coal.Kernel.Language (Object (..), moduleImports, moduleName, moduleObjects, opaque)
import Coal.Kernel.Parser (spaces)
import Coal.Kernel.Parser.Expr (expr)
import Coal.Kernel.Parser.Module (module_)
import Coal.Language
import Coal.Language.Module
import Coal.Parser (ParserError)
import Coal.Parser.Module
import Coal.TypeSystem.Substitution
import Control.Monad (forM, forM_, void)
import Control.Monad.Except
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (ask, local)
import Control.Monad.State (gets, liftIO)
import Data.Data (Data)
import Data.Either
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Text (Text)
import Data.Void (Void)
import Debug.Trace
import Extra (Name, isConstructor, (<$$>))
import System.IO.Unsafe (unsafePerformIO)
import System.Process
import Text.Megaparsec (eof, errorBundlePretty, runParser)
import Text.RawString.QQ

import qualified Coal.Common.Environment as Environment
import qualified Coal.Kernel.Compiler as Kernel
import qualified Coal.Kernel.Language as Kernel
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.Text.IO as Text

spec :: IO ()
spec = do
  a <- isLeft <$> main
  print a
  x <- main2
  print x
  x <- main3
  print x
  x <- main4
  print x
  x <- main5
  print x
  x <- main6
  print x
  x <- main7
  print x
  x <- main8
  print x
  x <- main9
  print x
  x <- main10
  print x
  x <- main11
  print x
  x <- main12
  print x
  a <- isLeft <$> main13
  print a
  a <- isLeft <$> main14
  print a
  a <- isLeft <$> main15
  print a
  a <- isLeft <$> main16
  print a
  x <- main17
  print x
  x <- main18
  print x

runTestFiles :: [String] -> IO (Either CompilerError Text)
runTestFiles files = do
  r <- compileFiles files
  case r of
    Left err@(CompilerError msg) -> do
      liftIO $ Text.putStrLn msg
      pure (Left err)
    Right{} ->
      Right <$> runTestBuild

main :: IO (Either CompilerError Text)
main = do
  runTestFiles
    [ "./test/Coal/examples/01/Main.coal"
    ]

main2 :: IO (Either CompilerError Text)
main2 = do
  runTestFiles
    [ "./test/Coal/examples/02/Main.coal"
    ]

main3 :: IO (Either CompilerError Text)
main3 = do
  runTestFiles
    [ "./test/Coal/examples/03/Main.coal"
    ]

main4 :: IO (Either CompilerError Text)
main4 = do
  runTestFiles
    [ "./test/Coal/examples/04/Main.coal"
    ]

main5 :: IO (Either CompilerError Text)
main5 = do
  runTestFiles
    [ "./test/Coal/examples/05/Math.coal"
    , "./test/Coal/examples/05/Main.coal"
    ]

main6 :: IO (Either CompilerError Text)
main6 = do
  runTestFiles
    [ "./test/Coal/examples/06/Tree.coal"
    , "./test/Coal/examples/06/Qsort.coal"
    , "./test/Coal/examples/06/Main.coal"
    ]

main7 :: IO (Either CompilerError Text)
main7 = do
  runTestFiles
    [ "./test/Coal/examples/07/Main.coal"
    ]

main8 :: IO (Either CompilerError Text)
main8 = do
  runTestFiles
    [ "./test/Coal/examples/08/Main.coal"
    ]

main9 :: IO (Either CompilerError Text)
main9 = do
  runTestFiles
    [ "./test/Coal/examples/09/Main.coal"
    ]

main10 :: IO (Either CompilerError Text)
main10 = do
  runTestFiles
    [ "./test/Coal/examples/10/Main.coal"
    ]

main11 :: IO (Either CompilerError Text)
main11 = do
  runTestFiles
    [ "./test/Coal/examples/11/Main.coal"
    ]

main12 :: IO (Either CompilerError Text)
main12 = do
  runTestFiles
    [ "./test/Coal/examples/12/Main.coal"
    ]

main13 :: IO (Either CompilerError Text)
main13 = do
  runTestFiles
    [ "./test/Coal/examples/13/Main.coal"
    ]

main14 :: IO (Either CompilerError Text)
main14 = do
  runTestFiles
    [ "./test/Coal/examples/14/Main.coal"
    ]

main15 :: IO (Either CompilerError Text)
main15 = do
  runTestFiles
    [ "./test/Coal/examples/15/Main.coal"
    ]

main16 :: IO (Either CompilerError Text)
main16 = do
  runTestFiles
    [ "./test/Coal/examples/16/Main.coal"
    ]

main17 :: IO (Either CompilerError Text)
main17 = do
  runTestFiles
    [ "./test/Coal/examples/17/Main.coal"
    ]

main18 :: IO (Either CompilerError Text)
main18 = do
  runTestFiles
    [ "./test/Coal/examples/18/Main.coal"
    ]

-- main19 :: IO (Either CompilerError Text)
-- main19 = do
--  runTestFiles
--    [ "./test/Coal/examples/19/Main.coal"
--    ]

main20 :: IO (Either CompilerError Text)
main20 = do
  runTestFiles
    [ "./test/Coal/examples/20/Main.coal"
    ]

compileFiles :: [String] -> IO (Either CompilerError ())
compileFiles files = do
  fs <- traverse readFile files
  let results = fmap (parseFile . Text.pack) fs
  case partitionEithers results of
    ([], objs) -> do
      evalCompilerT emptyCompilerEnvironment (run objs)
    (es, _) -> do
      forM_ es $
        \e ->
          putStrLn (errorBundlePretty e)
      pure (Right ())

withLocalEnvironment :: (Monad m) => [Definition Metadata Kind ()] -> CompilerT Metadata m a -> CompilerT Metadata m a
withLocalEnvironment = local . const . (insertBuiltinConstructors . buildEnvironment)

insertBuiltinConstructors :: CompilerEnvironment -> CompilerEnvironment
insertBuiltinConstructors CompilerEnvironment{..} =
  CompilerEnvironment
    { compilerDataConstructorEnvironment = Environment.insertMultiple builtinDataConstructors compilerDataConstructorEnvironment
    , compilerTraitEnvironment = Environment.insertMultiple builtinTraits compilerTraitEnvironment
    , compilerInstanceEnvironment = Environment.insertMultiple builtinInstances compilerInstanceEnvironment
    , compilerTypeConstructorEnvironment = Environment.insertMultiple builtinTypeConstructors compilerTypeConstructorEnvironment
    , ..
    }

builtinTraits :: [(Name, (TypeIndex Kind, Environment IndexedScheme))]
builtinTraits =
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

builtinInstances :: [(Name, Map IndexedType (Dictionary IndexedScheme))]
builtinInstances =
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
                , Forall mempty [] (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TConstructor KType "Ordering")
                )
              ]
          )
        ]
    )
  ]

builtinTypeConstructors :: [(Name, Kind)]
builtinTypeConstructors =
  []

builtinDataConstructors :: [(Name, Constructor TypeIndex Kind IndexedType)]
builtinDataConstructors =
  [
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
  ]

addBuiltinDefs :: [Definition a k t] -> [Definition a k t]
addBuiltinDefs defs =
  [ DType
      "Ordering"
      []
      [ Constructor "LessThan" 0 (Forall mempty [] (TConstructor () "Ordering"))
      , Constructor "GreaterThan" 0 (Forall mempty [] (TConstructor () "Ordering"))
      , Constructor "EqualTo" 0 (Forall mempty [] (TConstructor () "Ordering"))
      ]
  , DImport
      (Path ["Core$"])
      [ "operator__not"
      , "not"
      , "operator__reverse_composition"
      , "operator__reverse_application"
      , "always"
      , "operator__list_concatenation"
      , "trace_int32"
      , "trace_string"
      , "trace_bool"
      , "operator__string_concatenation"
      , "int32_to_string"
      , "pair_to_string"
      , "list_to_string"
      , "trace"
      , "unpack_nat"
      , "pack_nat"
      , "from_int32"
      , "compare"
      , "from_int32__$instance_Numeric(Intrinsic(Int32))"
      , "from_int32__$instance_Numeric(Intrinsic(Nat))"
      , "compare__$instance_Ordered(Intrinsic(Int32))"
      ]
  ]
    <> defs

insertImportedTypes :: Environment [Definition a Kind ()] -> [Definition a Kind ()] -> [Definition a Kind ()]
insertImportedTypes env defs = concatMap go defs <> defs
 where
  go =
    \case
      DImport (Path path) ns -> do
        [t | t@(DType c _ _) <- ds, c `elem` filter isConstructor ns]
       where
        ds = fromMaybe mempty (Environment.lookup (Text.intercalate "." path) env)
      _ ->
        []

run :: [(Text, Module Metadata Kind ())] -> CompilerT Metadata IO ()
run modules = do
  rs <- forM (overModuleDefinitions addBuiltinDefs <$$> modules) $
    \(src, m1) -> do
      defs <- gets compilerTypeDefinitions
      let m2 = overModuleDefinitions (insertImportedTypes defs) m1
      setSourceTextC src
      insertNamesC names
      case m2 of
        Module (Path path) _ defs -> do
          insertTypeDefinitionsC (Text.intercalate "." path) [t | t@(DType c _ _) <- defs]
          withLocalEnvironment defs (compileModule m2)
  liftIO $ do
    ms <- Kernel.compileModules (moduleCore1 : rs)
    generateLLOutput ms

compileModule :: (MonadIO m) => Module Metadata Kind () -> CompilerT Metadata m (Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type))
compileModule x = do
  a <- typeCheckingPass x

  x2 <- gets compilerConstraintsGenErrors

  case nub x2 of
    errs@(_ : _) ->
      forM_ errs $
        \err -> do
          src <- gets compilerSourceText
          let msg = prettyErrorMessage [Text.pack (show err)] src err
          throwError (CompilerError msg)
    [] ->
      pure ()

  x3 <- gets compilerSolverRuleViolations

  case nub x3 of
    errs@(_ : _) ->
      forM_ errs $
        \err -> do
          src <- gets compilerSourceText
          let msg =
                prettyErrorMessage
                  [ "\nType error:"
                  , Text.pack (show err)
                  ]
                  src
                  err
          throwError (CompilerError msg)
    [] ->
      pure ()

  b <- mainPass a

  cc <- gets compilerAssumptions
  liftIO (print cc)

  kernelTranslationC b

parseFile :: Text -> Either ParserError (Text, Module Metadata o ())
parseFile src = do
  m <- runParser parseModule "" src
  pure (src, m)

names :: [(Name, IndexedScheme)]
names =
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
  ,
    ( "compare"
    , Forall
        (Set.fromList [TypeIndex KType 0] :: Set (TypeIndex Kind))
        [Trait "Ordered" (TVariable (TypeIndex KType 0))]
        ( TVariable (TypeIndex KType 0)
            `TArrow` TVariable (TypeIndex KType 0)
            `TArrow` TConstructor KType "Ordering"
        )
    )
  ]

moduleCore1 :: Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type)
moduleCore1 = unsafeParseKernelExpr <$> moduleCore

moduleCore :: Kernel.Module Kernel.Type Name Text
moduleCore =
  Kernel.Module
    { moduleName = "Core$"
    , moduleImports =
        []
    , moduleObjects =
        [ OData "EqualTo" 0 (Kernel.TCon "Ordering" [])
        , OData "GreaterThan" 1 (Kernel.TCon "Ordering" [])
        , OData "LessThan" 2 (Kernel.TCon "Ordering" [])
        , OFunction
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
        , OFunction
            "Core$.compare"
            [ Label (Kernel.TCon "Ordered" [opaque]) "$a"
            ]
            [r| 
                  match<*/*/Ordering>($a : Ordered(*)) {
                    | ( $Record : { compare : */*/Ordering | * }/Ordered(*)
                      , $r : { compare : */*/Ordering | * }
                      ) =>
                        select
                          { compare = $f : */*/Ordering | _ : * } =
                            $r : { compare : */*/Ordering | * }
                          in
                            $f : */*/Ordering
                  }
              |]
        , OFunction
            "Core$.compare__$instance_Ordered(Intrinsic(Int32))"
            [ Label Kernel.int32 "x"
            , Label Kernel.int32 "y"
            ]
            [r| 
                  if ([< int32](x : int32, y : int32))
                    then
                      LessThan : Ordering
                    else
                      if ([> int32](x : int32, y : int32))
                        then
                          GreaterThan : Ordering
                        else
                          EqualTo : Ordering
              |]
        ]
    }

unsafeParseKernelModule :: Text -> Kernel.Module Kernel.Type Name (Kernel.Expr Kernel.Type)
unsafeParseKernelModule t =
  case runParser (spaces *> module_ <* eof) "" t of
    Left e ->
      error (errorBundlePretty e)
    Right r ->
      r

unsafeParseKernelExpr :: Text -> Kernel.Expr Kernel.Type
unsafeParseKernelExpr t =
  case runParser expr "" (Text.stripStart t) of
    Left e ->
      error (errorBundlePretty e)
    Right r ->
      r

generateLLOutput :: [(Name, [IRConstruct [IRLine]])] -> IO ()
generateLLOutput mods = do
  forM_ mods $
    \(name, code) -> do
      let out = irEncode code
      Text.writeFile ("./.build/" <> Text.unpack name <> ".ll") out
  Text.writeFile "./.build/build.sh" (buildScript (fst <$> mods))

buildScript :: [Text] -> Text
buildScript modules =
  Text.unlines
    ( [ "#!/bin/bash"
      , "cd \"$(dirname \"$0\")\" || exit 1"
      ]
        <> [ llcCmd name | name <- modules
           ]
        <> [ "gcc -g -I./ -lgc -lgmp ../runtime/lib.c " <> Text.concat [name <> ".o " | name <- modules] <> "-o dist"
           ]
    )
 where
  llcCmd name =
    "llc -filetype=obj "
      <> name
      <> ".ll -o "
      <> name
      <> ".o"

runTestBuild :: IO Text
runTestBuild = do
  void (readProcess "./.build/build.sh" [] "")
  Text.pack <$> readProcess "./.build/dist" [] ""
