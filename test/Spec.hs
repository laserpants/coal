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
import Coal.Dotgen.ToDot (writeDotFiles, writeDotFilesK)
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
import Coal.TypeSystem.Constraint.Assumption (Assumption (..))
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
  print (x == Right "24\n")
  x <- main3
  print (x == Right "1\n")
  x <- main4
  print (x == Right "2\n")
  x <- main5
  print (x == Right "40320\n")
  x <- main6
  print (x == Right "101\n")
  x <- main7
  print (x == Right "hello\n")
  x <- main8
  print (x == Right "cluedo\n")
  x <- main9
  print (x == Right "wat\n")
  x <- main10
  print ("10", x == Right "hello from the other side\n")
  x <- main11
  print ("11", x == Right "Covfefe\n")
  x <- main12
  print (x == Right "bork bork bork\n")
  a <- isLeft <$> main13
  print a
  a <- isLeft <$> main14
  print a
  a <- isLeft <$> main15
  print a
  a <- isLeft <$> main16
  print a
  x <- main17
  print (x == Right "false\n")
  x <- main18
  print ("18", x == Right "40320\n")
  x <- main20
  print (x == Right "Bob\n")
  x <- main21
  print (x == Right "Lazarus\n")
  x <- main22
  print (x == Right "Alphonso\n")
  x <- main24
  print (x == Right "1234\n")
  x <- main25
  print (x == Right "123\n")
  x <- main26
  print (x == Right "x\n")
  a <- isLeft <$> main27
  print a
  x <- main28
  print (x == Right "5\n")
  x <- main29
  print ("29", x == Right "ananab\n")
  x <- main30
  print (x == Right "2\n")
  x <- main31
  print (x == Right "5\n")
  x <- main32
  print (x == Right "123\n")
  x <- main34
  print (x == Right "111\n111\n")
  x <- main35
  print ("35", x == Right "9876\n")
  x <- main36
  print (x == Right "-123\n")
  x <- main37
  print (x == Right "59876\n")
  x <- main39
  print (x == Right "true\n")
  x <- main40
  print (x == Right "true\n")
  x <- main41
  print (x == Right "true\n")
  x <- main42
  print (x == Right "true\n")
  x <- main43
  print (x == Right "2\n")
  x <- main45
  print (x == Right "6\n")
  x <- main46
  print (x == Right "512\n")
  x <- main47
  print (x == Right "8\n")
  x <- main49
  print (x == Right "1\n")
  x <- main50
  print (x == Right "5\n")
  x <- main51
  print (x == Right "7\n")
  x <- main52
  print (x == Right "105\n")
  x <- main53
  print (x == Right "1\n")
  x <- main54
  print (x == Right "1000\n")
  x <- main56
  print (x == Right "false\n")
  x <- main57
  print (x == Right "1\n")

runTestFiles :: [String] -> IO (Either CompilerError Text)
runTestFiles files = do
  r <- compileFiles files
  case r of
    Left err@(CompilerError msg) -> do
      -- liftIO $ Text.putStrLn msg
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

main21 :: IO (Either CompilerError Text)
main21 = do
  runTestFiles
    [ "./test/Coal/examples/21/Main.coal"
    ]

main22 :: IO (Either CompilerError Text)
main22 = do
  runTestFiles
    [ "./test/Coal/examples/22/Main.coal"
    ]

main23 :: IO (Either CompilerError Text)
main23 = do
  runTestFiles
    [ "./test/Coal/examples/23/Main.coal"
    ]

main24 :: IO (Either CompilerError Text)
main24 = do
  runTestFiles
    [ "./test/Coal/examples/24/Main.coal"
    ]

main25 :: IO (Either CompilerError Text)
main25 = do
  runTestFiles
    [ "./test/Coal/examples/25/Main.coal"
    ]

main26 :: IO (Either CompilerError Text)
main26 = do
  runTestFiles
    [ "./test/Coal/examples/26/Main.coal"
    ]

main27 :: IO (Either CompilerError Text)
main27 = do
  runTestFiles
    [ "./test/Coal/examples/27/Main.coal"
    ]

main28 :: IO (Either CompilerError Text)
main28 = do
  runTestFiles
    [ "./test/Coal/examples/28/Main.coal"
    ]

main29 :: IO (Either CompilerError Text)
main29 = do
  runTestFiles
    [ "./test/Coal/examples/29/Main.coal"
    ]

main30 :: IO (Either CompilerError Text)
main30 = do
  runTestFiles
    [ "./test/Coal/examples/30/Main.coal"
    ]

main31 :: IO (Either CompilerError Text)
main31 = do
  runTestFiles
    [ "./test/Coal/examples/31/Main.coal"
    ]

main32 :: IO (Either CompilerError Text)
main32 = do
  runTestFiles
    [ "./test/Coal/examples/32/Main.coal"
    ]

main33 :: IO (Either CompilerError Text)
main33 = do
  runTestFiles
    [ "./test/Coal/examples/33/Main.coal"
    ]

main34 :: IO (Either CompilerError Text)
main34 = do
  runTestFiles
    [ "./test/Coal/examples/34/Main.coal"
    ]

main35 :: IO (Either CompilerError Text)
main35 = do
  runTestFiles
    [ "./test/Coal/examples/35/Main.coal"
    ]

main36 :: IO (Either CompilerError Text)
main36 = do
  runTestFiles
    [ "./test/Coal/examples/36/Main.coal"
    ]

main37 :: IO (Either CompilerError Text)
main37 = do
  runTestFiles
    [ "./test/Coal/examples/37/Main.coal"
    ]

main38 :: IO (Either CompilerError Text)
main38 = do
  runTestFiles
    [ "./test/Coal/examples/38/Main.coal"
    ]

main39 :: IO (Either CompilerError Text)
main39 = do
  runTestFiles
    [ "./test/Coal/examples/39/Main.coal"
    ]

main40 :: IO (Either CompilerError Text)
main40 = do
  runTestFiles
    [ "./test/Coal/examples/40/Main.coal"
    ]

main41 :: IO (Either CompilerError Text)
main41 = do
  runTestFiles
    [ "./test/Coal/examples/41/Main.coal"
    ]

main42 :: IO (Either CompilerError Text)
main42 = do
  runTestFiles
    [ "./test/Coal/examples/42/Main.coal"
    ]

main43 :: IO (Either CompilerError Text)
main43 = do
  runTestFiles
    [ "./test/Coal/examples/43/Main.coal"
    ]

main44 :: IO (Either CompilerError Text)
main44 = do
  runTestFiles
    [ "./test/Coal/examples/44/Main.coal"
    ]

main45 :: IO (Either CompilerError Text)
main45 = do
  runTestFiles
    [ "./test/Coal/examples/45/Main.coal"
    ]

main46 :: IO (Either CompilerError Text)
main46 = do
  runTestFiles
    [ "./test/Coal/examples/46/Main.coal"
    ]

main47 :: IO (Either CompilerError Text)
main47 = do
  runTestFiles
    [ "./test/Coal/examples/47/Main.coal"
    ]

main48 :: IO (Either CompilerError Text)
main48 = do
  runTestFiles
    [ "./test/Coal/examples/48/Main.coal"
    ]

main49 :: IO (Either CompilerError Text)
main49 = do
  runTestFiles
    [ "./test/Coal/examples/49/Main.coal"
    ]

main50 :: IO (Either CompilerError Text)
main50 = do
  runTestFiles
    [ "./test/Coal/examples/50/Main.coal"
    ]

main51 :: IO (Either CompilerError Text)
main51 = do
  runTestFiles
    [ "./test/Coal/examples/51/Main.coal"
    ]

main52 :: IO (Either CompilerError Text)
main52 = do
  runTestFiles
    [ "./test/Coal/examples/52/Main.coal"
    ]

main53 :: IO (Either CompilerError Text)
main53 = do
  runTestFiles
    [ "./test/Coal/examples/53/Main.coal"
    ]

main54 :: IO (Either CompilerError Text)
main54 = do
  runTestFiles
    [ "./test/Coal/examples/54/Main.coal"
    ]

main55 :: IO (Either CompilerError Text)
main55 = do
  runTestFiles
    [ "./test/Coal/examples/55/Main.coal"
    ]

main56 :: IO (Either CompilerError Text)
main56 = do
  runTestFiles
    [ "./test/Coal/examples/56/Main.coal"
    ]

main57 :: IO (Either CompilerError Text)
main57 = do
  runTestFiles
    [ "./test/Coal/examples/57/Main.coal"
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
    , compilerCodataAccessorEnvironment = Environment.insertMultiple builtinCodataAccessors compilerCodataAccessorEnvironment
    , ..
    }

builtinCodataAccessors :: [(Name, CodataAccessor TypeIndex Kind IndexedType)]
builtinCodataAccessors =
  [ -- TODO: remove

    ( "Head"
    , CodataAccessor
        "Head"
        ( Forall
            (Set.fromList mempty)
            []
            ( TConstructor KType "Stream" `TArrow` TIntrinsic IInt32
            )
        )
    )
  ,
    ( "Tail"
    , CodataAccessor
        "Tail"
        ( Forall
            (Set.fromList mempty)
            []
            ( TConstructor KType "Stream" `TArrow` TConstructor KType "Stream"
            )
        )
    )
  ]

builtinTraits :: [(Name, (TypeIndex Kind, Environment IndexedScheme))]
builtinTraits =
  []

--    ( "Numeric"
--    ,
--      ( TypeIndex KType 0
--      , Environment.fromList
--          [
--            ( "from_int32"
--            , Forall
--                (Set.fromList [TypeIndex KType 0])
--                []
--                ( TIntrinsic IInt32 `TArrow` TVariable (TypeIndex KType 0)
--                )
--            )
--          ,
--            ( "negate"
--            , Forall
--                (Set.fromList [TypeIndex KType 0])
--                []
--                ( TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0)
--                )
--            )
--          ]
--      )
--    )
--  ,
--    ( "Ordered"
--    ,
--      ( TypeIndex KType 0
--      , Environment.fromList
--          [
--            ( "compare"
--            , Forall
--                (Set.fromList [TypeIndex KType 0])
--                []
--                ( TVariable (TypeIndex KType 0)
--                    `TArrow` TVariable (TypeIndex KType 0)
--                    `TArrow` TConstructor KType "Ordering"
--                )
--            )
--          ]
--      )
--    )

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
              ,
                ( "negate"
                , Forall mempty [] (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32)
                )
              ,
                ( "(+)"
                , Forall mempty [] (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IInt32)
                )
              ,
                ( "(-)"
                , Forall mempty [] (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IInt32)
                )
              ,
                ( "(*)"
                , Forall mempty [] (TIntrinsic IInt32 `TArrow` TIntrinsic IInt32 `TArrow` TIntrinsic IInt32)
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
              ,
                ( "negate"
                , Forall mempty [] (TIntrinsic INat `TArrow` TIntrinsic INat)
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
  [
    ( "List"
    , KArrow KType KType
    )
  ]

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

addBuiltinDefs :: (Monoid a) => [Definition a Kind ()] -> [Definition a Kind ()]
addBuiltinDefs defs =
  [ DImport
      (Path ["Core$"])
      ( (fst <$> names)
          <> [ "from_int32__$instance_Numeric(Intrinsic(Int32))"
             , "from_int32__$instance_Numeric(Intrinsic(Nat))"
             , "negate__$instance_Numeric(Intrinsic(Int32))"
             , "negate__$instance_Numeric(Intrinsic(Nat))"
             , "(+)__$instance_Numeric(Intrinsic(Int32))"
             , "(-)__$instance_Numeric(Intrinsic(Int32))"
             , "(*)__$instance_Numeric(Intrinsic(Int32))"
             , "compare__$instance_Ordered(Intrinsic(Int32))"
             ]
      )
  , DTrait
      "Numeric"
      []
      (Parameter KType "a")
      [
        ( "from_int32"
        , TIntrinsic IInt32 `TArrow` TVariable (Parameter () "a")
        )
      ,
        ( "negate"
        , TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a")
        )
      ,
        ( "(+)"
        , TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a")
        )
      ,
        ( "(-)"
        , TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a")
        )
      ,
        ( "(*)"
        , TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a")
        )
      ]
  , DTrait
      "Ordered"
      []
      (Parameter KType "a")
      [
        ( "compare"
        , TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TConstructor () "Ordering"
        )
      ]
  , DType
      "Ordering"
      []
      [ Constructor "LessThan" 0 (Forall mempty [] (TConstructor () "Ordering"))
      , Constructor "GreaterThan" 0 (Forall mempty [] (TConstructor () "Ordering"))
      , Constructor "EqualTo" 0 (Forall mempty [] (TConstructor () "Ordering"))
      ]
  , DType
      "Option"
      [Parameter () "a"]
      [ Constructor "Some" 1 (Forall (Set.fromList [Parameter () "a"]) [] (TVariable (Parameter () "a") `TArrow` TApplication () (TConstructor () "Option") (TVariable (Parameter () "a") :| [])))
      , Constructor "None" 0 (Forall (Set.fromList [Parameter () "a"]) [] (TApplication () (TConstructor () "Option") (TVariable (Parameter () "a") :| [])))
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
      liftIO $ writeDotFiles "untyped" m1
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

  liftIO $ writeDotFiles "typed" a

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

  -- TODO: Num class

  b <- mainPass a

  liftIO $ writeDotFiles "compiled" b

  cc <- gets compilerAssumptions
  case nub cc of
    as@(_ : _) ->
      forM_ as $
        \Assumption{..} -> do
          src <- gets compilerSourceText
          let msg =
                prettyErrorMessage
                  [ "\nName not in scope:"
                  , assumptionName
                  ]
                  src
                  Assumption{..}
          throwError (CompilerError msg)
    [] ->
      pure ()

  r <- kernelTranslationC b

  liftIO $ writeDotFilesK "kernel" r

  pure r

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
        (tupleType (TVariable (TypeIndex KType 0) :| [TVariable (TypeIndex KType 1)]) `TArrow` TIntrinsic IString)
    )
  ,
    ( "list_to_string"
    , Forall
        (Set.fromList [TypeIndex KType 0] :: Set (TypeIndex Kind))
        [ Trait "Traceable" (TVariable (TypeIndex KType 0))
        ]
        (listType (TVariable (TypeIndex KType 0)) `TArrow` TIntrinsic IString)
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
        ( listType (TVariable (TypeIndex KType 0))
            `TArrow` listType (TVariable (TypeIndex KType 0))
            `TArrow` listType (TVariable (TypeIndex KType 0))
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
    ( "trace_char"
    , Forall
        (Set.fromList [TypeIndex KType 0])
        []
        ( TIntrinsic IChar `TArrow` TVariable (TypeIndex KType 0)
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
    ( "negate"
    , Forall
        (Set.fromList [TypeIndex KType 0] :: Set (TypeIndex Kind))
        [Trait "Numeric" (TVariable (TypeIndex KType 0))]
        (TVariable (TypeIndex KType 0) `TArrow` TVariable (TypeIndex KType 0))
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
  ,
    ( "string_to_list"
    , Forall
        mempty
        []
        (TIntrinsic IString `TArrow` listType (TIntrinsic IChar))
    )
  ,
    ( "string_head"
    , Forall
        mempty
        []
        (TIntrinsic IString `TArrow` TIntrinsic IChar)
    )
  ,
    ( "string_tail"
    , Forall
        mempty
        []
        (TIntrinsic IString `TArrow` TIntrinsic IString)
    )
  ,
    ( "string_reverse"
    , Forall
        mempty
        []
        (TIntrinsic IString `TArrow` TIntrinsic IString)
    )
  ,
    ( "string_remove_whitespace"
    , Forall
        mempty
        []
        (TIntrinsic IString `TArrow` TIntrinsic IString)
    )
  ,
    ( "string_length"
    , Forall
        mempty
        []
        (TIntrinsic IString `TArrow` TIntrinsic IInt32)
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
            "Core$.trace_char"
            [ Label Kernel.char "c"
            ]
            [r|
                  #(print_char : char/*, c : char) (fn(a : *) => a : *)
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
            "Core$.negate"
            [ Label (Kernel.TCon "Numeric" [opaque]) "$a"
            ]
            [r| 
                  match<*/*>($a : Numeric(*)) {
                    | ( $Record : { negate : */* | * }/Numeric(*)
                      , $r : { negate : */* | * }
                      ) =>
                        select
                          { negate = $f : */* | _ : * } =
                            $r : { negate : */* | * }
                          in
                            $f : */*
                  }
              |]
        , OFunction
            "Core$.(+)"
            [ Label (Kernel.TCon "Numeric" [opaque]) "$a"
            ]
            [r| 
                  match<*/*/*>($a : Numeric(*)) {
                    | ( $Record : { `(+)` : */*/* | * }/Numeric(*)
                      , $r : { `(+)` : */*/* | * }
                      ) =>
                        select
                          { `(+)` = $f : */*/* | _ : * } =
                            $r : { `(+)` : */*/* | * }
                          in
                            $f : */*/*
                  }
              |]
        , OFunction
            "Core$.(-)"
            [ Label (Kernel.TCon "Numeric" [opaque]) "$a"
            ]
            [r| 
                  match<*/*/*>($a : Numeric(*)) {
                    | ( $Record : { `(-)` : */*/* | * }/Numeric(*)
                      , $r : { `(-)` : */*/* | * }
                      ) =>
                        select
                          { `(-)` = $f : */*/* | _ : * } =
                            $r : { `(-)` : */*/* | * }
                          in
                            $f : */*/*
                  }
              |]
        , OFunction
            "Core$.(*)"
            [ Label (Kernel.TCon "Numeric" [opaque]) "$a"
            ]
            [r| 
                  match<*/*/*>($a : Numeric(*)) {
                    | ( $Record : { `(*)` : */*/* | * }/Numeric(*)
                      , $r : { `(*)` : */*/* | * }
                      ) =>
                        select
                          { `(*)` = $f : */*/* | _ : * } =
                            $r : { `(*)` : */*/* | * }
                          in
                            $f : */*/*
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
            "Core$.(+)__$instance_Numeric(Intrinsic(Int32))"
            [ Label Kernel.int32 "lhs"
            , Label Kernel.int32 "rhs"
            ]
            [r| 
                  [+ int32](lhs : int32, rhs : int32)
              |]
        , OFunction
            "Core$.(-)__$instance_Numeric(Intrinsic(Int32))"
            [ Label Kernel.int32 "lhs"
            , Label Kernel.int32 "rhs"
            ]
            [r| 
                  [- int32](lhs : int32, rhs : int32)
              |]
        , OFunction
            "Core$.(*)__$instance_Numeric(Intrinsic(Int32))"
            [ Label Kernel.int32 "lhs"
            , Label Kernel.int32 "rhs"
            ]
            [r| 
                  [* int32](lhs : int32, rhs : int32)
              |]
        , OFunction
            "Core$.negate__$instance_Numeric(Intrinsic(Int32))"
            [ Label Kernel.int32 "n"
            ]
            [r| 
                  [- int32](0, n : int32)
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
            "Core$.negate__$instance_Numeric(Intrinsic(Nat))"
            [ Label (Kernel.TCon "$Nat" []) "_"
            ]
            [r| 
                  $Zero : $Nat
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
        , OFunction
            "Core$.string_length"
            [ Label Kernel.string "str"
            ]
            [r| 
                  #(string_length : string/int32, str : string) (fn(a : int32) => a : int32)
              |]
        , OFunction
            "Core$.string_head"
            [ Label Kernel.string "str"
            ]
            [r| 
                  #(string_head : string/char, str : string) (fn(a : char) => a : char)
              |]
        , OFunction
            "Core$.string_tail"
            [ Label Kernel.string "str"
            ]
            [r| 
                  #(string_tail : string/string, str : string) (fn(a : string) => a : string)
              |]
        , OFunction
            "Core$.string_reverse"
            [ Label Kernel.string "str"
            ]
            [r| 
                  #(string_reverse : string/string, str : string) (fn(a : string) => a : string)
              |]
        , OFunction
            "Core$.string_remove_whitespace"
            [ Label Kernel.string "str"
            ]
            [r| 
                  #(string_remove_whitespace : string/string, str : string) (fn(a : string) => a : string)
              |]
        , OFunction
            "Core$.string_to_list"
            [ Label Kernel.string "str"
            ]
            [r| 
                  let
                    f : string/list(char)/list(char) =
                      fn(input : string, result : list(char)) => 
                        if ( [== int32]
                               ( @<int32>
                                   ( Core$.string_length : string/int32 
                                   , input : string
                                   )
                               , 0 
                               ) )
                          then
                            result : list(char)
                          else 
                            @<list(char)>
                              ( f : string/list(char)/list(char)
                              , @<string>
                                  ( Core$.string_tail : string/string
                                  , input : string
                                  )
                              , @<list(char)>
                                  ( $Cons : char/list(char)/list(char)
                                  , @<char>
                                      ( Core$.string_head : string/char
                                      , input : string
                                      )
                                  , result : list(char)
                                  )
                              )
                    in
                      @<list(char)>
                        ( f : string/list(char)/list(char)
                        , @<string>
                            ( Core$.string_reverse : string/string 
                            , str : string
                            )
                        , $Nil : list(char)
                        )
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
