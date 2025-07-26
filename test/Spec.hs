{-# LANGUAGE OverloadedStrings #-}

import Coal.Compiler
import Coal.Compiler.Environment
import Coal.Compiler.Stack
import Coal.Language
import Coal.Language.Module
import Coal.Ast.Metadata (Metadata (..))
import Coal.Parser.Module
import Control.Monad (forM)
import Coal.Common.Name (Name)
import Control.Monad.Reader (local)
import Control.Monad.State (gets, liftIO)
import Data.Data (Data)
import Data.Either (partitionEithers)
import Data.Set (Set)
import Data.Text (Text)
import Data.Void (Void)
import Debug.Trace
import Text.Megaparsec (ParseErrorBundle, errorBundlePretty, runParser)

import qualified Data.Set as Set
import qualified Data.Text as Text

main :: IO ()
main = do
  compileFiles
    [ "./test/Coal/examples/03/Main.coal"
    ]
  pure ()

compileFiles :: [String] -> IO ()
compileFiles files = do
  fs <- traverse readFile files
  let rs = fmap (parseFile . Text.pack) fs
  case partitionEithers rs of
    (e : _, _) ->
      putStrLn (errorBundlePretty e)
    (_, ys) -> do
      evalCompilerT emptyCompilerEnvironment (bork ys)

bork :: [Module Metadata Kind ()] -> CompilerT Metadata IO ()
bork modules = do
  tms <- forM modules $
    \m@(Module _ _ defs) -> do
      insertNamesC names
      local (\_ -> buildEnvironment defs) (typePass m)
  x1 <- gets compilerConstraints
  liftIO (print x1)
  x2 <- gets compilerConstraintsGenErrors
  liftIO (print x2)
  x3 <- gets compilerSolverRuleViolations
  liftIO (print x3)
  x4 <- gets compilerTypeAnnotationParams
  liftIO (print x4)
  traceShowM tms

parseFile :: Text -> Either (ParseErrorBundle Text Void) (Module Metadata o ())
parseFile = runParser parseModule ""

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
  ]
