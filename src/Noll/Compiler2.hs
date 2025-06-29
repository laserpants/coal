{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler2 where

import Control.Monad ((>=>))
import Control.Monad.RWS (RWST, runRWST)
import Control.Monad.Reader (MonadReader, Reader, ReaderT, ask, asks, runReader, runReaderT)
import Control.Monad.State (MonadState, StateT, gets, modify, put, runState, runStateT)
import Control.Monad.Writer (execWriter)
import Lang.Common.Environment (Environment (..))
import Lang.Common.Supply (Supply (..), supplied)
import Lang.Utils (Dictionary, Name, Over, forM_, (<$$$>))
import Noll.Compiler.NormalizeObjects (NormalizeObjectsTransformContext (..))
import Noll.Compiler.PatternMatching
import Noll.Compiler.PatternMatching.Rule (MatchMonad (..), matchPatterns, runMatchMonad)
import Noll.Compiler.Transform.Fold
import Noll.Compiler.Transform.Pattern.Desugar
import Noll.Compiler.Transform.Pattern.OrExpansion
import Noll.Compiler.Transform.Type.AliasExpansion
import Noll.Compiler.Transform.Unfold
import Noll.Compiler2.TypeInference
import Noll.Language
import Noll.Module (Constant (..), Definition (..), Function (..), Module (..), Path (..))
import Noll.SystemF
import Noll.SystemF.Substitution (mapsTo)

import qualified Lang.Common.Environment as Environment

data Compiler2Environment o k t = Compiler2Environment
  { compiler2DataConstructorEnv :: Environment (Constructor o k t)
  , compiler2TypeConstructorEnv :: Environment Kind
  , compiler2TraitEnv :: Environment (o k, Environment (Scheme o k t))
  , compiler2AliasEnv :: AliasEnvironment
  }
  deriving (Show, Eq, Ord, Read)

data Compiler2State = Compiler2State
  { compiler2Supply :: Int
  , compiler2NameStore :: Environment (Scheme TypeIndex Kind IndexedType)
  , compilerSubstitution :: Substitution
  }
  deriving (Show, Eq, Ord, Read)

{-# INLINE overCompiler2NameStore #-}
overCompiler2NameStore :: Over Compiler2State (Environment (Scheme TypeIndex Kind IndexedType))
overCompiler2NameStore fn Compiler2State{..} = Compiler2State{compiler2NameStore = fn compiler2NameStore, ..}

{-# INLINE overCompiler2Supply #-}
overCompiler2Supply :: Over Compiler2State Int
overCompiler2Supply fn Compiler2State{..} = Compiler2State{compiler2Supply = fn compiler2Supply, ..}

initialCompiler2State :: Compiler2State
initialCompiler2State =
  Compiler2State
    { compiler2Supply = 0
    , compiler2NameStore = mempty
    , compilerSubstitution = mempty
    }

type Compiler2Stack m c = RWST (Compiler2Environment TypeIndex Kind IndexedType) () Compiler2State m c

newtype Compiler2T m c = Compiler2 {compiler2Stack :: Compiler2Stack m c}
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader (Compiler2Environment TypeIndex Kind IndexedType)
    , MonadState Compiler2State
    )

{-# INLINE runCompiler2T #-}
runCompiler2T :: (Monad m) => Compiler2Environment TypeIndex Kind IndexedType -> Compiler2T m c -> m (c, Compiler2State)
runCompiler2T env com = do
  (c, s, _) <- runRWST (compiler2Stack com) env initialCompiler2State
  pure (c, s)

{-# INLINE evalCompiler2T #-}
evalCompiler2T :: (Monad m) => Compiler2Environment TypeIndex Kind IndexedType -> Compiler2T m c -> m c
evalCompiler2T = fst <$$$> runCompiler2T

{-# INLINE insertSupplyC #-}
insertSupplyC :: (Monad m) => Int -> Compiler2T m ()
insertSupplyC = modify . overCompiler2Supply . const

{-# INLINE insertNamesC #-}
insertNamesC :: (Monad m) => [(Name, Scheme TypeIndex Kind IndexedType)] -> Compiler2T m ()
insertNamesC names = modify (overCompiler2NameStore (Environment.insertMultiple names))

instance Supply Compiler2State where
  updateSupply = overCompiler2Supply
  getSupply = compiler2Supply

--

withSupplyC :: (Monad m) => (Int -> (a, Int)) -> Compiler2T m a
withSupplyC f = do
  n <- gets compiler2Supply
  let (r, n') = f n
  insertSupplyC n'
  pure r

aliasExpansionTrans :: (Monad m) => (a -> Reader AliasEnvironment a) -> a -> Compiler2T m a
aliasExpansionTrans f e = asks (runReader (f e) . compiler2AliasEnv)

expandAliasesC :: (Monad m, AliasContext a) => a -> Compiler2T m a
expandAliasesC = aliasExpansionTrans expandAliases

foldExpansionTrans :: (Monad m) => (a -> FoldExpansion a) -> a -> Compiler2T m a
foldExpansionTrans f e = withSupplyC (\n -> runFoldExpansion "fold" n (f e))

compileUnfoldsC :: (Monad m, CompileFoldsContext a) => a -> Compiler2T m a
compileUnfoldsC = foldExpansionTrans compileFolds

unfoldExpansionTrans :: (Monad m) => (a -> UnfoldExpansion a) -> a -> Compiler2T m a
unfoldExpansionTrans f e = withSupplyC (\n -> runUnfoldExpansion "unfold" n (f e))

compileFoldsC :: (Monad m) => (CompileUnfoldsContext a) => a -> Compiler2T m a
compileFoldsC = unfoldExpansionTrans compileUnfolds

indexedC :: (Monad m, Traversable t) => t e -> Compiler2T m (t IndexedType)
indexedC t = withSupplyC (runState (indexed t))

runTypeInferenceC :: (Monad m) => Module () () () -> Compiler2T m (Module () Kind IndexedType)
runTypeInferenceC m = do
  defs <- traverse indexedC ds
  (tdefs, as) <- typeDefinitionsC defs
  undefined
 where
  Module p ns ds = m

normalizeObjectC :: (Monad m, NormalizeObjectsTransformContext a) => a -> Compiler2T m a
normalizeObjectC = pure . normalizeObject

denormalizeObjectC :: (Monad m, NormalizeObjectsTransformContext a) => a -> Compiler2T m a
denormalizeObjectC = pure . denormalizeObject

patternDesugarTrans :: (Monad m) => (a -> PatternDesugar c TypeIndex Kind a) -> a -> Compiler2T m a
patternDesugarTrans f e = withSupplyC (\n -> runPatternDesugar "v" n (f e))

desugarPatternsC :: (Monad m, Sugared c TypeIndex Kind a) => a -> Compiler2T m a
desugarPatternsC = patternDesugarTrans desugarPatterns

matchMonadTrans :: (Monad m) => (a -> MatchMonad a) -> a -> Compiler2T m a
matchMonadTrans f e = withSupplyC (\n -> runMatchMonad "match" n (f e))

compileMatchExprsC :: (Monad m, MatchExpressionContext a) => a -> Compiler2T m a
compileMatchExprsC = matchMonadTrans compileMatchExprs

--

compileModule :: (Monad m) => Module () () () -> Compiler2T m (Module () Kind IndexedType)
compileModule =
  -- Expand type aliases
  expandAliasesC
    -- Expand unfolds (codata)
    >=> compileUnfoldsC
    -- Expand folds
    >=> compileFoldsC
    -- Type inference
    >=> runTypeInferenceC
    -- Normalize top-level expressions
    >=> normalizeObjectC
    -- Translate patterns in expression arguments to match expressions
    >=> desugarPatternsC
    -- Compile or-patterns
    >=> compileOrPatterns
    --    -- Translate record patterns to select operators
    --    >=> TODO
    -- Compile match statements
    >=> compileMatchExprsC
    --    -- Placeholder insertion
    --    >=> TODO
    -- Denormalize top-level expressions
    >=> denormalizeObjectC

--    -- Final lowering
--    >=> undefined

-----------------------
-----------------------
-----------------------

-- import Control.Monad.Reader (runReader)
-- import Lang.Common.List1 (NonEmpty (..), (<|))
-- import Lang.Label (Label (..))
-- import Noll.Compiler.Lowpass.Environment (initialTranslateEnvironment)
-- import Noll.Compiler.Lowpass.TranslateModule (translateModule)
-- import Noll.Language
-- import Noll.Module (Constant (..), Definition (..), Function (..), Module (..), Path (..))
-- import Noll.Set3.Test13x (moduleCore1)
--
-- import qualified Data.Map.Strict as Map
-- import qualified Data.Set as Set
-- import qualified Lang.Common.Environment as Environment
-- import qualified Lang.Lowpass.Compiler as Lowpass
-- import qualified Lang.Lowpass.Compiler.Utils as Lowpass
-- import qualified Lang.Lowpass.Language as Lowpass
-- import qualified Noll.Module as Module
--
-- progx_05 :: [Module () () ()]
-- progx_05 =
--   [ moduleMainB
--   ]
--
-- moduleMainB :: Module () () ()
-- moduleMainB =
--   Module.fromDefinitionList
--     (Path ["Main"])
--     []
--     [ DImport (Path ["Core$"]) ["trace_string"]
--     , DCodata
--         "Stream"
--         [Parameter () "a"]
--         [
--           ( "Head"
--           , TVariable (Parameter () "a")
--           )
--         ,
--           ( "Tail"
--           , TApplication () (TConstructor () "Stream") (TVariable (Parameter () "a") :| [])
--           )
--         ]
--     , DConstant
--         "nats"
--         ( Constant
--             ()
--             (With [] ())
--             ( EUnfold
--                 ()
--                 ()
--                 (Label () "Stream")
--                 "f"
--                 (PVariable () (Label () "n") :| [])
--                 ( Map.fromList
--                     [
--                       ( "Head"
--                       , EVariable () (Label () "n")
--                       )
--                     ,
--                       ( "Tail"
--                       , EApplication
--                           ()
--                           ()
--                           (EVariable () (Label () "f"))
--                           ( EApplication
--                               ()
--                               ()
--                               (EBinaryOperator () () OAddition)
--                               ( EVariable () (Label () "n")
--                                   <| ELiteral () (LInt32 1)
--                                   :| []
--                               )
--                               :| []
--                           )
--                       )
--                     ]
--                 )
--                 Nothing
--             )
--         )
--     , DFunction
--         "nth"
--         ( Function
--             ()
--             (With [] ())
--             (PVariable () (Label () "n") :| [])
--             ( EFold
--                 ()
--                 ()
--                 (EVariable () (Label () "n") :| [])
--                 ( EClause
--                     ()
--                     ( PConstructor
--                         ()
--                         (Label () "Zero")
--                         []
--                     )
--                     ( CPlain
--                         ()
--                         []
--                         ( ELambda
--                             ()
--                             (PVariable () (Label () "stream") :| [])
--                             ( ECodataSelect
--                                 ()
--                                 (Label () "Head")
--                                 (EVariable () (Label () "stream"))
--                                 Nothing
--                             )
--                         )
--                         :| []
--                     )
--                     <| EClause
--                       ()
--                       ( PConstructor
--                           ()
--                           (Label () "Succ")
--                           [ PAtVariable () (Label () "f")
--                           ]
--                       )
--                       ( CPlain
--                           ()
--                           []
--                           ( ELambda
--                               ()
--                               (PVariable () (Label () "stream") :| [])
--                               ( EApplication
--                                   ()
--                                   ()
--                                   (EVariable () (Label () "f"))
--                                   ( ECodataSelect
--                                       ()
--                                       (Label () "Tail")
--                                       (EVariable () (Label () "stream"))
--                                       Nothing
--                                       :| []
--                                   )
--                               )
--                           )
--                           :| []
--                       )
--                     :| []
--                 )
--                 Nothing
--             )
--         )
--     , DFunction
--         "main"
--         ( Function
--             ()
--             (With [] ())
--             (PLiteral () LUnit :| [])
--             ( ELet
--                 ()
--                 ( BPattern
--                     ()
--                     (PVariable () (Label () "v"))
--                     ( EApplication
--                         ()
--                         ()
--                         (EVariable () (Label () "nth"))
--                         ( ELiteral () (LInt32 5)
--                             <| EVariable () (Label () "nats")
--                             :| []
--                         )
--                     )
--                     :| []
--                 )
--                 ( EApplication
--                     ()
--                     ()
--                     (EVariable () (Label () "trace_string"))
--                     ( EVariable () (Label () "v")
--                         :| []
--                     )
--                 )
--             )
--         )
--     ]
--
-- ---
-- ---
-- ---
-- ---
--
-- progx_04 :: [Module () Kind IndexedType]
-- progx_04 =
--   [ moduleMain
--   ]
--
-- moduleMain :: Module () Kind IndexedType
-- moduleMain =
--   Module.fromDefinitionList
--     (Path ["Main"])
--     []
--     [ DImport (Path ["Core$"]) ["trace_string"]
--     , DFunction
--         "main"
--         ( Function
--             ()
--             (With [] (TVariable (TypeIndex KType 0)))
--             (PLiteral () LUnit :| [])
--             ( EApplication
--                 ()
--                 (TVariable (TypeIndex KType 0))
--                 (EVariable () (Label (TIntrinsic IString `TArrow` TVariable (TypeIndex KType 0)) "trace_string"))
--                 ( ELiteral () (LString "Hello, world!")
--                     :| []
--                 )
--             )
--         )
--     ]
--
-- moduleMain2 :: Module () Kind IndexedType
-- moduleMain2 =
--   Module.fromDefinitionList
--     (Path ["Main"])
--     []
--     [ DImport (Path ["Core$"]) ["trace_string"]
--     , DConstant
--         "main"
--         ( Constant
--             ()
--             (With [] (TIntrinsic IUnit `TArrow` TVariable (TypeIndex KType 0)))
--             ( ELambda
--                 ()
--                 (PLiteral () LUnit :| [])
--                 ( EApplication
--                     ()
--                     (TVariable (TypeIndex KType 0))
--                     (EVariable () (Label (TIntrinsic IString `TArrow` TVariable (TypeIndex KType 0)) "trace_string"))
--                     ( ELiteral () (LString "Hello, world!")
--                         :| []
--                     )
--                 )
--             )
--         )
--     ]
--
-- banan1 =
--   Lowpass.compileModules (runReader (traverse translateModule Noll.Compiler2.progx_04) testNameEnvironment)
--
-- banan2 :: IO ()
-- banan2 = Lowpass.testModules =<< Lowpass.compileModules (moduleCore1 : xs)
--  where
--   xs = runReader (traverse translateModule Noll.Compiler2.progx_04) testNameEnvironment
--
-- testNameEnvironment =
--   initialTranslateEnvironment
--     ( Environment.fromList
--         [
--           ( "always"
--           , "Core$.always"
--           )
--         ,
--           ( "trace"
--           , "trace"
--           )
--         ,
--           ( "@@@_trace_int32"
--           , "Core$.trace_int32"
--           )
--         ,
--           ( "not"
--           , "Core$.operator__not"
--           )
--         ]
--     )
