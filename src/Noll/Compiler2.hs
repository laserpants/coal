{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData #-}

module Noll.Compiler2 where

import Control.Monad ((>=>))
import Control.Monad.Reader (Reader, asks, runReader)
import Control.Monad.State (gets, runState)
import Data.Data (Data)
import Noll.Compiler.NormalizeObjects (NormalizeObjectsTransformContext (..))
import Noll.Compiler.PatternMatching
import Noll.Compiler.PatternMatching.Rule (MatchMonad (..), runMatchMonad)
import Noll.Compiler.Transform.Fold
import Noll.Compiler.Transform.Pattern.Desugar
import Noll.Compiler.Transform.Pattern.OrExpansion
import Noll.Compiler.Transform.Type.AliasExpansion
import Noll.Compiler.Transform.Unfold
import Noll.Compiler2.Internal
import Noll.Compiler2.TypeInference
import Noll.Language
import Noll.Module (Definition (..), Module (..))
import Noll.SystemF.Substitution (normalizeTypeIndexes)

withSupplyC :: (Monad m) => (Int -> (c, Int)) -> Compiler2T a m c
withSupplyC f = do
  n <- gets compiler2Supply
  let (r, n') = f n
  insertSupplyC n'
  pure r

aliasExpansionTrans :: (Monad m) => (c -> Reader AliasEnvironment c) -> c -> Compiler2T a m c
aliasExpansionTrans f e = asks (runReader (f e) . compiler2AliasEnv)

expandAliasesC :: (Monad m, Data a) => Module a Kind () -> Compiler2T a m (Module a Kind ())
expandAliasesC = aliasExpansionTrans expandAliases

foldExpansionTrans :: (Monad m) => (c -> FoldExpansion c) -> c -> Compiler2T a m c
foldExpansionTrans f e = withSupplyC (\n -> runFoldExpansion "fold" n (f e))

compileUnfoldsC :: (Monad m, Data a, Monoid a) => Module a Kind () -> Compiler2T a m (Module a Kind ())
compileUnfoldsC = foldExpansionTrans compileFolds

unfoldExpansionTrans :: (Monad m) => (c -> UnfoldExpansion c) -> c -> Compiler2T a m c
unfoldExpansionTrans f e = withSupplyC (\n -> runUnfoldExpansion "unfold" n (f e))

compileFoldsC :: (Monad m, Data a, Monoid a) => Module a Kind () -> Compiler2T a m (Module a Kind ())
compileFoldsC = unfoldExpansionTrans compileUnfolds

indexedC :: (Monad m, Traversable t) => t e -> Compiler2T a m (t IndexedType)
indexedC t = withSupplyC (runState (indexed t))

runTypeInferenceC :: (Monad m, Data a, Eq a, Show a) => Module a Kind () -> Compiler2T a m (Module a Kind IndexedType)
runTypeInferenceC m = do
  defs <- traverse indexedC ds
  (tdefs, _) <- typeDefinitionsC defs
  pure (Module p ns (normalizeTypeIndexes tdefs))
 where
  Module p ns ds = m

normalizeObjectC :: (Monad m, NormalizeObjectsTransformContext c) => c -> Compiler2T a m c
normalizeObjectC = pure . normalizeObject

denormalizeObjectC :: (Monad m, NormalizeObjectsTransformContext c) => c -> Compiler2T a m c
denormalizeObjectC = pure . denormalizeObject

patternDesugarTrans :: (Monad m) => (c -> PatternDesugar s TypeIndex Kind c) -> c -> Compiler2T a m c
patternDesugarTrans f e = withSupplyC (\n -> runPatternDesugar "v" n (f e))

desugarPatternsC :: (Monad m, Sugared s TypeIndex Kind c) => c -> Compiler2T a m c
desugarPatternsC = patternDesugarTrans desugarPatterns

matchMonadTrans :: (Monad m) => (c -> MatchMonad c) -> c -> Compiler2T a m c
matchMonadTrans f e = withSupplyC (\n -> runMatchMonad "match" n (f e))

compileMatchExprsC :: (Monad m, MatchExpressionContext c) => c -> Compiler2T a m c
compileMatchExprsC = matchMonadTrans compileMatchExprs

--

passOne :: (Monad m, Monoid a, Data a, Eq a, Show a) => Module a Kind () -> Compiler2T a m (Module a Kind IndexedType)
passOne =
  -- Expand type aliases
  expandAliasesC
    -- Expand unfolds (codata)
    >=> compileUnfoldsC
    -- Expand folds
    >=> compileFoldsC
    -- Type inference
    >=> runTypeInferenceC

compileModule :: (Monad m, Monoid a, Data a, Eq a, Show a) => Module a Kind () -> Compiler2T a m (Module a Kind IndexedType)
compileModule =
  passOne
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
    -- Final lowering
    >=> undefined

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
