{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.Module.BundleSpec (bundleSpec) where

import Coal.Ast.Metadata (Metadata (..))
import Coal.Common.Environment (Environment (..), mapEnvironment)
import qualified Coal.Common.Environment as Environment
import Coal.Compiler (pipeline)
import Coal.Compiler.Environment
import Coal.Compiler.Module.Builders
import Coal.Compiler.Module.Bundle
import Coal.Compiler.Pass (Pass (..), mapPass, (>->))
import Coal.Compiler.Pass.ParsingPhase (parsingPhase)
import Coal.Compiler.Pass.PreflightPhase (preflightPhase)
import Coal.Compiler.Pass.PreflightPhase.TopologicalSort (passTopologicalSort)
import Coal.Compiler.Stack
import Coal.Language
import Coal.Language.Module
import Data.Either (rights)
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.Set as Set
import Debug.Trace
import Extras (Name, forM, forM_)
import System.Process
import Test.Hspec

bundleSpec :: Spec
bundleSpec = do
  res <- runIO $ runBundle ["./lang/Nat.coal", "./lang/IO.coal", "./test/Coal/examples/133/Main.coal"]
  let bundle : _ = filter (\ModuleBundle{..} -> modulePath == Path ["Main"]) (rights (sequence res))
  testMain bundle

testMain :: ModuleBundle -> Spec
testMain ModuleBundle{..} = do
  describe "DataConstructors" $ do
    it "" $
      mapEnvironment stripMeta moduleDataConstructors == mainDataConstructors

  describe "TypeConstructors" $ do
    it "" $
      mapEnvironment stripMeta moduleTypeConstructors == mainTypeConstructors

  describe "CodataAccessors" $ do
    it "" $
      mapEnvironment stripMeta moduleCodataAccessors == mainCodataAccessors

  describe "CotypeConstructorInfo" $ do
    it "" $
      mapEnvironment stripMeta moduleCotypeConstructors == mainCotypeConstructors

  describe "Names" $ do
    it "" $
      moduleNames == mainNames

  describe "Exports" $ do
    it "" $
      moduleExports == Set.fromList ["*"]

class StripMeta i where
  stripMeta :: i a -> i ()

instance StripMeta DataConstructorInfo where
  stripMeta (DataConstructorInfo _ n c s) = DataConstructorInfo () n c s

instance StripMeta TypeConstructorInfo where
  stripMeta (TypeConstructorInfo _ n k) = TypeConstructorInfo () n k

instance StripMeta CodataAccessorInfo where
  stripMeta (CodataAccessorInfo _ n a) = CodataAccessorInfo () n a

instance StripMeta CotypeConstructorInfo where
  stripMeta (CotypeConstructorInfo _ n k) = CotypeConstructorInfo () n k

mainNames :: Environment NameInfo
mainNames =
  Environment.fromList
    [
      ( "Option"
      , IType (KArrow KType KType)
      )
    ,
      ( "Stream"
      , ICotype (KArrow KType KType)
      )
    ,
      ( "None"
      , IDataConstructor (Forall (Set.fromList [TypeIndex KType 0]) [] (TApplication KType (TConstructor (KArrow KType KType) "Option") (TVariable (TypeIndex KType 0) :| [])))
      )
    ,
      ( "Some"
      , IDataConstructor (Forall (Set.fromList [TypeIndex KType 0]) [] (TVariable (TypeIndex KType 0) `TArrow` TApplication KType (TConstructor (KArrow KType KType) "Option") (TVariable (TypeIndex KType 0) :| [])))
      )
    ,
      ( "Head"
      , ICodataAccessor (Forall (Set.fromList [TypeIndex KType 0]) [] (TApplication KType (TConstructor (KArrow KType KType) "Stream") (TVariable (TypeIndex KType 0) :| []) `TArrow` TVariable (TypeIndex KType 0)))
      )
    ,
      ( "Tail"
      , ICodataAccessor (Forall (Set.fromList [TypeIndex KType 0]) [] (TApplication KType (TConstructor (KArrow KType KType) "Stream") (TVariable (TypeIndex KType 0) :| []) `TArrow` TApplication KType (TConstructor (KArrow KType KType) "Stream") (TVariable (TypeIndex KType 0) :| [])))
      )
    ]

mainDataConstructors :: Environment (DataConstructorInfo ())
mainDataConstructors =
  Environment.fromList
    [
      ( "None"
      , DataConstructorInfo
          ()
          "None"
          (DataConstructor "None" 0 (Forall (Set.fromList [TypeIndex KType 0]) [] (TApplication KType (TConstructor (KArrow KType KType) "Option") (TVariable (TypeIndex KType 0) :| []))))
          (Set.fromList ["None", "Some"])
      )
    ,
      ( "Some"
      , DataConstructorInfo
          ()
          "Some"
          ( DataConstructor "Some" 1 (Forall (Set.fromList [TypeIndex KType 0]) [] (TVariable (TypeIndex KType 0) `TArrow` TApplication KType (TConstructor (KArrow KType KType) "Option") (TVariable (TypeIndex KType 0) :| [])))
          )
          (Set.fromList ["None", "Some"])
      )
    ]

mainTypeConstructors :: Environment (TypeConstructorInfo ())
mainTypeConstructors =
  Environment.fromList
    [
      ( "Option"
      , TypeConstructorInfo () "Option" (KArrow KType KType)
      )
    ]

mainCodataAccessors :: Environment (CodataAccessorInfo ())
mainCodataAccessors =
  Environment.fromList
    [
      ( "Head"
      , CodataAccessorInfo
          ()
          "Head"
          ( CodataAccessor
              "Head"
              (Forall (Set.fromList [TypeIndex KType 0]) [] (TApplication KType (TConstructor (KArrow KType KType) "Stream") (TVariable (TypeIndex KType 0) :| []) `TArrow` TVariable (TypeIndex KType 0)))
          )
      )
    ,
      ( "Tail"
      , CodataAccessorInfo
          ()
          "Tail"
          ( CodataAccessor
              "Tail"
              (Forall (Set.fromList [TypeIndex KType 0]) [] (TApplication KType (TConstructor (KArrow KType KType) "Stream") (TVariable (TypeIndex KType 0) :| []) `TArrow` TApplication KType (TConstructor (KArrow KType KType) "Stream") (TVariable (TypeIndex KType 0) :| [])))
          )
      )
    ]

mainCotypeConstructors :: Environment (CotypeConstructorInfo ())
mainCotypeConstructors =
  Environment.fromList
    [
      ( "Stream"
      , CotypeConstructorInfo
          ()
          "Stream"
          (KArrow KType KType)
      )
    ]

runBundle :: [FilePath] -> IO (Either CompilerFailureMode [ModuleBundle])
runBundle names = do
  (r, _, _) <- runCompilerT emptyCompilerEnvironment prog
  pure r
 where
  prog = do
    s <- runPass (parsingPhase >-> passTopologicalSort) names
    forM s build
