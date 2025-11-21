{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.BuildSpec (buildSpec, runBuild) where

import Coal.AST.Metadata (Metadata (..))
import Coal.Common.Environment (Environment (..), mapEnvironment)
import qualified Coal.Common.Environment as Environment
import Coal.Compiler.Build
import Coal.Compiler.Build.Internal
import Coal.Compiler.Environment
import Coal.Compiler.Pass (Pass (..), (>->))
import Coal.Compiler.Pass.ParsingPhase (parsingPhase)
import Coal.Compiler.Pass.PreflightPhase.TopologicalSort (passTopologicalSort)
import Coal.Compiler.Stack
import Coal.Language
import Coal.Language.Module
import Data.Either (rights)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Extras (forM, (<$$>))
import Test.Hspec

buildSpec :: Spec
buildSpec = do
  undefined

--  res <- runIO $ runBuild ["./lang/Nat.coal", "./lang/IO.coal", "./test/Coal/examples/133/Main.coal"]
--  let build : _ = filter (\ModuleBuild{..} -> modulePath == Path ["Main"]) (rights (sequence res))
--  test133 build
--
--  res <- runIO $ runBuild ["./lang/Nat.coal", "./lang/IO.coal", "./test/Coal/examples/134/Main.coal"]
--  let build : _ = filter (\ModuleBuild{..} -> modulePath == Path ["Main"]) (rights (sequence res))
--  test134 build
--
-- test133 :: ModuleBuild Metadata -> Spec
-- test133 build@ModuleBuild{..} = do
--  describe "DataConstructors" $ do
--    it "" $
--      mapEnvironment stripMeta (exportedDataConstructors build) == mainExportedDataConstructors
--
--  describe "TypeConstructors" $ do
--    it "" $
--      mapEnvironment stripMeta (exportedTypeConstructors build) == mainExportedTypeConstructors
--
--  describe "CodataAccessors" $ do
--    it "" $
--      mapEnvironment stripMeta (exportedCodataAccessors build) == mainExportedCodataAccessors
--
--  describe "CotypeConstructorEntry" $ do
--    it "" $
--      mapEnvironment stripMeta (exportedCotypeConstructors build) == mainCotypeConstructors
--
--  describe "Names" $ do
--    it "" $
--      exportedNames build == mainNames
--
--  describe "Exports" $ do
--    it "" $
--      moduleExports == Set.fromList ["Head", "None", "Option", "Some", "Stream", "Tail", "main"]
--
-- test134 :: ModuleBuild Metadata -> Spec
-- test134 build@ModuleBuild{..} = do
--  describe "DataConstructors" $ do
--    it "" $
--      mapEnvironment stripMeta (exportedDataConstructors build) == mainExportedDataConstructors
--
--  describe "TypeConstructors" $ do
--    it "" $
--      mapEnvironment stripMeta moduleTypeConstructors == mainTypeConstructors
--
--  describe "TypeConstructors" $ do
--    it "" $
--      mapEnvironment stripMeta (exportedTypeConstructors build) == mainExportedTypeConstructors
--
--  describe "CodataAccessors" $ do
--    it "" $
--      mapEnvironment stripMeta moduleCodataAccessors == mainExportedCodataAccessors
--
--  describe "CotypeConstructorEntry" $ do
--    it "" $
--      mapEnvironment stripMeta moduleCotypeConstructors == mainCotypeConstructors
--
--  describe "TraitInfo" $
--    it "" $
--      mapEnvironment stripMeta moduleTraits == mainTraits
--
--  describe "InstanceInfo" $ do
--    it "" $
--      Environment.mapEnvironment (fmap stripMeta) (exportedInstances build) == mainExportedInstances
--
--  describe "Names" $ do
--    it "" $
--      exportedNames build == mainNames2
--
--  describe "Exports" $ do
--    it "" $
--      moduleExports == Set.fromList ["Functor", "Head", "None", "Option", "Some", "Stream", "Tail", "map", "main"]
--
-- class StripMeta i where
--  stripMeta :: i a -> i ()
--
-- instance StripMeta DataConstructorEntry where
--  stripMeta (DataConstructorEntry _ n c s) = DataConstructorEntry () n c s
--
-- instance StripMeta TypeConstructorEntry where
--  stripMeta (TypeConstructorEntry _ n k ns) = TypeConstructorEntry () n k ns
--
-- instance StripMeta CodataAccessorInfo where
--  stripMeta (CodataAccessorInfo _ n a) = CodataAccessorInfo () n a
--
-- instance StripMeta CotypeConstructorEntry where
--  stripMeta (CotypeConstructorEntry _ n k ns) = CotypeConstructorEntry () n k ns
--
-- instance StripMeta TraitInfo where
--  stripMeta (TraitInfo _ n t d) = TraitInfo () n t d
--
-- instance StripMeta InstanceInfo where
--  stripMeta (InstanceInfo _ t t2 d) = InstanceInfo () t t2 d
--
-- mainNames :: [NameInfo]
-- mainNames =
--    [
--      IType "Option" (KArrow KType KType)
--      , ICotype "Stream" (KArrow KType KType)
--      , IDataConstructor "None" (Forall (Set.fromList [TypeIndex KType 0]) [] (TApplication KType (TConstructor (KArrow KType KType) "Option") (TVariable (TypeIndex KType 0) :| [])))
--      , IDataConstructor "Some" (Forall (Set.fromList [TypeIndex KType 0]) [] (TVariable (TypeIndex KType 0) `TArrow` TApplication KType (TConstructor (KArrow KType KType) "Option") (TVariable (TypeIndex KType 0) :| [])))
--      , ICodataAccessor "Head" (Forall (Set.fromList [TypeIndex KType 0]) [] (TApplication KType (TConstructor (KArrow KType KType) "Stream") (TVariable (TypeIndex KType 0) :| []) `TArrow` TVariable (TypeIndex KType 0)))
--      , ICodataAccessor "Tail" (Forall (Set.fromList [TypeIndex KType 0]) [] (TApplication KType (TConstructor (KArrow KType KType) "Stream") (TVariable (TypeIndex KType 0) :| []) `TArrow` TApplication KType (TConstructor (KArrow KType KType) "Stream") (TVariable (TypeIndex KType 0) :| [])))
--      , IFunctionPlaceholder "main"
--    ]
--
-- mainNames2 :: [NameInfo]
-- mainNames2 =
--    [
--      IType "Option" (KArrow KType KType)
--      , ICotype "Stream" (KArrow KType KType)
--      , IDataConstructor "None" (Forall (Set.fromList [TypeIndex KType 0]) [] (TApplication KType (TConstructor (KArrow KType KType) "Option") (TVariable (TypeIndex KType 0) :| [])))
--      , IDataConstructor "Some" (Forall (Set.fromList [TypeIndex KType 0]) [] (TVariable (TypeIndex KType 0) `TArrow` TApplication KType (TConstructor (KArrow KType KType) "Option") (TVariable (TypeIndex KType 0) :| [])))
--      , ICodataAccessor "Head" (Forall (Set.fromList [TypeIndex KType 0]) [] (TApplication KType (TConstructor (KArrow KType KType) "Stream") (TVariable (TypeIndex KType 0) :| []) `TArrow` TVariable (TypeIndex KType 0)))
--      , ICodataAccessor "Tail" (Forall (Set.fromList [TypeIndex KType 0]) [] (TApplication KType (TConstructor (KArrow KType KType) "Stream") (TVariable (TypeIndex KType 0) :| []) `TArrow` TApplication KType (TConstructor (KArrow KType KType) "Stream") (TVariable (TypeIndex KType 0) :| [])))
--      , ITrait "Functor"
--      , IFunction
--         "map"
--          ( Forall
--              (Set.fromList [TypeIndex (KArrow KType KType) 0, TypeIndex KType 1, TypeIndex KType 2])
--              [Trait "Functor" (TVariable (TypeIndex (KArrow KType KType) 0))]
--              ( (TVariable (TypeIndex KType 1) `TArrow` TVariable (TypeIndex KType 2))
--                  `TArrow` TApplication KType (TVariable (TypeIndex (KArrow KType KType) 0)) (TVariable (TypeIndex KType 1) :| [])
--                  `TArrow` TApplication KType (TVariable (TypeIndex (KArrow KType KType) 0)) (TVariable (TypeIndex KType 2) :| [])
--              )
--          )
--      , IFunctionPlaceholder "main"
--    ]
--
-- mainExportedDataConstructors :: Environment (DataConstructorEntry ())
-- mainExportedDataConstructors =
--  Environment.fromList
--    [
--      ( "None"
--      , DataConstructorEntry
--          ()
--          "None"
--          (DataConstructor "None" 0 (Forall (Set.fromList [TypeIndex KType 0]) [] (TApplication KType (TConstructor (KArrow KType KType) "Option") (TVariable (TypeIndex KType 0) :| []))))
--          (Set.fromList ["None", "Some"])
--      )
--    ,
--      ( "Some"
--      , DataConstructorEntry
--          ()
--          "Some"
--          ( DataConstructor "Some" 1 (Forall (Set.fromList [TypeIndex KType 0]) [] (TVariable (TypeIndex KType 0) `TArrow` TApplication KType (TConstructor (KArrow KType KType) "Option") (TVariable (TypeIndex KType 0) :| [])))
--          )
--          (Set.fromList ["None", "Some"])
--      )
--    ]
--
-- mainTypeConstructors :: Environment (TypeConstructorEntry ())
-- mainTypeConstructors =
--  Environment.fromList
--    [
--      ( "Option"
--      , TypeConstructorEntry () "Option" (KArrow KType KType) ["Some", "None"]
--      )
--    ,
--      ( "List"
--      , TypeConstructorEntry () "List" (KArrow KType KType) []
--      )
--    ]
--
-- mainExportedTypeConstructors :: Environment (TypeConstructorEntry ())
-- mainExportedTypeConstructors =
--  Environment.fromList
--    [
--      ( "Option"
--      , TypeConstructorEntry () "Option" (KArrow KType KType) ["Some", "None"]
--      )
--    ]
--
-- mainExportedCodataAccessors :: Environment (CodataAccessorInfo ())
-- mainExportedCodataAccessors =
--  Environment.fromList
--    [
--      ( "Head"
--      , CodataAccessorInfo
--          ()
--          "Head"
--          ( CodataAccessor
--              "Head"
--              (Forall (Set.fromList [TypeIndex KType 0]) [] (TApplication KType (TConstructor (KArrow KType KType) "Stream") (TVariable (TypeIndex KType 0) :| []) `TArrow` TVariable (TypeIndex KType 0)))
--          )
--      )
--    ,
--      ( "Tail"
--      , CodataAccessorInfo
--          ()
--          "Tail"
--          ( CodataAccessor
--              "Tail"
--              (Forall (Set.fromList [TypeIndex KType 0]) [] (TApplication KType (TConstructor (KArrow KType KType) "Stream") (TVariable (TypeIndex KType 0) :| []) `TArrow` TApplication KType (TConstructor (KArrow KType KType) "Stream") (TVariable (TypeIndex KType 0) :| [])))
--          )
--      )
--    ]
--
-- mainCotypeConstructors :: Environment (CotypeConstructorEntry ())
-- mainCotypeConstructors =
--  Environment.fromList
--    [
--      ( "Stream"
--      , CotypeConstructorEntry
--          ()
--          "Stream"
--          (KArrow KType KType)
--          ["Head", "Tail"]
--      )
--    ]
--
-- mainTraits :: Environment (TraitInfo ())
-- mainTraits =
--  Environment.fromList
--    [
--      ( "Functor"
--      , TraitInfo
--          ()
--          "Functor"
--          (Parameter (KArrow KType KType) "f")
--          ( Environment.fromList
--              [
--                ( "map"
--                , Forall
--                    (Set.fromList [Parameter () "f", Parameter () "a", Parameter () "b"])
--                    []
--                    ( (TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "b"))
--                        `TArrow` TApplication () (TVariable (Parameter () "f")) (TVariable (Parameter () "a") :| [])
--                        `TArrow` TApplication () (TVariable (Parameter () "f")) (TVariable (Parameter () "b") :| [])
--                    )
--                )
--              ]
--          )
--      )
--    ]
--
-- mainExportedInstances :: Environment (Map IndexedType (InstanceInfo ()))
-- mainExportedInstances =
--  Environment.fromList
--    [
--      ( "Functor"
--      , Map.fromList
--          [
--            ( TConstructor (KArrow KType KType) "List"
--            , InstanceInfo
--                ()
--                (TConstructor () "List")
--                (TConstructor (KArrow KType KType) "List")
--                ( Map.fromList
--                    [
--                      ( "map"
--                      , Forall
--                          (Set.fromList [TypeIndex KType 1, TypeIndex KType 2])
--                          []
--                          ( (TVariable (TypeIndex KType 1) `TArrow` TVariable (TypeIndex KType 2))
--                              `TArrow` TApplication KType (TConstructor (KArrow KType KType) "List") (TVariable (TypeIndex KType 1) :| [])
--                              `TArrow` TApplication KType (TConstructor (KArrow KType KType) "List") (TVariable (TypeIndex KType 2) :| [])
--                          )
--                      )
--                    ]
--                )
--            )
--          ,
--            ( TConstructor (KArrow KType KType) "Option"
--            , InstanceInfo
--                ()
--                (TConstructor () "Option")
--                (TConstructor (KArrow KType KType) "Option")
--                ( Map.fromList
--                    [
--                      ( "map"
--                      , Forall
--                          (Set.fromList [TypeIndex KType 1, TypeIndex KType 2])
--                          []
--                          ( (TVariable (TypeIndex KType 1) `TArrow` TVariable (TypeIndex KType 2))
--                              `TArrow` TApplication KType (TConstructor (KArrow KType KType) "Option") (TVariable (TypeIndex KType 1) :| [])
--                              `TArrow` TApplication KType (TConstructor (KArrow KType KType) "Option") (TVariable (TypeIndex KType 2) :| [])
--                          )
--                      )
--                    ]
--                )
--            )
--          ]
--      )
--    ]

runBuild :: [FilePath] -> IO (Either CompilerFailureMode [ModuleBuild Metadata])
runBuild names = do
  (r, _, _) <- runCompilerT emptyCompilerEnvironment prog
  pure (snd <$$> r)
 where
  prog = do
    s <- runPass (parsingPhase >-> passTopologicalSort) names
    forM s prepareBuild
