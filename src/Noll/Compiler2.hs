{-# LANGUAGE OverloadedStrings #-}

module Noll.Compiler2 where

import Noll.Set3.Test13x (moduleCore1)
import Noll.Compiler.Lowpass.TranslateModule (translateModule)
import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Noll.Language
import Noll.Module (Constant (..), Definition (..), Function (..), Module (..), Path (..))
import Noll.Compiler.Lowpass.Environment (initialTranslateEnvironment)
import Control.Monad.Reader (runReader)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Noll.Module as Module
import qualified Lang.Lowpass.Compiler as Lowpass
import qualified Lang.Lowpass.Compiler.Utils as Lowpass
import qualified Lang.Lowpass.Language as Lowpass
import qualified Lang.Common.Environment as Environment

progx_04 :: [Module () Kind IndexedType]
progx_04 = 
  [ moduleMain
  ]

moduleMain :: Module () Kind IndexedType
moduleMain =
  Module.fromDefinitionList
    (Path ["Main"])
    []
    [ DImport (Path ["Core$"]) ["trace_string"]
    , DFunction
        "main"
        ( Function
            ()
            (With [] (TVariable (TypeIndex KType 0)))
            (PLiteral () LUnit :| [])
            ( EApplication
                ()
                (TVariable (TypeIndex KType 0))
                (EVariable () (Label (TIntrinsic IString `TArrow` TVariable (TypeIndex KType 0)) "trace_string"))
                ( ELiteral () (LString "Hello, world!")
                    :| []
                )
            )
        )
    ]

moduleMain2 :: Module () Kind IndexedType
moduleMain2 =
  Module.fromDefinitionList
    (Path ["Main"])
    []
    [ DImport (Path ["Core$"]) ["trace_string"]
    , DConstant
        "main"
        ( Constant
            ()
            (With [] (TIntrinsic IUnit `TArrow` TVariable (TypeIndex KType 0)))
            ( ELambda
                ()
                (PLiteral () LUnit :| [])
                ( EApplication
                    ()
                    (TVariable (TypeIndex KType 0))
                    (EVariable () (Label (TIntrinsic IString `TArrow` TVariable (TypeIndex KType 0)) "trace_string"))
                    ( ELiteral () (LString "Hello, world!")
                        :| []
                    )
                )
            )
        )
    ]

banan1 =
  Lowpass.compileModules (runReader (traverse translateModule Noll.Compiler2.progx_04) testNameEnvironment)

banan2 :: IO ()
banan2 = Lowpass.testModules =<< Lowpass.compileModules (moduleCore1 : xs)
  where
    xs = runReader (traverse translateModule Noll.Compiler2.progx_04) testNameEnvironment

testNameEnvironment =
  initialTranslateEnvironment
    ( Environment.fromList
        [
          ( "always"
          , "Core$.always"
          )
        ,
          ( "trace"
          , "trace"
          )
        ,
          ( "@@@_trace_int32"
          , "Core$.trace_int32"
          )
        ,
          ( "not"
          , "Core$.operator__not"
          )
        ]
    )
