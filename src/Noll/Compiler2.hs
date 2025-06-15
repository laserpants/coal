{-# LANGUAGE OverloadedStrings #-}

module Noll.Compiler2 where

import Control.Monad.Reader (runReader)
import Lang.Common.List1 (NonEmpty (..), (<|))
import Lang.Label (Label (..))
import Noll.Compiler.Lowpass.Environment (initialTranslateEnvironment)
import Noll.Compiler.Lowpass.TranslateModule (translateModule)
import Noll.Language
import Noll.Module (Constant (..), Definition (..), Function (..), Module (..), Path (..))
import Noll.Set3.Test13x (moduleCore1)

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Lang.Common.Environment as Environment
import qualified Lang.Lowpass.Compiler as Lowpass
import qualified Lang.Lowpass.Compiler.Utils as Lowpass
import qualified Lang.Lowpass.Language as Lowpass
import qualified Noll.Module as Module

progx_05 :: [Module () () ()]
progx_05 =
  [ moduleMainB
  ]

moduleMainB :: Module () () ()
moduleMainB =
  Module.fromDefinitionList
    (Path ["Main"])
    []
    [ DImport (Path ["Core$"]) ["trace_string"]
    , DCodata
        "Stream"
        [Parameter () "a"]
        [
          ( "Head"
          , TVariable (Parameter () "a")
          )
        ,
          ( "Tail"
          , TApplication () (TConstructor () "Stream") (TVariable (Parameter () "a") :| [])
          )
        ]
    , DConstant
        "nats"
        ( Constant
            ()
            (With [] ())
            ( EUnfold
                ()
                ()
                (Label () "Stream")
                "f"
                (PVariable () (Label () "n") :| [])
                ( Map.fromList
                    [
                      ( "Head"
                      , EVariable () (Label () "n")
                      )
                    ,
                      ( "Tail"
                      , EApplication
                          ()
                          ()
                          (EVariable () (Label () "f"))
                          ( EApplication
                              ()
                              ()
                              (EBinaryOperator () () OAddition)
                              ( EVariable () (Label () "n")
                                  <| ELiteral () (LInt32 1)
                                  :| []
                              )
                              :| []
                          )
                      )
                    ]
                )
                Nothing
            )
        )
    , DFunction
        "nth"
        ( Function
            ()
            (With [] ())
            (PVariable () (Label () "n") :| [])
            ( EFold
                ()
                ()
                (EVariable () (Label () "n") :| [])
                ( EClause
                    ()
                    ( PConstructor
                        ()
                        (Label () "Zero")
                        []
                    )
                    ( CPlain
                        ()
                        []
                        ( ELambda
                            ()
                            (PVariable () (Label () "stream") :| [])
                            ( ECodataSelect
                                ()
                                (Label () "Head")
                                (EVariable () (Label () "stream"))
                                Nothing
                            )
                        )
                        :| []
                    )
                    <| EClause
                      ()
                      ( PConstructor
                          ()
                          (Label () "Succ")
                          [ PAtVariable () (Label () "f")
                          ]
                      )
                      ( CPlain
                          ()
                          []
                          ( ELambda
                              ()
                              (PVariable () (Label () "stream") :| [])
                              ( EApplication
                                  ()
                                  ()
                                  (EVariable () (Label () "f"))
                                  ( ECodataSelect
                                      ()
                                      (Label () "Tail")
                                      (EVariable () (Label () "stream"))
                                      Nothing
                                      :| []
                                  )
                              )
                          )
                          :| []
                      )
                    :| []
                )
                Nothing
            )
        )
    , DFunction
        "main"
        ( Function
            ()
            (With [] ())
            (PLiteral () LUnit :| [])
            ( ELet
                ()
                ( BPattern
                    ()
                    (PVariable () (Label () "v"))
                    ( EApplication
                        ()
                        ()
                        (EVariable () (Label () "nth"))
                        ( ELiteral () (LInt32 5)
                            <| EVariable () (Label () "nats")
                            :| []
                        )
                    )
                    :| []
                )
                ( EApplication
                    ()
                    ()
                    (EVariable () (Label () "trace_string"))
                    ( EVariable () (Label () "v")
                        :| []
                    )
                )
            )
        )
    ]

---
---
---
---

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
