{-# LANGUAGE OverloadedStrings #-}

module Coal.ProtoCompiler.ProtoBuildSpec where

import qualified Coal.Common.Environment as Environment
import Coal.Common.Label (Label (..))
import Coal.Language
import Coal.Language.Module.Import (Import (..))
import Coal.Language.Module.Path (Path (..))
import Coal.Language.Type (Parameter (..))
import Coal.Language.Type.Kind.Indexed (ToKindIndexed (..))
import Coal.ProtoCompiler.KindEnvironment (moduleKindEnvironment)
import Coal.ProtoCompiler.ProtoBuild
import Coal.ProtoCompiler.ProtoBuild.ProtoPrep (protoOprepareBuild)
import Coal.ProtoCompiler.ProtoStack (ProtoCompilerT, evalProtoCompilerT)
import Coal.ProtoLanguage.ProtoDefinition
import Coal.ProtoLanguage.ProtoModule (ModuleExportList (..), ProtoModule (..))
import Coal.ProtoTypeSystem.Kind.Constraint.Generation
import Coal.ProtoTypeSystem.Kind.Constraint.Solver (protoOsolveKindConstraints)
import Coal.ProtoTypeSystem.Kind.Error (ProtoKindError (..))
import Coal.ProtoTypeSystem.Kind.Substitution
import Coal.ProtoTypeSystem.Kind.Unification
import Control.Monad.State (evalState, evalStateT)
import Data.Either (rights)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.Set as Set
import Debug.Trace
import Extras (Name)
import Text.Pretty.Simple (pPrint)

testModuleBuiltinsPreKinds :: (Monoid a) => ProtoModule a () ()
testModuleBuiltinsPreKinds =
  ProtoModule
    { protoOmodulePath = Path ["Builtin"]
    , protoOmoduleExportList = ExportAll
    , protoOmoduleDefinitions =
        [ ProtoDTrait
            mempty
            "Numeric"
            ( ProtoTraitDefinition
                { protoOtraitDefinitionMetadata =
                    mempty
                , protoOtraitDefinitionTraitName =
                    "Numeric"
                , protoOtraitDefinitionConstraints =
                    []
                , protoOtraitDefinitionParameter =
                    Parameter () "a"
                , protoOtraitDefinitionInterface =
                    [
                      ( "from_int32"
                      , Forall
                          (Set.fromList [Parameter () "a"])
                          [Trait "Numeric" (TVariable (Parameter () "a"))]
                          (TIntrinsic IInt32 `TArrow` TVariable (Parameter () "a"))
                      )
                    ,
                      ( "from_int64"
                      , Forall
                          (Set.fromList [Parameter () "a"])
                          [Trait "Numeric" (TVariable (Parameter () "a"))]
                          (TIntrinsic IInt64 `TArrow` TVariable (Parameter () "a"))
                      )
                    ,
                      ( "from_bignum"
                      , Forall
                          (Set.fromList [Parameter () "a"])
                          [Trait "Numeric" (TVariable (Parameter () "a"))]
                          (TIntrinsic IBignum `TArrow` TVariable (Parameter () "a"))
                      )
                    ,
                      ( "negate"
                      , Forall
                          (Set.fromList [Parameter () "a"])
                          [Trait "Numeric" (TVariable (Parameter () "a"))]
                          (TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a"))
                      )
                    ,
                      ( "(+)"
                      , Forall
                          (Set.fromList [Parameter () "a"])
                          [Trait "Numeric" (TVariable (Parameter () "a"))]
                          (TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a"))
                      )
                    ,
                      ( "(-)"
                      , Forall
                          (Set.fromList [Parameter () "a"])
                          [Trait "Numeric" (TVariable (Parameter () "a"))]
                          (TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a"))
                      )
                    ,
                      ( "(*)"
                      , Forall
                          (Set.fromList [Parameter () "a"])
                          [Trait "Numeric" (TVariable (Parameter () "a"))]
                          (TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a"))
                      )
                    ]
                }
            )
        , ProtoDInstance
            mempty
            ( ProtoInstanceDefinition
                { protoOinstanceDefinitionMetadata =
                    mempty
                , protoOinstanceDefinitionConstraints =
                    []
                , protoOinstanceDefinitionTraitName =
                    "Numeric"
                , protoOinstanceDefinitionType =
                    TIntrinsic IInt32
                , protoOinstanceDefinitionImplementations =
                    [ ProtoDFunction
                        mempty
                        "from_int32"
                        ( ProtoFunctionDefinition
                            { protoOfunctionDefinitionMetadata =
                                mempty
                            , protoOfunctionDefinitionAnnotation =
                                Nothing
                            , protoOfunctionDefinitionType =
                                With [] ()
                            , protoOfunctionDefinitionPatterns =
                                PVariable mempty (Label () "n") :| []
                            , protoOfunctionDefinitionExpression =
                                EVariable mempty (Label () "n")
                            }
                        )
                    , ProtoDFunction
                        mempty
                        "from_int64"
                        ( ProtoFunctionDefinition
                            { protoOfunctionDefinitionMetadata =
                                mempty
                            , protoOfunctionDefinitionAnnotation =
                                Nothing
                            , protoOfunctionDefinitionType =
                                With [] ()
                            , protoOfunctionDefinitionPatterns =
                                PVariable mempty (Label () "n") :| []
                            , protoOfunctionDefinitionExpression =
                                EApplication
                                  mempty
                                  ()
                                  (EVariable mempty (Label () "bultin$_int64_to_int32"))
                                  ( EVariable mempty (Label () "n")
                                      :| []
                                  )
                            }
                        )
                    , ProtoDFunction
                        mempty
                        "from_bignum"
                        ( ProtoFunctionDefinition
                            { protoOfunctionDefinitionMetadata =
                                mempty
                            , protoOfunctionDefinitionAnnotation =
                                Nothing
                            , protoOfunctionDefinitionType =
                                With [] ()
                            , protoOfunctionDefinitionPatterns =
                                PVariable mempty (Label () "n") :| []
                            , protoOfunctionDefinitionExpression =
                                EApplication
                                  mempty
                                  ()
                                  (EVariable mempty (Label () "bultin$_bignum_to_int32"))
                                  ( EVariable mempty (Label () "n")
                                      :| []
                                  )
                            }
                        )
                    , ProtoDFunction
                        mempty
                        "negate"
                        ( ProtoFunctionDefinition
                            { protoOfunctionDefinitionMetadata =
                                mempty
                            , protoOfunctionDefinitionAnnotation =
                                Nothing
                            , protoOfunctionDefinitionType =
                                With [] ()
                            , protoOfunctionDefinitionPatterns =
                                PVariable mempty (Label () "n") :| []
                            , protoOfunctionDefinitionExpression =
                                EApplication
                                  mempty
                                  ()
                                  (EVariable mempty (Label () "bultin$_int32_neg"))
                                  ( EVariable mempty (Label () "n")
                                      :| []
                                  )
                            }
                        )
                    , ProtoDFunction
                        mempty
                        "(+)"
                        ( ProtoFunctionDefinition
                            { protoOfunctionDefinitionMetadata =
                                mempty
                            , protoOfunctionDefinitionAnnotation =
                                Nothing
                            , protoOfunctionDefinitionType =
                                With [] ()
                            , protoOfunctionDefinitionPatterns =
                                PVariable mempty (Label () "m") <| PVariable mempty (Label () "n") :| []
                            , protoOfunctionDefinitionExpression =
                                EApplication
                                  mempty
                                  ()
                                  (EVariable mempty (Label () "bultin$_int32_add"))
                                  ( EVariable mempty (Label () "m")
                                      <| EVariable mempty (Label () "n")
                                      :| []
                                  )
                            }
                        )
                    , ProtoDFunction
                        mempty
                        "(-)"
                        ( ProtoFunctionDefinition
                            { protoOfunctionDefinitionMetadata =
                                mempty
                            , protoOfunctionDefinitionAnnotation =
                                Nothing
                            , protoOfunctionDefinitionType =
                                With [] ()
                            , protoOfunctionDefinitionPatterns =
                                PVariable mempty (Label () "m") <| PVariable mempty (Label () "n") :| []
                            , protoOfunctionDefinitionExpression =
                                EApplication
                                  mempty
                                  ()
                                  (EVariable mempty (Label () "bultin$_int32_sub"))
                                  ( EVariable mempty (Label () "m")
                                      <| EVariable mempty (Label () "n")
                                      :| []
                                  )
                            }
                        )
                    , ProtoDFunction
                        mempty
                        "(*)"
                        ( ProtoFunctionDefinition
                            { protoOfunctionDefinitionMetadata =
                                mempty
                            , protoOfunctionDefinitionAnnotation =
                                Nothing
                            , protoOfunctionDefinitionType =
                                With [] ()
                            , protoOfunctionDefinitionPatterns =
                                PVariable mempty (Label () "m") <| PVariable mempty (Label () "n") :| []
                            , protoOfunctionDefinitionExpression =
                                EApplication
                                  mempty
                                  ()
                                  (EVariable mempty (Label () "bultin$_int32_mul"))
                                  ( EVariable mempty (Label () "m")
                                      <| EVariable mempty (Label () "n")
                                      :| []
                                  )
                            }
                        )
                    ]
                }
            )
        ]
    }

testModuleBuiltins :: (Monoid a) => ProtoModule a Kind ()
testModuleBuiltins =
  ProtoModule
    { protoOmodulePath = Path ["Builtin"]
    , protoOmoduleExportList = ExportAll
    , protoOmoduleDefinitions =
        [ ProtoDTrait
            mempty
            "Numeric"
            ( ProtoTraitDefinition
                { protoOtraitDefinitionMetadata =
                    mempty
                , protoOtraitDefinitionTraitName =
                    "Numeric"
                , protoOtraitDefinitionConstraints =
                    []
                , protoOtraitDefinitionParameter =
                    Parameter KType "a"
                , protoOtraitDefinitionInterface =
                    [
                      ( "from_int32"
                      , Forall
                          (Set.fromList [Parameter KType "a"])
                          [Trait "Numeric" (TVariable (Parameter KType "a"))]
                          (TIntrinsic IInt32 `TArrow` TVariable (Parameter KType "a"))
                      )
                    ,
                      ( "from_int64"
                      , Forall
                          (Set.fromList [Parameter KType "a"])
                          [Trait "Numeric" (TVariable (Parameter KType "a"))]
                          (TIntrinsic IInt64 `TArrow` TVariable (Parameter KType "a"))
                      )
                    ,
                      ( "from_bignum"
                      , Forall
                          (Set.fromList [Parameter KType "a"])
                          [Trait "Numeric" (TVariable (Parameter KType "a"))]
                          (TIntrinsic IBignum `TArrow` TVariable (Parameter KType "a"))
                      )
                    ,
                      ( "negate"
                      , Forall
                          (Set.fromList [Parameter KType "a"])
                          [Trait "Numeric" (TVariable (Parameter KType "a"))]
                          (TVariable (Parameter KType "a") `TArrow` TVariable (Parameter KType "a"))
                      )
                    ,
                      ( "(+)"
                      , Forall
                          (Set.fromList [Parameter KType "a"])
                          [Trait "Numeric" (TVariable (Parameter KType "a"))]
                          (TVariable (Parameter KType "a") `TArrow` TVariable (Parameter KType "a") `TArrow` TVariable (Parameter KType "a"))
                      )
                    ,
                      ( "(-)"
                      , Forall
                          (Set.fromList [Parameter KType "a"])
                          [Trait "Numeric" (TVariable (Parameter KType "a"))]
                          (TVariable (Parameter KType "a") `TArrow` TVariable (Parameter KType "a") `TArrow` TVariable (Parameter KType "a"))
                      )
                    ,
                      ( "(*)"
                      , Forall
                          (Set.fromList [Parameter KType "a"])
                          [Trait "Numeric" (TVariable (Parameter KType "a"))]
                          (TVariable (Parameter KType "a") `TArrow` TVariable (Parameter KType "a") `TArrow` TVariable (Parameter KType "a"))
                      )
                    ]
                }
            )
        , ProtoDInstance
            mempty
            ( ProtoInstanceDefinition
                { protoOinstanceDefinitionMetadata =
                    mempty
                , protoOinstanceDefinitionConstraints =
                    []
                , protoOinstanceDefinitionTraitName =
                    "Numeric"
                , protoOinstanceDefinitionType =
                    TIntrinsic IInt32
                , protoOinstanceDefinitionImplementations =
                    [ ProtoDFunction
                        mempty
                        "from_int32"
                        ( ProtoFunctionDefinition
                            { protoOfunctionDefinitionMetadata =
                                mempty
                            , protoOfunctionDefinitionAnnotation =
                                Nothing
                            , protoOfunctionDefinitionType =
                                With [] ()
                            , protoOfunctionDefinitionPatterns =
                                PVariable mempty (Label () "n") :| []
                            , protoOfunctionDefinitionExpression =
                                EVariable mempty (Label () "n")
                            }
                        )
                    , ProtoDFunction
                        mempty
                        "from_int64"
                        ( ProtoFunctionDefinition
                            { protoOfunctionDefinitionMetadata =
                                mempty
                            , protoOfunctionDefinitionAnnotation =
                                Nothing
                            , protoOfunctionDefinitionType =
                                With [] ()
                            , protoOfunctionDefinitionPatterns =
                                PVariable mempty (Label () "n") :| []
                            , protoOfunctionDefinitionExpression =
                                EApplication
                                  mempty
                                  ()
                                  (EVariable mempty (Label () "bultin$_int64_to_int32"))
                                  ( EVariable mempty (Label () "n")
                                      :| []
                                  )
                            }
                        )
                    , ProtoDFunction
                        mempty
                        "from_bignum"
                        ( ProtoFunctionDefinition
                            { protoOfunctionDefinitionMetadata =
                                mempty
                            , protoOfunctionDefinitionAnnotation =
                                Nothing
                            , protoOfunctionDefinitionType =
                                With [] ()
                            , protoOfunctionDefinitionPatterns =
                                PVariable mempty (Label () "n") :| []
                            , protoOfunctionDefinitionExpression =
                                EApplication
                                  mempty
                                  ()
                                  (EVariable mempty (Label () "bultin$_bignum_to_int32"))
                                  ( EVariable mempty (Label () "n")
                                      :| []
                                  )
                            }
                        )
                    , ProtoDFunction
                        mempty
                        "negate"
                        ( ProtoFunctionDefinition
                            { protoOfunctionDefinitionMetadata =
                                mempty
                            , protoOfunctionDefinitionAnnotation =
                                Nothing
                            , protoOfunctionDefinitionType =
                                With [] ()
                            , protoOfunctionDefinitionPatterns =
                                PVariable mempty (Label () "n") :| []
                            , protoOfunctionDefinitionExpression =
                                EApplication
                                  mempty
                                  ()
                                  (EVariable mempty (Label () "bultin$_int32_neg"))
                                  ( EVariable mempty (Label () "n")
                                      :| []
                                  )
                            }
                        )
                    , ProtoDFunction
                        mempty
                        "(+)"
                        ( ProtoFunctionDefinition
                            { protoOfunctionDefinitionMetadata =
                                mempty
                            , protoOfunctionDefinitionAnnotation =
                                Nothing
                            , protoOfunctionDefinitionType =
                                With [] ()
                            , protoOfunctionDefinitionPatterns =
                                PVariable mempty (Label () "m") <| PVariable mempty (Label () "n") :| []
                            , protoOfunctionDefinitionExpression =
                                EApplication
                                  mempty
                                  ()
                                  (EVariable mempty (Label () "bultin$_int32_add"))
                                  ( EVariable mempty (Label () "m")
                                      <| EVariable mempty (Label () "n")
                                      :| []
                                  )
                            }
                        )
                    , ProtoDFunction
                        mempty
                        "(-)"
                        ( ProtoFunctionDefinition
                            { protoOfunctionDefinitionMetadata =
                                mempty
                            , protoOfunctionDefinitionAnnotation =
                                Nothing
                            , protoOfunctionDefinitionType =
                                With [] ()
                            , protoOfunctionDefinitionPatterns =
                                PVariable mempty (Label () "m") <| PVariable mempty (Label () "n") :| []
                            , protoOfunctionDefinitionExpression =
                                EApplication
                                  mempty
                                  ()
                                  (EVariable mempty (Label () "bultin$_int32_sub"))
                                  ( EVariable mempty (Label () "m")
                                      <| EVariable mempty (Label () "n")
                                      :| []
                                  )
                            }
                        )
                    , ProtoDFunction
                        mempty
                        "(*)"
                        ( ProtoFunctionDefinition
                            { protoOfunctionDefinitionMetadata =
                                mempty
                            , protoOfunctionDefinitionAnnotation =
                                Nothing
                            , protoOfunctionDefinitionType =
                                With [] ()
                            , protoOfunctionDefinitionPatterns =
                                PVariable mempty (Label () "m") <| PVariable mempty (Label () "n") :| []
                            , protoOfunctionDefinitionExpression =
                                EApplication
                                  mempty
                                  ()
                                  (EVariable mempty (Label () "bultin$_int32_mul"))
                                  ( EVariable mempty (Label () "m")
                                      <| EVariable mempty (Label () "n")
                                      :| []
                                  )
                            }
                        )
                    ]
                }
            )
        ]
    }

--

testModule0 :: (Monoid a) => ProtoModule a Kind ()
testModule0 =
  ProtoModule
    { protoOmodulePath = Path ["IO"]
    , protoOmoduleExportList = ExportAll
    , protoOmoduleDefinitions =
        [ ProtoDFunction
            mempty
            "println_int32"
            ( ProtoFunctionDefinition
                { protoOfunctionDefinitionMetadata = mempty
                , protoOfunctionDefinitionAnnotation =
                    Just
                      ( With
                          []
                          ( TApplication
                              KType
                              (TConstructor (KArrow KType KType) "IO")
                              (TIntrinsic IUnit)
                          )
                      )
                , protoOfunctionDefinitionType = With [] ()
                , protoOfunctionDefinitionPatterns =
                    PAnnotation
                      mempty
                      (TIntrinsic IInt32)
                      (PVariable mempty (Label () "n"))
                      :| []
                , protoOfunctionDefinitionExpression =
                    EApplication
                      mempty
                      ()
                      (EVariable mempty (Label () "io$_println_int32"))
                      ( EVariable mempty (Label () "n")
                          :| []
                      )
                }
            )
        ]
    }

testModule1 :: (Monoid a) => ProtoModule a Kind ()
testModule1 =
  ProtoModule
    { protoOmodulePath = Path ["Main"]
    , protoOmoduleExportList = ExportAll
    , protoOmoduleDefinitions =
        [ ProtoDImport
            mempty
            (Path ["Math"])
            [ NameImport mempty "factorial"
            ]
        , ProtoDImport
            mempty
            (Path ["IO"])
            [ NameImport mempty "println_int32"
            ]
        , ProtoDFunction
            mempty
            "main"
            ( ProtoFunctionDefinition
                { protoOfunctionDefinitionMetadata = mempty
                , protoOfunctionDefinitionAnnotation = Nothing
                , protoOfunctionDefinitionType = With [] ()
                , protoOfunctionDefinitionPatterns =
                    PLiteral mempty LUnit :| []
                , protoOfunctionDefinitionExpression =
                    EApplication
                      mempty
                      ()
                      (EVariable mempty (Label () "println_int32"))
                      ( EApplication
                          mempty
                          ()
                          (EVariable mempty (Label () "factorial"))
                          ( EApplication
                              mempty
                              ()
                              (EVariable mempty (Label () "from_int32"))
                              ( ELiteral mempty (LInt32 8)
                                  :| []
                              )
                              :| []
                          )
                          :| []
                      )
                }
            )
        ]
    }

testModule2 :: (Monoid a) => ProtoModule a Kind ()
testModule2 =
  ProtoModule
    { protoOmodulePath = Path ["Math"]
    , protoOmoduleExportList = ExportAll
    , protoOmoduleDefinitions =
        [ ProtoDImport
            mempty
            (Path ["Nat"])
            [ NameImport mempty "pack"
            , NameImport mempty "unpack"
            ]
        , ProtoDFunction
            mempty
            "factorial"
            ( ProtoFunctionDefinition
                { protoOfunctionDefinitionMetadata =
                    mempty
                , protoOfunctionDefinitionAnnotation =
                    Just (With [] (TIntrinsic IInt32))
                , protoOfunctionDefinitionType =
                    With [] ()
                , protoOfunctionDefinitionPatterns =
                    PAnnotation
                      mempty
                      (TIntrinsic IInt32)
                      (PVariable mempty (Label () "n"))
                      :| []
                , protoOfunctionDefinitionExpression =
                    EFold
                      mempty
                      ()
                      ( EApplication
                          mempty
                          ()
                          (EVariable mempty (Label () "pack"))
                          (EVariable mempty (Label () "n") :| [])
                          :| []
                      )
                      ( EClause
                          mempty
                          (PConstructor mempty (Label () "Zero") [])
                          ( CPlain
                              mempty
                              []
                              ( EApplication
                                  mempty
                                  ()
                                  (EVariable mempty (Label () "from_int32"))
                                  ( ELiteral mempty (LInt32 1)
                                      :| []
                                  )
                              )
                              :| []
                          )
                          <| EClause
                            mempty
                            ( PAs
                                mempty
                                (Label () "m")
                                ( PConstructor
                                    mempty
                                    (Label () "Succ")
                                    [ PAtVariable
                                        mempty
                                        (Label () "f")
                                    ]
                                )
                            )
                            ( CPlain
                                mempty
                                []
                                ( EApplication
                                    mempty
                                    ()
                                    (EVariable mempty (Label () "(*)"))
                                    ( EApplication
                                        mempty
                                        ()
                                        (EVariable mempty (Label () "unpack"))
                                        ( EVariable mempty (Label () "m")
                                            :| []
                                        )
                                        <| EVariable mempty (Label () "f")
                                        :| []
                                    )
                                )
                                :| []
                            )
                          :| []
                      )
                }
            )
        ]
    }

testModule3 :: (Monoid a) => ProtoModule a Kind ()
testModule3 =
  ProtoModule
    { protoOmodulePath = Path ["Nat"]
    , protoOmoduleExportList = ExportAll
    , protoOmoduleDefinitions =
        [ ProtoDFunction
            mempty
            "pack"
            ( ProtoFunctionDefinition
                { protoOfunctionDefinitionMetadata = mempty
                , protoOfunctionDefinitionAnnotation =
                    Just
                      ( With
                          []
                          (TIntrinsic INat)
                      )
                , protoOfunctionDefinitionType =
                    With [] ()
                , protoOfunctionDefinitionPatterns =
                    PAnnotation
                      mempty
                      (TIntrinsic IInt32)
                      (PVariable mempty (Label () "m"))
                      :| []
                , protoOfunctionDefinitionExpression =
                    EApplication
                      mempty
                      ()
                      (EVariable mempty (Label () "nat$_pack"))
                      ( EVariable mempty (Label () "m")
                          :| []
                      )
                }
            )
        , ProtoDFunction
            mempty
            "unpack"
            ( ProtoFunctionDefinition
                { protoOfunctionDefinitionMetadata = mempty
                , protoOfunctionDefinitionAnnotation =
                    Just
                      ( With
                          []
                          (TIntrinsic IInt32)
                      )
                , protoOfunctionDefinitionType =
                    With [] ()
                , protoOfunctionDefinitionPatterns =
                    PAnnotation
                      mempty
                      (TIntrinsic INat)
                      (PVariable mempty (Label () "n"))
                      :| []
                , protoOfunctionDefinitionExpression =
                    EApplication
                      mempty
                      ()
                      (EVariable mempty (Label () "nat$_unpack"))
                      ( EVariable mempty (Label () "n")
                          :| []
                      )
                }
            )
        ]
    }

--

testModule2B :: (Monoid a) => ProtoModule a Kind ()
testModule2B =
  ProtoModule
    { protoOmodulePath = Path ["Math"]
    , protoOmoduleExportList = ExportAll
    , protoOmoduleDefinitions =
        [ ProtoDImport
            mempty
            (Path ["Nat"])
            [ NameImport mempty "pack"
            , NameImport mempty "unpack"
            ]
        , ProtoDFunction
            mempty
            "factorial"
            ( ProtoFunctionDefinition
                { protoOfunctionDefinitionMetadata =
                    mempty
                , protoOfunctionDefinitionAnnotation =
                    Just (With [] (TIntrinsic IInt32))
                , protoOfunctionDefinitionType =
                    With [] ()
                , protoOfunctionDefinitionPatterns =
                    PAnnotation
                      mempty
                      (TIntrinsic IInt32)
                      (PVariable mempty (Label () "n"))
                      :| []
                , protoOfunctionDefinitionExpression =
                    ERecursiveLet
                      mempty
                      (PVariable mempty (Label () "$fold-70cdac64"))
                      ( ELambda
                          mempty
                          (PVariable mempty (Label () "$variable-185c7b8df7b0") :| [])
                          ( EMatch
                              mempty
                              ()
                              (EVariable mempty (Label () "$variable-185c7b8df7b0"))
                              ( EClause
                                  mempty
                                  (PConstructor mempty (Label () "Zero") [])
                                  ( CPlain
                                      mempty
                                      []
                                      ( EApplication
                                          mempty
                                          ()
                                          (EVariable mempty (Label () "from_int32"))
                                          ( ELiteral mempty (LInt32 1)
                                              :| []
                                          )
                                      )
                                      :| []
                                  )
                                  <| EClause
                                    mempty
                                    ( PAs
                                        mempty
                                        (Label () "m")
                                        ( PConstructor
                                            mempty
                                            (Label () "Succ")
                                            [ PVariable
                                                mempty
                                                (Label () "f")
                                            ]
                                        )
                                    )
                                    ( CPlain
                                        mempty
                                        []
                                        ( EApplication
                                            mempty
                                            ()
                                            (EVariable mempty (Label () "(*)"))
                                            ( EApplication
                                                mempty
                                                ()
                                                (EVariable mempty (Label () "unpack"))
                                                ( EVariable mempty (Label () "m")
                                                    :| []
                                                )
                                                <| EApplication
                                                  mempty
                                                  ()
                                                  (EVariable mempty (Label () "$fold-70cdac64"))
                                                  ( EVariable mempty (Label () "f")
                                                      :| []
                                                  )
                                                :| []
                                            )
                                        )
                                        :| []
                                    )
                                  :| []
                              )
                          )
                      )
                      ( EApplication
                          mempty
                          ()
                          (EVariable mempty (Label () "$fold-70cdac64"))
                          ( EApplication
                              mempty
                              ()
                              (EVariable mempty (Label () "pack"))
                              ( EVariable mempty (Label () "n")
                                  :| []
                              )
                              :| []
                          )
                      )
                }
            )
        ]
    }

-- testA :: (Monoid a) => IO (Either () (ProtoBuild a))
-- testA = evalProtoCompilerT (protoOprepareBuild testModuleBuiltins)

--  testB :: ProtoModule () Kind ()
--  testB = evalStateT (toKindIndexed (testModuleBuiltinsPreKinds :: ProtoModule () () ())) 0

-- testC :: ([(Name, Kind)], [ProtoKindConstraintsGenOutput])
-- testC = runProtoKindConstraintsGen env (protoOemitKindConstraints testB)
-- where
--  env =
--    Environment.fromList
--      [ ("Numeric", KArrow KType KTrait)
--      ]

-- testD = protoOapplyKinds sub testB
-- where
--  Right sub = res
--  res :: Either ProtoKindError ProtoKindSubstitution
--  res = protoOKindUnifierMonad (protoOsolveKindConstraints constraints)
--  constraints = rights (snd testC)

testE :: IO (Either () (ProtoBuild ())) -- ProtoCompilerT m a ()
testE = do
  evalProtoCompilerT $ do
    indexedModule <- toKindIndexed (testModuleBuiltinsPreKinds :: ProtoModule () () ())
    env <- moduleKindEnvironment indexedModule
    (_, res1) <- runProtoKindConstraintsGen env (protoOemitKindConstraints indexedModule)
    let constraints = rights res1
        Right sub = protoOKindUnifierMonad (protoOsolveKindConstraints constraints) :: Either ProtoKindError ProtoKindSubstitution
        res3 = protoOapplyKinds sub indexedModule :: ProtoModule () Kind ()
    protoOprepareBuild res3

