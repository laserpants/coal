{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.ProtoCompiler.ProtoBuildSpec where

import Coal.AST.Metadata (Metadata (..))
import qualified Coal.Common.Environment as Environment
import Coal.Common.Label (Label (..))
import Coal.Compiler.Build
import Coal.Compiler.TypeInference
import Coal.Graphviz.ProtoDot
import Coal.Language
import Coal.Language.Module.Import (Import (..))
import Coal.Language.Module.Path (Path (..))
import Coal.Language.Type (Parameter (..))
import Coal.Language.Type.Kind.Indexed (ToKindIndexed (..))
import Coal.ProtoCompiler.KindEnvironment (moduleKindEnvironment)
import Coal.ProtoCompiler.ProtoBuild
import Coal.ProtoCompiler.ProtoBuild.ProtoPrep (protoOprepareBuild, protoOreplacePlaceholders)
import Coal.ProtoCompiler.ProtoStack
import Coal.ProtoCompiler.ProtoState
import Coal.ProtoLanguage.ProtoDefinition
import Coal.ProtoLanguage.ProtoModule (ModuleExportList (..), ProtoModule (..))
import Coal.ProtoTypeSystem.Kind.Constraint.Generation
import Coal.ProtoTypeSystem.Kind.Constraint.Solver (protoOsolveKindConstraints)
import Coal.ProtoTypeSystem.Kind.Error (ProtoKindError (..))
import Coal.ProtoTypeSystem.Kind.Substitution
import Coal.ProtoTypeSystem.Kind.Unification
import Coal.TypeSystem.Constraint
import Coal.TypeSystem.Constraint.Assumption
import Coal.TypeSystem.Constraint.Generation.Stack
import Coal.TypeSystem.Substitution
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.State (evalState, evalStateT, get, gets, modify, runState)
import Data.Data (Data)
import Data.Either (lefts, rights)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Debug.Trace
import Extras (Name, forM_)
import Text.Pretty.Simple (pPrint)

testModuleBuiltinsPreKinds :: (Monoid a) => ProtoModule a () ()
testModuleBuiltinsPreKinds =
  ProtoModule
    { protoOmodulePath = Path ["Builtin"]
    , protoOmoduleExportList = ExportAll
    , protoOmoduleDefinitions =
        [ ProtoDType
            mempty
            "IO"
            ( ProtoTypeDefinition
                { protoOtypeDefinitionParameters = [Parameter () "a"]
                , protoOtypeDefinitionConstructors = []
                }
            )
        , ProtoDTrait
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
        [ ProtoDType
            mempty
            "IO"
            ( ProtoTypeDefinition
                { protoOtypeDefinitionParameters = [Parameter KType "a"]
                , protoOtypeDefinitionConstructors = []
                }
            )
        , ProtoDTrait
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

testModule0PreKinds :: (Monoid a) => ProtoModule a () ()
testModule0PreKinds =
  ProtoModule
    { protoOmodulePath = Path ["IO"]
    , protoOmoduleExportList = ExportAll
    , protoOmoduleDefinitions =
        [ ProtoDImport
            mempty
            (Path ["Builtin"])
            [ TypeImport mempty "IO" []
            ]
        , ProtoDFunction
            mempty
            "println_int32"
            ( ProtoFunctionDefinition
                { protoOfunctionDefinitionMetadata = mempty
                , protoOfunctionDefinitionAnnotation =
                    Just
                      ( With
                          []
                          ( TApplication
                              ()
                              (TConstructor () "IO")
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

testModule0 :: (Monoid a) => ProtoModule a Kind ()
testModule0 =
  ProtoModule
    { protoOmodulePath = Path ["IO"]
    , protoOmoduleExportList = ExportAll
    , protoOmoduleDefinitions =
        [ ProtoDImport
            mempty
            (Path ["Builtin"])
            [ TypeImport mempty "IO" []
            ]
        , ProtoDFunction
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

testModule1PreKinds :: (Monoid a) => ProtoModule a () ()
testModule1PreKinds =
  ProtoModule
    { protoOmodulePath = Path ["Main"]
    , protoOmoduleExportList = ExportAll
    , protoOmoduleDefinitions =
        [ ProtoDType
            mempty
            "IO"
            ( ProtoTypeDefinition
                { protoOtypeDefinitionParameters = [Parameter () "a"]
                , protoOtypeDefinitionConstructors = []
                }
            )
        , ProtoDImport
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

-- testModule1 :: (Monoid a) => ProtoModule a Kind ()
-- testModule1 =
--  ProtoModule
--    { protoOmodulePath = Path ["Main"]
--    , protoOmoduleExportList = ExportAll
--    , protoOmoduleDefinitions =
--        [ ProtoDImport
--            mempty
--            (Path ["Math"])
--            [ NameImport mempty "factorial"
--            ]
--        , ProtoDImport
--            mempty
--            (Path ["IO"])
--            [ NameImport mempty "println_int32"
--            ]
--        , ProtoDFunction
--            mempty
--            "main"
--            ( ProtoFunctionDefinition
--                { protoOfunctionDefinitionMetadata = mempty
--                , protoOfunctionDefinitionAnnotation = Nothing
--                , protoOfunctionDefinitionType = With [] ()
--                , protoOfunctionDefinitionPatterns =
--                    PLiteral mempty LUnit :| []
--                , protoOfunctionDefinitionExpression =
--                    EApplication
--                      mempty
--                      ()
--                      (EVariable mempty (Label () "println_int32"))
--                      ( EApplication
--                          mempty
--                          ()
--                          (EVariable mempty (Label () "factorial"))
--                          ( EApplication
--                              mempty
--                              ()
--                              (EVariable mempty (Label () "from_int32"))
--                              ( ELiteral mempty (LInt32 8)
--                                  :| []
--                              )
--                              :| []
--                          )
--                          :| []
--                      )
--                }
--            )
--        ]
--    }

testModule2PreKinds :: (Monoid a) => ProtoModule a () ()
testModule2PreKinds =
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
        , ProtoDImport
            mempty
            (Path ["Nat"])
            [ TypeImport mempty "Nat" ["Succ", "Zero"]
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
                      (PVariable mempty (Label () "$fold"))
                      ( ELambda
                          mempty
                          (PVariable mempty (Label () "$fold.expr") :| [])
                          ( EMatch
                              mempty
                              ()
                              (EVariable mempty (Label () "$fold.expr"))
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
                          )
                      )
                      ( EApplication
                          mempty
                          ()
                          (EVariable mempty (Label () "$fold"))
                          ( EApplication
                              mempty
                              ()
                              (EVariable mempty (Label () "pack"))
                              (EVariable mempty (Label () "n") :| [])
                              :| []
                          )
                      )
                      -- EFold
                      --  mempty
                      --  ()
                      --  ( EApplication
                      --      mempty
                      --      ()
                      --      (EVariable mempty (Label () "pack"))
                      --      (EVariable mempty (Label () "n") :| [])
                      --      :| []
                      --  )
                      --  ( EClause
                      --      mempty
                      --      (PConstructor mempty (Label () "Zero") [])
                      --      ( CPlain
                      --          mempty
                      --          []
                      --          ( EApplication
                      --              mempty
                      --              ()
                      --              (EVariable mempty (Label () "from_int32"))
                      --              ( ELiteral mempty (LInt32 1)
                      --                  :| []
                      --              )
                      --          )
                      --          :| []
                      --      )
                      --      <| EClause
                      --        mempty
                      --        ( PAs
                      --            mempty
                      --            (Label () "m")
                      --            ( PConstructor
                      --                mempty
                      --                (Label () "Succ")
                      --                [ PAtVariable
                      --                    mempty
                      --                    (Label () "f")
                      --                ]
                      --            )
                      --        )
                      --        ( CPlain
                      --            mempty
                      --            []
                      --            ( EApplication
                      --                mempty
                      --                ()
                      --                (EVariable mempty (Label () "(*)"))
                      --                ( EApplication
                      --                    mempty
                      --                    ()
                      --                    (EVariable mempty (Label () "unpack"))
                      --                    ( EVariable mempty (Label () "m")
                      --                        :| []
                      --                    )
                      --                    <| EVariable mempty (Label () "f")
                      --                    :| []
                      --                )
                      --            )
                      --            :| []
                      --        )
                      --      :| []
                      --  )
                }
            )
        ]
    }

-- testModule2 :: (Monoid a) => ProtoModule a Kind ()
-- testModule2 =
--  ProtoModule
--    { protoOmodulePath = Path ["Math"]
--    , protoOmoduleExportList = ExportAll
--    , protoOmoduleDefinitions =
--        [ ProtoDImport
--            mempty
--            (Path ["Nat"])
--            [ NameImport mempty "pack"
--            , NameImport mempty "unpack"
--            ]
--        , ProtoDFunction
--            mempty
--            "factorial"
--            ( ProtoFunctionDefinition
--                { protoOfunctionDefinitionMetadata =
--                    mempty
--                , protoOfunctionDefinitionAnnotation =
--                    Just (With [] (TIntrinsic IInt32))
--                , protoOfunctionDefinitionType =
--                    With [] ()
--                , protoOfunctionDefinitionPatterns =
--                    PAnnotation
--                      mempty
--                      (TIntrinsic IInt32)
--                      (PVariable mempty (Label () "n"))
--                      :| []
--                , protoOfunctionDefinitionExpression =
--                    EFold
--                      mempty
--                      ()
--                      ( EApplication
--                          mempty
--                          ()
--                          (EVariable mempty (Label () "pack"))
--                          (EVariable mempty (Label () "n") :| [])
--                          :| []
--                      )
--                      ( EClause
--                          mempty
--                          (PConstructor mempty (Label () "Zero") [])
--                          ( CPlain
--                              mempty
--                              []
--                              ( EApplication
--                                  mempty
--                                  ()
--                                  (EVariable mempty (Label () "from_int32"))
--                                  ( ELiteral mempty (LInt32 1)
--                                      :| []
--                                  )
--                              )
--                              :| []
--                          )
--                          <| EClause
--                            mempty
--                            ( PAs
--                                mempty
--                                (Label () "m")
--                                ( PConstructor
--                                    mempty
--                                    (Label () "Succ")
--                                    [ PAtVariable
--                                        mempty
--                                        (Label () "f")
--                                    ]
--                                )
--                            )
--                            ( CPlain
--                                mempty
--                                []
--                                ( EApplication
--                                    mempty
--                                    ()
--                                    (EVariable mempty (Label () "(*)"))
--                                    ( EApplication
--                                        mempty
--                                        ()
--                                        (EVariable mempty (Label () "unpack"))
--                                        ( EVariable mempty (Label () "m")
--                                            :| []
--                                        )
--                                        <| EVariable mempty (Label () "f")
--                                        :| []
--                                    )
--                                )
--                                :| []
--                            )
--                          :| []
--                      )
--                }
--            )
--        ]
--    }

testModule3PreKinds :: (Monoid a) => ProtoModule a () ()
testModule3PreKinds =
  ProtoModule
    { protoOmodulePath = Path ["Nat"]
    , protoOmoduleExportList = ExportAll
    , protoOmoduleDefinitions =
        [ ProtoDType
            mempty
            "Nat"
            ( ProtoTypeDefinition
                { protoOtypeDefinitionParameters = []
                , protoOtypeDefinitionConstructors =
                    [ DataConstructor
                        { constructorName = "Succ"
                        , constructorArity = 1
                        , constructorScheme = Forall mempty [] (TIntrinsic INat `TArrow` TIntrinsic INat)
                        }
                    , DataConstructor
                        { constructorName = "Zero"
                        , constructorArity = 0
                        , constructorScheme = Forall mempty [] (TIntrinsic INat)
                        }
                    ]
                }
            )
        , ProtoDFunction
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

testModule4PreKinds :: (Monoid a) => ProtoModule a () ()
testModule4PreKinds =
  ProtoModule
    { protoOmodulePath = Path ["Main"]
    , protoOmoduleExportList = ExportAll
    , protoOmoduleDefinitions =
        [ ProtoDFunction
            mempty
            "some_fun"
            ( ProtoFunctionDefinition
                { protoOfunctionDefinitionMetadata = mempty
                , protoOfunctionDefinitionAnnotation = Nothing
                , protoOfunctionDefinitionType = With [] ()
                , protoOfunctionDefinitionPatterns =
                    PVariable mempty (Label () "m") :| []
                , protoOfunctionDefinitionExpression =
                    ELiteral mempty (LInt32 1)
                }
            )
        ]
    }

-- testModule3 :: (Monoid a) => ProtoModule a Kind ()
-- testModule3 =
--  ProtoModule
--    { protoOmodulePath = Path ["Nat"]
--    , protoOmoduleExportList = ExportAll
--    , protoOmoduleDefinitions =
--        [ ProtoDFunction
--            mempty
--            "pack"
--            ( ProtoFunctionDefinition
--                { protoOfunctionDefinitionMetadata = mempty
--                , protoOfunctionDefinitionAnnotation =
--                    Just
--                      ( With
--                          []
--                          (TIntrinsic INat)
--                      )
--                , protoOfunctionDefinitionType =
--                    With [] ()
--                , protoOfunctionDefinitionPatterns =
--                    PAnnotation
--                      mempty
--                      (TIntrinsic IInt32)
--                      (PVariable mempty (Label () "m"))
--                      :| []
--                , protoOfunctionDefinitionExpression =
--                    EApplication
--                      mempty
--                      ()
--                      (EVariable mempty (Label () "nat$_pack"))
--                      ( EVariable mempty (Label () "m")
--                          :| []
--                      )
--                }
--            )
--        , ProtoDFunction
--            mempty
--            "unpack"
--            ( ProtoFunctionDefinition
--                { protoOfunctionDefinitionMetadata = mempty
--                , protoOfunctionDefinitionAnnotation =
--                    Just
--                      ( With
--                          []
--                          (TIntrinsic IInt32)
--                      )
--                , protoOfunctionDefinitionType =
--                    With [] ()
--                , protoOfunctionDefinitionPatterns =
--                    PAnnotation
--                      mempty
--                      (TIntrinsic INat)
--                      (PVariable mempty (Label () "n"))
--                      :| []
--                , protoOfunctionDefinitionExpression =
--                    EApplication
--                      mempty
--                      ()
--                      (EVariable mempty (Label () "nat$_unpack"))
--                      ( EVariable mempty (Label () "n")
--                          :| []
--                      )
--                }
--            )
--        ]
--    }

--

-- testModule2B :: (Monoid a) => ProtoModule a Kind ()
-- testModule2B =
--  ProtoModule
--    { protoOmodulePath = Path ["Math"]
--    , protoOmoduleExportList = ExportAll
--    , protoOmoduleDefinitions =
--        [ ProtoDImport
--            mempty
--            (Path ["Nat"])
--            [ NameImport mempty "pack"
--            , NameImport mempty "unpack"
--            ]
--        , ProtoDFunction
--            mempty
--            "factorial"
--            ( ProtoFunctionDefinition
--                { protoOfunctionDefinitionMetadata =
--                    mempty
--                , protoOfunctionDefinitionAnnotation =
--                    Just (With [] (TIntrinsic IInt32))
--                , protoOfunctionDefinitionType =
--                    With [] ()
--                , protoOfunctionDefinitionPatterns =
--                    PAnnotation
--                      mempty
--                      (TIntrinsic IInt32)
--                      (PVariable mempty (Label () "n"))
--                      :| []
--                , protoOfunctionDefinitionExpression =
--                    ERecursiveLet
--                      mempty
--                      (PVariable mempty (Label () "$fold-70cdac64"))
--                      ( ELambda
--                          mempty
--                          (PVariable mempty (Label () "$variable-185c7b8df7b0") :| [])
--                          ( EMatch
--                              mempty
--                              ()
--                              (EVariable mempty (Label () "$variable-185c7b8df7b0"))
--                              ( EClause
--                                  mempty
--                                  (PConstructor mempty (Label () "Zero") [])
--                                  ( CPlain
--                                      mempty
--                                      []
--                                      ( EApplication
--                                          mempty
--                                          ()
--                                          (EVariable mempty (Label () "from_int32"))
--                                          ( ELiteral mempty (LInt32 1)
--                                              :| []
--                                          )
--                                      )
--                                      :| []
--                                  )
--                                  <| EClause
--                                    mempty
--                                    ( PAs
--                                        mempty
--                                        (Label () "m")
--                                        ( PConstructor
--                                            mempty
--                                            (Label () "Succ")
--                                            [ PVariable
--                                                mempty
--                                                (Label () "f")
--                                            ]
--                                        )
--                                    )
--                                    ( CPlain
--                                        mempty
--                                        []
--                                        ( EApplication
--                                            mempty
--                                            ()
--                                            (EVariable mempty (Label () "(*)"))
--                                            ( EApplication
--                                                mempty
--                                                ()
--                                                (EVariable mempty (Label () "unpack"))
--                                                ( EVariable mempty (Label () "m")
--                                                    :| []
--                                                )
--                                                <| EApplication
--                                                  mempty
--                                                  ()
--                                                  (EVariable mempty (Label () "$fold-70cdac64"))
--                                                  ( EVariable mempty (Label () "f")
--                                                      :| []
--                                                  )
--                                                :| []
--                                            )
--                                        )
--                                        :| []
--                                    )
--                                  :| []
--                              )
--                          )
--                      )
--                      ( EApplication
--                          mempty
--                          ()
--                          (EVariable mempty (Label () "$fold-70cdac64"))
--                          ( EApplication
--                              mempty
--                              ()
--                              (EVariable mempty (Label () "pack"))
--                              ( EVariable mempty (Label () "n")
--                                  :| []
--                              )
--                              :| []
--                          )
--                      )
--                }
--            )
--        ]
--    }

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
    kindIndexedModule <- toKindIndexed (testModuleBuiltinsPreKinds :: ProtoModule () () ())
    env <- moduleKindEnvironment kindIndexedModule
    (_, res1) <- runProtoKindConstraintsGen env (protoOemitKindConstraints kindIndexedModule)
    let constraints = rights res1
        errs = lefts res1
        Right sub = protoOKindUnifierMonad (protoOsolveKindConstraints constraints) :: Either ProtoKindError ProtoKindSubstitution
        res3 = protoOapplyKinds sub kindIndexedModule :: ProtoModule () Kind ()
    traceShowM errs
    traceShowM res3
    traceShowM (res3 == testModuleBuiltins)
    protoOprepareBuild res3
    protoOgetCurrentBuildC

testF :: IO (Either () (ProtoBuild ())) -- ProtoCompilerT m a ()
testF = do
  evalProtoCompilerT $ do
    kindIndexedModule <- toKindIndexed (testModule0PreKinds :: ProtoModule () () ())
    env <- moduleKindEnvironment kindIndexedModule
    (_, res1) <- runProtoKindConstraintsGen env (protoOemitKindConstraints kindIndexedModule)
    let constraints = rights res1
        errs = lefts res1
        Right sub = protoOKindUnifierMonad (protoOsolveKindConstraints constraints) :: Either ProtoKindError ProtoKindSubstitution
        res3 = protoOapplyKinds sub kindIndexedModule :: ProtoModule () Kind ()
    traceShowM errs
    traceShowM (res3 == testModule0)
    protoOprepareBuild res3
    protoOgetCurrentBuildC

testG :: IO (Either () (ProtoBuild ())) -- ProtoCompilerT m a ()
testG = do
  evalProtoCompilerT $ do
    kindIndexedModule <- toKindIndexed (testModuleBuiltinsPreKinds :: ProtoModule () () ())
    env <- moduleKindEnvironment kindIndexedModule
    (_, res1) <- runProtoKindConstraintsGen env (protoOemitKindConstraints kindIndexedModule)
    let constraints = rights res1
        errs = lefts res1
        Right sub = protoOKindUnifierMonad (protoOsolveKindConstraints constraints) :: Either ProtoKindError ProtoKindSubstitution
        res3 = protoOapplyKinds sub kindIndexedModule :: ProtoModule () Kind ()
    protoOprepareBuild res3
    --
    kindIndexedModule <- toKindIndexed (testModule0PreKinds :: ProtoModule () () ())
    env <- moduleKindEnvironment kindIndexedModule
    (_, res1) <- runProtoKindConstraintsGen env (protoOemitKindConstraints kindIndexedModule)
    let constraints = rights res1
        errs = lefts res1
        Right sub = protoOKindUnifierMonad (protoOsolveKindConstraints constraints) :: Either ProtoKindError ProtoKindSubstitution
        res3 = protoOapplyKinds sub kindIndexedModule :: ProtoModule () Kind ()
    traceShowM errs
    traceShowM res3
    traceShowM (res3 == testModule0)
    protoOprepareBuild res3
    protoOgetCurrentBuildC

-- updateNames :: (Monad m, Data a) => [ProtoDefinition a Kind IndexedType] -> ProtoCompilerT m a ()
-- updateNames defs =
--  forM_ defs $

defineName :: (Monad m, Data a) => ProtoDefinition a Kind IndexedType -> ProtoCompilerT m a ()
defineName =
  \case
    def@(ProtoDFunction _ name ProtoFunctionDefinition{..}) ->
      protoOdefine name (typeOf def)
    def@(ProtoDLet _ name ProtoLetDefinition{..}) ->
      protoOdefine name (typeOf def)
    ProtoDInstance _ ProtoInstanceDefinition{..} -> do
      let trait = Trait protoOinstanceDefinitionTraitName protoOinstanceDefinitionType
      forM_ protoOinstanceDefinitionImplementations $
        \case
          def@(ProtoDFunction _ name _) ->
            protoOdefine (instanceLabel trait name) (typeOf def)
          def@(ProtoDLet _ name _) ->
            protoOdefine (instanceLabel trait name) (typeOf def)
    _ ->
      pure ()

xyz :: [ProtoModule Metadata () ()] -> IO ()
xyz modules = do
  (_, r, _) <- runProtoCompilerT $ do
    forM_ modules $
      \modul -> do
        protoOclearAssumptionsC
        clearNameStoreC
        setCurrentModuleC modul

        res3 <- inferKinds modul
        protoOprepareBuild res3

        ProtoModule _ _ defs1 <- inferTypes res3
        protoOreplacePlaceholders

        c <- protoOgetCurrentBuildC
        pPrint c

        --        p <- gets protoOcompilerCurrentPath
        --        xxx <- gets protoOcompilerModules
        --        pPrint p
        --        pPrint xxx
        --        traceShowM "-----------"
        --        traceShowM "-----------"
        --        traceShowM "-----------"

        --        pPrint defs1

        let mm = modul{protoOmoduleDefinitions = defs1}
        let qq = generateDotSyntax mm

        --        pPrint errors
        sub1 <- gets protoOcompilerSubstitution
        --        traceShowM sub1
        pPrint qq

        --        let ProtoModule _ _ defs = c :: ProtoModule Metadata Kind IndexedType
        --        (tdefs, _) <- typeDefinitionsC defs

        -- traceShowM "********"
        -- traceShowM c
        -- traceShowM asms

        pure ()

  --  pPrint r

  pure ()

indexTypes :: (Monad m, Traversable t) => t e -> ProtoCompilerT m a (t IndexedType)
indexTypes ds = run (indexed ds) =<< gets protoOcompilerSupply
 where
  run s m = do
    let (r, n) = runState s m
    protoOupdateSupplyC n
    pure r

inferKinds :: (Monad m) => ProtoModule Metadata () () -> ProtoCompilerT m Metadata (ProtoModule Metadata Kind ())
inferKinds modul = do
  indexed <- toKindIndexed modul
  generateKindConstraints indexed
  constraints <- gets protoOcompilerKindConstraints
  case protoOKindUnifierMonad (protoOsolveKindConstraints constraints) of
    Left err ->
      error "TODO"
    Right sub ->
      return (protoOapplyKinds sub indexed)

inferTypes :: (MonadIO m, Data a, Show a, Eq a) => ProtoModule a Kind () -> ProtoCompilerT m a (ProtoModule a Kind IndexedType)
inferTypes modul = do
  ProtoModule{..} <- indexTypes modul
  forM_ protoOmoduleDefinitions $
    \def -> do
      protoOgenerateConstraints def
      sub <- solveX
      defineName (apply sub def)
  sub <- gets protoOcompilerSubstitution
  modify (overProtoCompilerAssumptions (apply sub))
  pure $
    ProtoModule
      { protoOmoduleDefinitions = fmap (fmap normalizeRowTypes) (apply sub protoOmoduleDefinitions)
      , ..
      }

-- typeDefinition1 :: (Monad m, Data a, Show a) => ProtoDefinition a Kind IndexedType -> ProtoCompilerT m a ()
-- typeDefinition1 =
--  \case
--    ProtoDFunction _ name ProtoFunctionDefinition { .. } -> do
--      protoOgenerateConstraints protoOfunctionDefinitionExpression
--
--    -- define name (typeOf (apply sub def))
--    ProtoDLet _ name ProtoLetDefinition{..} ->
--      -- generateConstraints def
--      -- solve
--      pure ()
--    -- define name (typeOf (apply sub def))
--
--    ProtoDInstance a ProtoInstanceDefinition{..} ->
--      forM_ protoOinstanceDefinitionImplementations $
--        \case
--          ProtoDFunction _ name ProtoFunctionDefinition{..} ->
--            pure ()
--          ProtoDLet _ name ProtoLetDefinition{..} ->
--            pure ()
--    _ ->
--      pure ()

testModule9 :: (Monoid a) => ProtoModule a () ()
testModule9 =
  ProtoModule
    { protoOmodulePath = Path ["IO"]
    , protoOmoduleExportList = ExportAll
    , protoOmoduleDefinitions =
        [ ProtoDType
            mempty
            "IO"
            ( ProtoTypeDefinition
                { protoOtypeDefinitionParameters = [Parameter () "a"]
                , protoOtypeDefinitionConstructors = []
                }
            )
        , ProtoDFunction
            mempty
            "println_int32"
            ( ProtoFunctionDefinition
                { protoOfunctionDefinitionMetadata = mempty
                , protoOfunctionDefinitionAnnotation =
                    Just
                      ( With
                          []
                          ( TApplication
                              ()
                              (TConstructor () "IO")
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

foo =
  xyz
    [ testModuleBuiltinsPreKinds
    , testModule0PreKinds
    , testModule3PreKinds
    , testModule2PreKinds
    --    , testModule1PreKinds
    ]

foo2 =
  xyz
    [ testModule4PreKinds
    ]

foo3 =
  xyz
    [ testModule9
    ]
