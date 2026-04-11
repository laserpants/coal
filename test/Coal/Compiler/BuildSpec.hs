{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Coal.Compiler.BuildSpec where

import Coal.AST.Metadata (Metadata (..))
import qualified Coal.Common.Environment as Environment
import Coal.Common.Label (Label (..))
import Coal.Compiler.TypeInference
import Coal.Graphviz.Dot
import Coal.Language
import Coal.Language.Module.Import (Import (..))
import Coal.Language.Module.Path (Path (..))
import Coal.Language.Type (Parameter (..))
import Coal.Language.Type.Kind.Indexed (ToKindIndexed (..))
import Coal.Compiler.KindEnvironment (moduleKindEnvironment)
import Coal.Compiler.Build
import Coal.Compiler.Build.Prep (protoOprepareBuild, protoOreplacePlaceholders)
import Coal.Compiler.Stack
import Coal.Compiler.State
import Coal.Language.Definition
import Coal.Language.Module (ModuleExportList (..), Module (..))
import Coal.TypeSystem.Kind.Constraint.Generation
import Coal.TypeSystem.Kind.Constraint.Solver (protoOsolveKindConstraints)
import Coal.TypeSystem.Kind.Error (KindError (..))
import Coal.TypeSystem.Kind.Substitution
import Coal.TypeSystem.Kind.Unification
import Coal.TypeSystem.Constraint
import Coal.TypeSystem.Constraint.Assumption
import Coal.TypeSystem.Constraint.Generation.Stack
import Coal.TypeSystem.Substitution
import Control.Monad (when)
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

testModuleBuiltinsPreKinds :: (Monoid a) => Module a () ()
testModuleBuiltinsPreKinds =
  Module
    { protoOmodulePath = Path ["Builtin"]
    , protoOmoduleExportList = ExportAll
    , protoOmoduleDefinitions =
        [ DType
            mempty
            "IO"
            ( TypeDefinition
                { protoOtypeDefinitionParameters = [Parameter () "a"]
                , protoOtypeDefinitionConstructors = mempty
                }
            )
        , DTrait
            mempty
            "Numeric"
            ( TraitDefinition
                { protoOtraitDefinitionMetadata =
                    mempty
                , protoOtraitDefinitionTraitName =
                    "Numeric"
                , protoOtraitDefinitionConstraints =
                    mempty
                , protoOtraitDefinitionParameter =
                    Parameter () "a"
                , protoOtraitDefinitionInterface =
                    [ TraitDefinitionInterfaceEntry
                        "from_int32"
                        ( Forall
                            (Set.fromList [Parameter () "a"])
                            (Set.fromList [Trait "Numeric" (TVariable (Parameter () "a"))])
                            (TIntrinsic IInt32 `TArrow` TVariable (Parameter () "a"))
                        )
                    , TraitDefinitionInterfaceEntry
                        "from_int64"
                        ( Forall
                            (Set.fromList [Parameter () "a"])
                            (Set.fromList [Trait "Numeric" (TVariable (Parameter () "a"))])
                            (TIntrinsic IInt64 `TArrow` TVariable (Parameter () "a"))
                        )
                    , TraitDefinitionInterfaceEntry
                        "from_bignum"
                        ( Forall
                            (Set.fromList [Parameter () "a"])
                            (Set.fromList [Trait "Numeric" (TVariable (Parameter () "a"))])
                            (TIntrinsic IBignum `TArrow` TVariable (Parameter () "a"))
                        )
                    , TraitDefinitionInterfaceEntry
                        "negate"
                        ( Forall
                            (Set.fromList [Parameter () "a"])
                            (Set.fromList [Trait "Numeric" (TVariable (Parameter () "a"))])
                            (TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a"))
                        )
                    , TraitDefinitionInterfaceEntry
                        "(+)"
                        ( Forall
                            (Set.fromList [Parameter () "a"])
                            (Set.fromList [Trait "Numeric" (TVariable (Parameter () "a"))])
                            (TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a"))
                        )
                    , TraitDefinitionInterfaceEntry
                        "(-)"
                        ( Forall
                            (Set.fromList [Parameter () "a"])
                            (Set.fromList [Trait "Numeric" (TVariable (Parameter () "a"))])
                            (TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a"))
                        )
                    , TraitDefinitionInterfaceEntry
                        "(*)"
                        ( Forall
                            (Set.fromList [Parameter () "a"])
                            (Set.fromList [Trait "Numeric" (TVariable (Parameter () "a"))])
                            (TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a") `TArrow` TVariable (Parameter () "a"))
                        )
                    ]
                }
            )
        , DInstance
            mempty
            ( InstanceDefinition
                { protoOinstanceDefinitionMetadata =
                    mempty
                , protoOinstanceDefinitionConstraints =
                    mempty
                , protoOinstanceDefinitionTraitName =
                    "Numeric"
                , protoOinstanceDefinitionType =
                    TIntrinsic IInt32
                , protoOinstanceDefinitionImplementations =
                    [ DFunction
                        mempty
                        "from_int32"
                        ( FunctionDefinition
                            { protoOfunctionDefinitionMetadata =
                                mempty
                            , protoOfunctionDefinitionAnnotation =
                                Nothing
                            , protoOfunctionDefinitionType =
                                With mempty ()
                            , protoOfunctionDefinitionPatterns =
                                PVariable mempty (Label () "n") :| mempty
                            , protoOfunctionDefinitionExpression =
                                EVariable mempty (Label () "n")
                            }
                        )
                    , DFunction
                        mempty
                        "from_int64"
                        ( FunctionDefinition
                            { protoOfunctionDefinitionMetadata =
                                mempty
                            , protoOfunctionDefinitionAnnotation =
                                Nothing
                            , protoOfunctionDefinitionType =
                                With mempty ()
                            , protoOfunctionDefinitionPatterns =
                                PVariable mempty (Label () "n") :| mempty
                            , protoOfunctionDefinitionExpression =
                                EApplication
                                  mempty
                                  ()
                                  (EVariable mempty (Label () "bultin$_int64_to_int32"))
                                  ( EVariable mempty (Label () "n")
                                      :| mempty
                                  )
                            }
                        )
                    , DFunction
                        mempty
                        "from_bignum"
                        ( FunctionDefinition
                            { protoOfunctionDefinitionMetadata =
                                mempty
                            , protoOfunctionDefinitionAnnotation =
                                Nothing
                            , protoOfunctionDefinitionType =
                                With mempty ()
                            , protoOfunctionDefinitionPatterns =
                                PVariable mempty (Label () "n") :| mempty
                            , protoOfunctionDefinitionExpression =
                                EApplication
                                  mempty
                                  ()
                                  (EVariable mempty (Label () "bultin$_bignum_to_int32"))
                                  ( EVariable mempty (Label () "n")
                                      :| mempty
                                  )
                            }
                        )
                    , DFunction
                        mempty
                        "negate"
                        ( FunctionDefinition
                            { protoOfunctionDefinitionMetadata =
                                mempty
                            , protoOfunctionDefinitionAnnotation =
                                Nothing
                            , protoOfunctionDefinitionType =
                                With mempty ()
                            , protoOfunctionDefinitionPatterns =
                                PVariable mempty (Label () "n") :| mempty
                            , protoOfunctionDefinitionExpression =
                                EApplication
                                  mempty
                                  ()
                                  (EVariable mempty (Label () "bultin$_int32_neg"))
                                  ( EVariable mempty (Label () "n")
                                      :| mempty
                                  )
                            }
                        )
                    , DFunction
                        mempty
                        "(+)"
                        ( FunctionDefinition
                            { protoOfunctionDefinitionMetadata =
                                mempty
                            , protoOfunctionDefinitionAnnotation =
                                Nothing
                            , protoOfunctionDefinitionType =
                                With mempty ()
                            , protoOfunctionDefinitionPatterns =
                                PVariable mempty (Label () "m") <| PVariable mempty (Label () "n") :| mempty
                            , protoOfunctionDefinitionExpression =
                                EApplication
                                  mempty
                                  ()
                                  (EVariable mempty (Label () "bultin$_int32_add"))
                                  ( EVariable mempty (Label () "m")
                                      <| EVariable mempty (Label () "n")
                                      :| mempty
                                  )
                            }
                        )
                    , DFunction
                        mempty
                        "(-)"
                        ( FunctionDefinition
                            { protoOfunctionDefinitionMetadata =
                                mempty
                            , protoOfunctionDefinitionAnnotation =
                                Nothing
                            , protoOfunctionDefinitionType =
                                With mempty ()
                            , protoOfunctionDefinitionPatterns =
                                PVariable mempty (Label () "m") <| PVariable mempty (Label () "n") :| mempty
                            , protoOfunctionDefinitionExpression =
                                EApplication
                                  mempty
                                  ()
                                  (EVariable mempty (Label () "bultin$_int32_sub"))
                                  ( EVariable mempty (Label () "m")
                                      <| EVariable mempty (Label () "n")
                                      :| mempty
                                  )
                            }
                        )
                    , DFunction
                        mempty
                        "(*)"
                        ( FunctionDefinition
                            { protoOfunctionDefinitionMetadata =
                                mempty
                            , protoOfunctionDefinitionAnnotation =
                                Nothing
                            , protoOfunctionDefinitionType =
                                With mempty ()
                            , protoOfunctionDefinitionPatterns =
                                PVariable mempty (Label () "m") <| PVariable mempty (Label () "n") :| mempty
                            , protoOfunctionDefinitionExpression =
                                EApplication
                                  mempty
                                  ()
                                  (EVariable mempty (Label () "bultin$_int32_mul"))
                                  ( EVariable mempty (Label () "m")
                                      <| EVariable mempty (Label () "n")
                                      :| mempty
                                  )
                            }
                        )
                    ]
                }
            )
        ]
    }

testModuleBuiltins :: (Monoid a) => Module a Kind ()
testModuleBuiltins =
  Module
    { protoOmodulePath = Path ["Builtin"]
    , protoOmoduleExportList = ExportAll
    , protoOmoduleDefinitions =
        [ DType
            mempty
            "IO"
            ( TypeDefinition
                { protoOtypeDefinitionParameters = [Parameter KType "a"]
                , protoOtypeDefinitionConstructors = mempty
                }
            )
        , DTrait
            mempty
            "Numeric"
            ( TraitDefinition
                { protoOtraitDefinitionMetadata =
                    mempty
                , protoOtraitDefinitionTraitName =
                    "Numeric"
                , protoOtraitDefinitionConstraints =
                    mempty
                , protoOtraitDefinitionParameter =
                    Parameter KType "a"
                , protoOtraitDefinitionInterface =
                    [ TraitDefinitionInterfaceEntry
                        "from_int32"
                        ( Forall
                            (Set.fromList [Parameter KType "a"])
                            (Set.fromList [Trait "Numeric" (TVariable (Parameter KType "a"))])
                            (TIntrinsic IInt32 `TArrow` TVariable (Parameter KType "a"))
                        )
                    , TraitDefinitionInterfaceEntry
                        "from_int64"
                        ( Forall
                            (Set.fromList [Parameter KType "a"])
                            (Set.fromList [Trait "Numeric" (TVariable (Parameter KType "a"))])
                            (TIntrinsic IInt64 `TArrow` TVariable (Parameter KType "a"))
                        )
                    , TraitDefinitionInterfaceEntry
                        "from_bignum"
                        ( Forall
                            (Set.fromList [Parameter KType "a"])
                            (Set.fromList [Trait "Numeric" (TVariable (Parameter KType "a"))])
                            (TIntrinsic IBignum `TArrow` TVariable (Parameter KType "a"))
                        )
                    , TraitDefinitionInterfaceEntry
                        "negate"
                        ( Forall
                            (Set.fromList [Parameter KType "a"])
                            (Set.fromList [Trait "Numeric" (TVariable (Parameter KType "a"))])
                            (TVariable (Parameter KType "a") `TArrow` TVariable (Parameter KType "a"))
                        )
                    , TraitDefinitionInterfaceEntry
                        "(+)"
                        ( Forall
                            (Set.fromList [Parameter KType "a"])
                            (Set.fromList [Trait "Numeric" (TVariable (Parameter KType "a"))])
                            (TVariable (Parameter KType "a") `TArrow` TVariable (Parameter KType "a") `TArrow` TVariable (Parameter KType "a"))
                        )
                    , TraitDefinitionInterfaceEntry
                        "(-)"
                        ( Forall
                            (Set.fromList [Parameter KType "a"])
                            (Set.fromList [Trait "Numeric" (TVariable (Parameter KType "a"))])
                            (TVariable (Parameter KType "a") `TArrow` TVariable (Parameter KType "a") `TArrow` TVariable (Parameter KType "a"))
                        )
                    , TraitDefinitionInterfaceEntry
                        "(*)"
                        ( Forall
                            (Set.fromList [Parameter KType "a"])
                            (Set.fromList [Trait "Numeric" (TVariable (Parameter KType "a"))])
                            (TVariable (Parameter KType "a") `TArrow` TVariable (Parameter KType "a") `TArrow` TVariable (Parameter KType "a"))
                        )
                    ]
                }
            )
        , DInstance
            mempty
            ( InstanceDefinition
                { protoOinstanceDefinitionMetadata =
                    mempty
                , protoOinstanceDefinitionConstraints =
                    mempty
                , protoOinstanceDefinitionTraitName =
                    "Numeric"
                , protoOinstanceDefinitionType =
                    TIntrinsic IInt32
                , protoOinstanceDefinitionImplementations =
                    [ DFunction
                        mempty
                        "from_int32"
                        ( FunctionDefinition
                            { protoOfunctionDefinitionMetadata =
                                mempty
                            , protoOfunctionDefinitionAnnotation =
                                Nothing
                            , protoOfunctionDefinitionType =
                                With mempty ()
                            , protoOfunctionDefinitionPatterns =
                                PVariable mempty (Label () "n") :| mempty
                            , protoOfunctionDefinitionExpression =
                                EVariable mempty (Label () "n")
                            }
                        )
                    , DFunction
                        mempty
                        "from_int64"
                        ( FunctionDefinition
                            { protoOfunctionDefinitionMetadata =
                                mempty
                            , protoOfunctionDefinitionAnnotation =
                                Nothing
                            , protoOfunctionDefinitionType =
                                With mempty ()
                            , protoOfunctionDefinitionPatterns =
                                PVariable mempty (Label () "n") :| mempty
                            , protoOfunctionDefinitionExpression =
                                EApplication
                                  mempty
                                  ()
                                  (EVariable mempty (Label () "bultin$_int64_to_int32"))
                                  ( EVariable mempty (Label () "n")
                                      :| mempty
                                  )
                            }
                        )
                    , DFunction
                        mempty
                        "from_bignum"
                        ( FunctionDefinition
                            { protoOfunctionDefinitionMetadata =
                                mempty
                            , protoOfunctionDefinitionAnnotation =
                                Nothing
                            , protoOfunctionDefinitionType =
                                With mempty ()
                            , protoOfunctionDefinitionPatterns =
                                PVariable mempty (Label () "n") :| mempty
                            , protoOfunctionDefinitionExpression =
                                EApplication
                                  mempty
                                  ()
                                  (EVariable mempty (Label () "bultin$_bignum_to_int32"))
                                  ( EVariable mempty (Label () "n")
                                      :| mempty
                                  )
                            }
                        )
                    , DFunction
                        mempty
                        "negate"
                        ( FunctionDefinition
                            { protoOfunctionDefinitionMetadata =
                                mempty
                            , protoOfunctionDefinitionAnnotation =
                                Nothing
                            , protoOfunctionDefinitionType =
                                With mempty ()
                            , protoOfunctionDefinitionPatterns =
                                PVariable mempty (Label () "n") :| mempty
                            , protoOfunctionDefinitionExpression =
                                EApplication
                                  mempty
                                  ()
                                  (EVariable mempty (Label () "bultin$_int32_neg"))
                                  ( EVariable mempty (Label () "n")
                                      :| mempty
                                  )
                            }
                        )
                    , DFunction
                        mempty
                        "(+)"
                        ( FunctionDefinition
                            { protoOfunctionDefinitionMetadata =
                                mempty
                            , protoOfunctionDefinitionAnnotation =
                                Nothing
                            , protoOfunctionDefinitionType =
                                With mempty ()
                            , protoOfunctionDefinitionPatterns =
                                PVariable mempty (Label () "m") <| PVariable mempty (Label () "n") :| mempty
                            , protoOfunctionDefinitionExpression =
                                EApplication
                                  mempty
                                  ()
                                  (EVariable mempty (Label () "bultin$_int32_add"))
                                  ( EVariable mempty (Label () "m")
                                      <| EVariable mempty (Label () "n")
                                      :| mempty
                                  )
                            }
                        )
                    , DFunction
                        mempty
                        "(-)"
                        ( FunctionDefinition
                            { protoOfunctionDefinitionMetadata =
                                mempty
                            , protoOfunctionDefinitionAnnotation =
                                Nothing
                            , protoOfunctionDefinitionType =
                                With mempty ()
                            , protoOfunctionDefinitionPatterns =
                                PVariable mempty (Label () "m") <| PVariable mempty (Label () "n") :| mempty
                            , protoOfunctionDefinitionExpression =
                                EApplication
                                  mempty
                                  ()
                                  (EVariable mempty (Label () "bultin$_int32_sub"))
                                  ( EVariable mempty (Label () "m")
                                      <| EVariable mempty (Label () "n")
                                      :| mempty
                                  )
                            }
                        )
                    , DFunction
                        mempty
                        "(*)"
                        ( FunctionDefinition
                            { protoOfunctionDefinitionMetadata =
                                mempty
                            , protoOfunctionDefinitionAnnotation =
                                Nothing
                            , protoOfunctionDefinitionType =
                                With mempty ()
                            , protoOfunctionDefinitionPatterns =
                                PVariable mempty (Label () "m") <| PVariable mempty (Label () "n") :| mempty
                            , protoOfunctionDefinitionExpression =
                                EApplication
                                  mempty
                                  ()
                                  (EVariable mempty (Label () "bultin$_int32_mul"))
                                  ( EVariable mempty (Label () "m")
                                      <| EVariable mempty (Label () "n")
                                      :| mempty
                                  )
                            }
                        )
                    ]
                }
            )
        ]
    }

--

testModule0PreKinds :: (Monoid a) => Module a () ()
testModule0PreKinds =
  Module
    { protoOmodulePath = Path ["IO"]
    , protoOmoduleExportList = ExportAll
    , protoOmoduleDefinitions =
        [ DImport
            mempty
            (Path ["Builtin"])
            [ TypeImport mempty "IO" mempty
            ]
        , DFunction
            mempty
            "println_int32"
            ( FunctionDefinition
                { protoOfunctionDefinitionMetadata = mempty
                , protoOfunctionDefinitionAnnotation =
                    Just
                      ( With
                          mempty
                          ( TApplication
                              ()
                              (TConstructor () "IO")
                              (TIntrinsic IUnit)
                          )
                      )
                , protoOfunctionDefinitionType = With mempty ()
                , protoOfunctionDefinitionPatterns =
                    PAnnotation
                      mempty
                      (TIntrinsic IInt32)
                      (PVariable mempty (Label () "n"))
                      :| mempty
                , protoOfunctionDefinitionExpression =
                    EApplication
                      mempty
                      ()
                      (EVariable mempty (Label () "io$_println_int32"))
                      ( EVariable mempty (Label () "n")
                          :| mempty
                      )
                }
            )
        ]
    }

testModule0 :: (Monoid a) => Module a Kind ()
testModule0 =
  Module
    { protoOmodulePath = Path ["IO"]
    , protoOmoduleExportList = ExportAll
    , protoOmoduleDefinitions =
        [ DImport
            mempty
            (Path ["Builtin"])
            [ TypeImport mempty "IO" mempty
            ]
        , DFunction
            mempty
            "println_int32"
            ( FunctionDefinition
                { protoOfunctionDefinitionMetadata = mempty
                , protoOfunctionDefinitionAnnotation =
                    Just
                      ( With
                          mempty
                          ( TApplication
                              KType
                              (TConstructor (KArrow KType KType) "IO")
                              (TIntrinsic IUnit)
                          )
                      )
                , protoOfunctionDefinitionType = With mempty ()
                , protoOfunctionDefinitionPatterns =
                    PAnnotation
                      mempty
                      (TIntrinsic IInt32)
                      (PVariable mempty (Label () "n"))
                      :| mempty
                , protoOfunctionDefinitionExpression =
                    EApplication
                      mempty
                      ()
                      (EVariable mempty (Label () "io$_println_int32"))
                      ( EVariable mempty (Label () "n")
                          :| mempty
                      )
                }
            )
        ]
    }

testModule1PreKinds :: (Monoid a) => Module a () ()
testModule1PreKinds =
  Module
    { protoOmodulePath = Path ["Main"]
    , protoOmoduleExportList = ExportAll
    , protoOmoduleDefinitions =
        [ DType
            mempty
            "IO"
            ( TypeDefinition
                { protoOtypeDefinitionParameters = [Parameter () "a"]
                , protoOtypeDefinitionConstructors = mempty
                }
            )
        , DImport
            mempty
            (Path ["Math"])
            [ NameImport mempty "factorial"
            ]
        , DImport
            mempty
            (Path ["IO"])
            [ NameImport mempty "println_int32"
            ]
        , DFunction
            mempty
            "main"
            ( FunctionDefinition
                { protoOfunctionDefinitionMetadata = mempty
                , protoOfunctionDefinitionAnnotation = Nothing
                , protoOfunctionDefinitionType = With mempty ()
                , protoOfunctionDefinitionPatterns =
                    PLiteral mempty LUnit :| mempty
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
                                  :| mempty
                              )
                              :| mempty
                          )
                          :| mempty
                      )
                }
            )
        ]
    }

-- testModule1 :: (Monoid a) => Module a Kind ()
-- testModule1 =
--  Module
--    { protoOmodulePath = Path ["Main"]
--    , protoOmoduleExportList = ExportAll
--    , protoOmoduleDefinitions =
--        [ DImport
--            mempty
--            (Path ["Math"])
--            [ NameImport mempty "factorial"
--            ]
--        , DImport
--            mempty
--            (Path ["IO"])
--            [ NameImport mempty "println_int32"
--            ]
--        , DFunction
--            mempty
--            "main"
--            ( FunctionDefinition
--                { protoOfunctionDefinitionMetadata = mempty
--                , protoOfunctionDefinitionAnnotation = Nothing
--                , protoOfunctionDefinitionType = With mempty ()
--                , protoOfunctionDefinitionPatterns =
--                    PLiteral mempty LUnit :| mempty
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
--                                  :| mempty
--                              )
--                              :| mempty
--                          )
--                          :| mempty
--                      )
--                }
--            )
--        ]
--    }

testModule2PreKinds :: (Monoid a) => Module a () ()
testModule2PreKinds =
  Module
    { protoOmodulePath = Path ["Math"]
    , protoOmoduleExportList = ExportAll
    , protoOmoduleDefinitions =
        [ DImport
            mempty
            (Path ["Nat"])
            [ NameImport mempty "pack"
            , NameImport mempty "unpack"
            ]
        , DImport
            mempty
            (Path ["Nat"])
            [ TypeImport mempty "Nat" ["Succ", "Zero"]
            ]
        , DFunction
            mempty
            "factorial"
            ( FunctionDefinition
                { protoOfunctionDefinitionMetadata =
                    mempty
                , protoOfunctionDefinitionAnnotation =
                    Just (With mempty (TIntrinsic IInt32))
                , protoOfunctionDefinitionType =
                    With mempty ()
                , protoOfunctionDefinitionPatterns =
                    PAnnotation
                      mempty
                      (TIntrinsic IInt32)
                      (PVariable mempty (Label () "n"))
                      :| mempty
                , protoOfunctionDefinitionExpression =
                    ERecursiveLet
                      mempty
                      (PVariable mempty (Label () "$fold"))
                      ( ELambda
                          mempty
                          (PVariable mempty (Label () "$fold.expr") :| mempty)
                          ( EMatch
                              mempty
                              ()
                              (EVariable mempty (Label () "$fold.expr"))
                              ( EClause
                                  mempty
                                  (PConstructor mempty (Label () "Zero") mempty)
                                  ( CPlain
                                      mempty
                                      mempty
                                      ( EApplication
                                          mempty
                                          ()
                                          (EVariable mempty (Label () "from_int32"))
                                          ( ELiteral mempty (LInt32 1)
                                              :| mempty
                                          )
                                      )
                                      :| mempty
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
                                        mempty
                                        ( EApplication
                                            mempty
                                            ()
                                            (EVariable mempty (Label () "(*)"))
                                            ( EApplication
                                                mempty
                                                ()
                                                (EVariable mempty (Label () "unpack"))
                                                ( EVariable mempty (Label () "m")
                                                    :| mempty
                                                )
                                                <| EVariable mempty (Label () "f")
                                                :| mempty
                                            )
                                        )
                                        :| mempty
                                    )
                                  :| mempty
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
                              (EVariable mempty (Label () "n") :| mempty)
                              :| mempty
                          )
                      )
                      -- EFold
                      --  mempty
                      --  ()
                      --  ( EApplication
                      --      mempty
                      --      ()
                      --      (EVariable mempty (Label () "pack"))
                      --      (EVariable mempty (Label () "n") :| mempty)
                      --      :| mempty
                      --  )
                      --  ( EClause
                      --      mempty
                      --      (PConstructor mempty (Label () "Zero") mempty)
                      --      ( CPlain
                      --          mempty
                      --          mempty
                      --          ( EApplication
                      --              mempty
                      --              ()
                      --              (EVariable mempty (Label () "from_int32"))
                      --              ( ELiteral mempty (LInt32 1)
                      --                  :| mempty
                      --              )
                      --          )
                      --          :| mempty
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
                      --            mempty
                      --            ( EApplication
                      --                mempty
                      --                ()
                      --                (EVariable mempty (Label () "(*)"))
                      --                ( EApplication
                      --                    mempty
                      --                    ()
                      --                    (EVariable mempty (Label () "unpack"))
                      --                    ( EVariable mempty (Label () "m")
                      --                        :| mempty
                      --                    )
                      --                    <| EVariable mempty (Label () "f")
                      --                    :| mempty
                      --                )
                      --            )
                      --            :| mempty
                      --        )
                      --      :| mempty
                      --  )
                }
            )
        ]
    }

-- testModule2 :: (Monoid a) => Module a Kind ()
-- testModule2 =
--  Module
--    { protoOmodulePath = Path ["Math"]
--    , protoOmoduleExportList = ExportAll
--    , protoOmoduleDefinitions =
--        [ DImport
--            mempty
--            (Path ["Nat"])
--            [ NameImport mempty "pack"
--            , NameImport mempty "unpack"
--            ]
--        , DFunction
--            mempty
--            "factorial"
--            ( FunctionDefinition
--                { protoOfunctionDefinitionMetadata =
--                    mempty
--                , protoOfunctionDefinitionAnnotation =
--                    Just (With mempty (TIntrinsic IInt32))
--                , protoOfunctionDefinitionType =
--                    With mempty ()
--                , protoOfunctionDefinitionPatterns =
--                    PAnnotation
--                      mempty
--                      (TIntrinsic IInt32)
--                      (PVariable mempty (Label () "n"))
--                      :| mempty
--                , protoOfunctionDefinitionExpression =
--                    EFold
--                      mempty
--                      ()
--                      ( EApplication
--                          mempty
--                          ()
--                          (EVariable mempty (Label () "pack"))
--                          (EVariable mempty (Label () "n") :| mempty)
--                          :| mempty
--                      )
--                      ( EClause
--                          mempty
--                          (PConstructor mempty (Label () "Zero") mempty)
--                          ( CPlain
--                              mempty
--                              mempty
--                              ( EApplication
--                                  mempty
--                                  ()
--                                  (EVariable mempty (Label () "from_int32"))
--                                  ( ELiteral mempty (LInt32 1)
--                                      :| mempty
--                                  )
--                              )
--                              :| mempty
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
--                                mempty
--                                ( EApplication
--                                    mempty
--                                    ()
--                                    (EVariable mempty (Label () "(*)"))
--                                    ( EApplication
--                                        mempty
--                                        ()
--                                        (EVariable mempty (Label () "unpack"))
--                                        ( EVariable mempty (Label () "m")
--                                            :| mempty
--                                        )
--                                        <| EVariable mempty (Label () "f")
--                                        :| mempty
--                                    )
--                                )
--                                :| mempty
--                            )
--                          :| mempty
--                      )
--                }
--            )
--        ]
--    }

testModule3PreKinds :: (Monoid a) => Module a () ()
testModule3PreKinds =
  Module
    { protoOmodulePath = Path ["Nat"]
    , protoOmoduleExportList = ExportAll
    , protoOmoduleDefinitions =
        [ DType
            mempty
            "Nat"
            ( TypeDefinition
                { protoOtypeDefinitionParameters = mempty
                , protoOtypeDefinitionConstructors =
                    [ DataConstructor
                        { constructorName = "Succ"
                        , constructorArity = 1
                        , constructorScheme = Forall mempty mempty (TIntrinsic INat `TArrow` TIntrinsic INat)
                        }
                    , DataConstructor
                        { constructorName = "Zero"
                        , constructorArity = 0
                        , constructorScheme = Forall mempty mempty (TIntrinsic INat)
                        }
                    ]
                }
            )
        , DFunction
            mempty
            "pack"
            ( FunctionDefinition
                { protoOfunctionDefinitionMetadata = mempty
                , protoOfunctionDefinitionAnnotation =
                    Just
                      ( With
                          mempty
                          (TIntrinsic INat)
                      )
                , protoOfunctionDefinitionType =
                    With mempty ()
                , protoOfunctionDefinitionPatterns =
                    PAnnotation
                      mempty
                      (TIntrinsic IInt32)
                      (PVariable mempty (Label () "m"))
                      :| mempty
                , protoOfunctionDefinitionExpression =
                    EApplication
                      mempty
                      ()
                      (EVariable mempty (Label () "nat$_pack"))
                      ( EVariable mempty (Label () "m")
                          :| mempty
                      )
                }
            )
        , DFunction
            mempty
            "unpack"
            ( FunctionDefinition
                { protoOfunctionDefinitionMetadata = mempty
                , protoOfunctionDefinitionAnnotation =
                    Just
                      ( With
                          mempty
                          (TIntrinsic IInt32)
                      )
                , protoOfunctionDefinitionType =
                    With mempty ()
                , protoOfunctionDefinitionPatterns =
                    PAnnotation
                      mempty
                      (TIntrinsic INat)
                      (PVariable mempty (Label () "n"))
                      :| mempty
                , protoOfunctionDefinitionExpression =
                    EApplication
                      mempty
                      ()
                      (EVariable mempty (Label () "nat$_unpack"))
                      ( EVariable mempty (Label () "n")
                          :| mempty
                      )
                }
            )
        ]
    }

testModule4PreKinds :: (Monoid a) => Module a () ()
testModule4PreKinds =
  Module
    { protoOmodulePath = Path ["Main"]
    , protoOmoduleExportList = ExportAll
    , protoOmoduleDefinitions =
        [ DFunction
            mempty
            "some_fun"
            ( FunctionDefinition
                { protoOfunctionDefinitionMetadata = mempty
                , protoOfunctionDefinitionAnnotation = Nothing
                , protoOfunctionDefinitionType = With mempty ()
                , protoOfunctionDefinitionPatterns =
                    PVariable mempty (Label () "m") :| mempty
                , protoOfunctionDefinitionExpression =
                    ELiteral mempty (LInt32 1)
                }
            )
        ]
    }

-- testModule3 :: (Monoid a) => Module a Kind ()
-- testModule3 =
--  Module
--    { protoOmodulePath = Path ["Nat"]
--    , protoOmoduleExportList = ExportAll
--    , protoOmoduleDefinitions =
--        [ DFunction
--            mempty
--            "pack"
--            ( FunctionDefinition
--                { protoOfunctionDefinitionMetadata = mempty
--                , protoOfunctionDefinitionAnnotation =
--                    Just
--                      ( With
--                          mempty
--                          (TIntrinsic INat)
--                      )
--                , protoOfunctionDefinitionType =
--                    With mempty ()
--                , protoOfunctionDefinitionPatterns =
--                    PAnnotation
--                      mempty
--                      (TIntrinsic IInt32)
--                      (PVariable mempty (Label () "m"))
--                      :| mempty
--                , protoOfunctionDefinitionExpression =
--                    EApplication
--                      mempty
--                      ()
--                      (EVariable mempty (Label () "nat$_pack"))
--                      ( EVariable mempty (Label () "m")
--                          :| mempty
--                      )
--                }
--            )
--        , DFunction
--            mempty
--            "unpack"
--            ( FunctionDefinition
--                { protoOfunctionDefinitionMetadata = mempty
--                , protoOfunctionDefinitionAnnotation =
--                    Just
--                      ( With
--                          mempty
--                          (TIntrinsic IInt32)
--                      )
--                , protoOfunctionDefinitionType =
--                    With mempty ()
--                , protoOfunctionDefinitionPatterns =
--                    PAnnotation
--                      mempty
--                      (TIntrinsic INat)
--                      (PVariable mempty (Label () "n"))
--                      :| mempty
--                , protoOfunctionDefinitionExpression =
--                    EApplication
--                      mempty
--                      ()
--                      (EVariable mempty (Label () "nat$_unpack"))
--                      ( EVariable mempty (Label () "n")
--                          :| mempty
--                      )
--                }
--            )
--        ]
--    }

--

-- testModule2B :: (Monoid a) => Module a Kind ()
-- testModule2B =
--  Module
--    { protoOmodulePath = Path ["Math"]
--    , protoOmoduleExportList = ExportAll
--    , protoOmoduleDefinitions =
--        [ DImport
--            mempty
--            (Path ["Nat"])
--            [ NameImport mempty "pack"
--            , NameImport mempty "unpack"
--            ]
--        , DFunction
--            mempty
--            "factorial"
--            ( FunctionDefinition
--                { protoOfunctionDefinitionMetadata =
--                    mempty
--                , protoOfunctionDefinitionAnnotation =
--                    Just (With mempty (TIntrinsic IInt32))
--                , protoOfunctionDefinitionType =
--                    With mempty ()
--                , protoOfunctionDefinitionPatterns =
--                    PAnnotation
--                      mempty
--                      (TIntrinsic IInt32)
--                      (PVariable mempty (Label () "n"))
--                      :| mempty
--                , protoOfunctionDefinitionExpression =
--                    ERecursiveLet
--                      mempty
--                      (PVariable mempty (Label () "$fold-70cdac64"))
--                      ( ELambda
--                          mempty
--                          (PVariable mempty (Label () "$variable-185c7b8df7b0") :| mempty)
--                          ( EMatch
--                              mempty
--                              ()
--                              (EVariable mempty (Label () "$variable-185c7b8df7b0"))
--                              ( EClause
--                                  mempty
--                                  (PConstructor mempty (Label () "Zero") mempty)
--                                  ( CPlain
--                                      mempty
--                                      mempty
--                                      ( EApplication
--                                          mempty
--                                          ()
--                                          (EVariable mempty (Label () "from_int32"))
--                                          ( ELiteral mempty (LInt32 1)
--                                              :| mempty
--                                          )
--                                      )
--                                      :| mempty
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
--                                        mempty
--                                        ( EApplication
--                                            mempty
--                                            ()
--                                            (EVariable mempty (Label () "(*)"))
--                                            ( EApplication
--                                                mempty
--                                                ()
--                                                (EVariable mempty (Label () "unpack"))
--                                                ( EVariable mempty (Label () "m")
--                                                    :| mempty
--                                                )
--                                                <| EApplication
--                                                  mempty
--                                                  ()
--                                                  (EVariable mempty (Label () "$fold-70cdac64"))
--                                                  ( EVariable mempty (Label () "f")
--                                                      :| mempty
--                                                  )
--                                                :| mempty
--                                            )
--                                        )
--                                        :| mempty
--                                    )
--                                  :| mempty
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
--                                  :| mempty
--                              )
--                              :| mempty
--                          )
--                      )
--                }
--            )
--        ]
--    }

-- testA :: (Monoid a) => IO (Either () (Build a))
-- testA = evalCompilerT (protoOprepareBuild testModuleBuiltins)

--  testB :: Module () Kind ()
--  testB = evalStateT (toKindIndexed (testModuleBuiltinsPreKinds :: Module () () ())) 0

-- testC :: ([(Name, Kind)], [KindConstraintsGenOutput])
-- testC = runKindConstraintsGen env (protoOemitKindConstraints testB)
-- where
--  env =
--    Environment.fromList
--      [ ("Numeric", KArrow KType KTrait)
--      ]

-- testD = protoOapplyKinds sub testB
-- where
--  Right sub = res
--  res :: Either KindError KindSubstitution
--  res = protoOkindUnifierMonad (protoOsolveKindConstraints constraints)
--  constraints = rights (snd testC)

testE :: IO (Either CompilerFailureMode (Build ())) -- CompilerT a m ()
testE = do
  evalCompilerT undefined $ do
    kindIndexedModule <- toKindIndexed (testModuleBuiltinsPreKinds :: Module () () ())
    env <- moduleKindEnvironment kindIndexedModule
    (_, res1) <- runKindConstraintsGen env (protoOemitKindConstraints kindIndexedModule)
    let constraints = rights res1
        errs = lefts res1
        Right sub = protoOkindUnifierMonad (protoOsolveKindConstraints constraints) :: Either KindError KindSubstitution
        res3 = protoOapplyKinds sub kindIndexedModule :: Module () Kind ()
    traceShowM errs
    traceShowM res3
    traceShowM (res3 == testModuleBuiltins)
    protoOprepareBuild res3
    protoOgetCurrentBuildC

testF :: IO (Either CompilerFailureMode (Build ())) -- CompilerT a m ()
testF = do
  evalCompilerT undefined $ do
    kindIndexedModule <- toKindIndexed (testModule0PreKinds :: Module () () ())
    env <- moduleKindEnvironment kindIndexedModule
    (_, res1) <- runKindConstraintsGen env (protoOemitKindConstraints kindIndexedModule)
    let constraints = rights res1
        errs = lefts res1
        Right sub = protoOkindUnifierMonad (protoOsolveKindConstraints constraints) :: Either KindError KindSubstitution
        res3 = protoOapplyKinds sub kindIndexedModule :: Module () Kind ()
    traceShowM errs
    traceShowM (res3 == testModule0)
    protoOprepareBuild res3
    protoOgetCurrentBuildC

testG :: IO (Either CompilerFailureMode (Build ())) -- CompilerT a m ()
testG = do
  evalCompilerT undefined $ do
    kindIndexedModule <- toKindIndexed (testModuleBuiltinsPreKinds :: Module () () ())
    env <- moduleKindEnvironment kindIndexedModule
    (_, res1) <- runKindConstraintsGen env (protoOemitKindConstraints kindIndexedModule)
    let constraints = rights res1
        errs = lefts res1
        Right sub = protoOkindUnifierMonad (protoOsolveKindConstraints constraints) :: Either KindError KindSubstitution
        res3 = protoOapplyKinds sub kindIndexedModule :: Module () Kind ()
    protoOprepareBuild res3
    --
    kindIndexedModule <- toKindIndexed (testModule0PreKinds :: Module () () ())
    env <- moduleKindEnvironment kindIndexedModule
    (_, res1) <- runKindConstraintsGen env (protoOemitKindConstraints kindIndexedModule)
    let constraints = rights res1
        errs = lefts res1
        Right sub = protoOkindUnifierMonad (protoOsolveKindConstraints constraints) :: Either KindError KindSubstitution
        res3 = protoOapplyKinds sub kindIndexedModule :: Module () Kind ()
    traceShowM errs
    traceShowM res3
    traceShowM (res3 == testModule0)
    protoOprepareBuild res3
    protoOgetCurrentBuildC

-- updateNames :: (Monad m, Data a) => [Definition a Kind IndexedType] -> CompilerT a m ()
-- updateNames defs =
--  forM_ defs $

defineName :: (Monad m, Data a) => Definition a Kind IndexedType -> CompilerT a m ()
defineName =
  \case
    def@(DFunction _ name FunctionDefinition{..}) ->
      protoOdefine name (typeOf def)
    def@(DLet _ name LetDefinition{..}) ->
      protoOdefine name (typeOf def)
    DInstance _ InstanceDefinition{..} -> do
      let trait = Trait protoOinstanceDefinitionTraitName protoOinstanceDefinitionType
      forM_ protoOinstanceDefinitionImplementations $
        \case
          def@(DFunction _ name _) -> do
            let to = (typeOf def)
            protoOdefine (instanceLabel trait name) to
          def@(DLet _ name _) ->
            protoOdefine (instanceLabel trait name) (typeOf def)
    _ ->
      pure ()

xyz :: [Module Metadata () ()] -> IO ()
xyz modules = do
  (_, r, _) <- runCompilerT undefined $ do
    forM_ modules $
      \modul -> do
        protoOclearAssumptionsC
        protoOclearNameStoreC
        setCurrentModuleC modul

        res3 <- inferKinds modul
        protoOprepareBuild res3

        Module _ _ defs1 <- inferTypes res3
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
        -- sub1 <- gets protoOcompilerSubstitution
        --        traceShowM sub1
        pPrint qq

        --        let Module _ _ defs = c :: Module Metadata Kind IndexedType
        --        (tdefs, _) <- typeDefinitionsC defs

        -- traceShowM "********"
        -- traceShowM c
        -- traceShowM asms

        pure ()

  --  pPrint r

  pure ()

indexTypes :: (Monad m, Traversable t) => t e -> CompilerT a m (t IndexedType)
indexTypes ds = run (indexed ds) =<< gets protoOcompilerSupply
 where
  run s m = do
    let (r, n) = runState s m
    protoOupdateSupplyC n
    pure r

inferKinds :: (Monad m) => Module Metadata () () -> CompilerT Metadata m (Module Metadata Kind ())
inferKinds modul = do
  indexed <- toKindIndexed modul
  generateKindConstraints indexed
  constraints <- gets protoOcompilerKindConstraints
  case protoOkindUnifierMonad (protoOsolveKindConstraints constraints) of
    Left err ->
      error "TODO"
    Right sub ->
      return (protoOapplyKinds sub indexed)

inferTypes :: (MonadIO m, Data a, Show a, Eq a) => Module a Kind () -> CompilerT a m (Module a Kind IndexedType)
inferTypes modul = do
  Module{..} <- indexTypes modul
  forM_ protoOmoduleDefinitions $
    \def -> do
      protoOgenerateConstraints def
      sub <- solveX
      defineName (apply sub def)
  sub <- gets protoOcompilerSubstitution
  modify (overCompilerAssumptions (apply sub))
  pure $
    Module
      { protoOmoduleDefinitions = fmap (fmap normalizeRowTypes) (apply sub protoOmoduleDefinitions)
      , ..
      }

-- typeDefinition1 :: (Monad m, Data a, Show a) => Definition a Kind IndexedType -> CompilerT a m ()
-- typeDefinition1 =
--  \case
--    DFunction _ name FunctionDefinition { .. } -> do
--      protoOgenerateConstraints protoOfunctionDefinitionExpression
--
--    -- define name (typeOf (apply sub def))
--    DLet _ name LetDefinition{..} ->
--      -- generateConstraints def
--      -- solve
--      pure ()
--    -- define name (typeOf (apply sub def))
--
--    DInstance a InstanceDefinition{..} ->
--      forM_ protoOinstanceDefinitionImplementations $
--        \case
--          DFunction _ name FunctionDefinition{..} ->
--            pure ()
--          DLet _ name LetDefinition{..} ->
--            pure ()
--    _ ->
--      pure ()

testModule9 :: (Monoid a) => Module a () ()
testModule9 =
  Module
    { protoOmodulePath = Path ["IO"]
    , protoOmoduleExportList = ExportAll
    , protoOmoduleDefinitions =
        [ DType
            mempty
            "IO"
            ( TypeDefinition
                { protoOtypeDefinitionParameters = [Parameter () "a"]
                , protoOtypeDefinitionConstructors = mempty
                }
            )
        , DFunction
            mempty
            "println_int32"
            ( FunctionDefinition
                { protoOfunctionDefinitionMetadata = mempty
                , protoOfunctionDefinitionAnnotation =
                    Just
                      ( With
                          mempty
                          ( TApplication
                              ()
                              (TConstructor () "IO")
                              (TIntrinsic IUnit)
                          )
                      )
                , protoOfunctionDefinitionType = With mempty ()
                , protoOfunctionDefinitionPatterns =
                    PAnnotation
                      mempty
                      (TIntrinsic IInt32)
                      (PVariable mempty (Label () "n"))
                      :| mempty
                , protoOfunctionDefinitionExpression =
                    EApplication
                      mempty
                      ()
                      (EVariable mempty (Label () "io$_println_int32"))
                      ( EVariable mempty (Label () "n")
                          :| mempty
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
