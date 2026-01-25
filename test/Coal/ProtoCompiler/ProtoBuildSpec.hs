{-# LANGUAGE OverloadedStrings #-}

module Coal.ProtoCompiler.ProtoBuildSpec where

import Coal.Common.Label (Label (..))
import Coal.Language
import Coal.Language.Module.Import (Import (..))
import Coal.Language.Module.Path (Path (..))
import Coal.ProtoLanguage.ProtoDefinition
import Coal.ProtoLanguage.ProtoModule (ProtoModule (..))
import Data.List.NonEmpty (NonEmpty (..), (<|))
import qualified Data.Set as Set

testModuleBuiltins :: (Monoid a) => ProtoModule a Kind ()
testModuleBuiltins =
  ProtoModule
    { protoOmodulePath = Path ["Builtin"]
    , protoOmoduleDefinitions =
        [ ProtoDTrait
            mempty
            "Numeric"
            ( ProtoTraitDefinition
                { protoOtraitDefinitionMetadata =
                    mempty
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
                        --                    , ProtoDFunction
                        --                        mempty
                        --                        "from_int64"
                        --                        ( ProtoFunctionDefinition
                        --                            { protoOfunctionDefinitionMetadata =
                        --                                mempty
                        --                            , protoOfunctionDefinitionAnnotation =
                        --                                Nothing
                        --                            , protoOfunctionDefinitionType =
                        --                                With [] ()
                        --                            , protoOfunctionDefinitionPatterns =
                        --                                PVariable mempty (Label () "n") :| []
                        --                            , protoOfunctionDefinitionExpression =
                        --                                EVariable mempty (Label () "n")
                        --                            }
                        --                        )
                        --                    , ProtoDFunction
                        --                        mempty
                        --                        "from_bignum"
                        --                        ( ProtoFunctionDefinition
                        --                            { protoOfunctionDefinitionMetadata =
                        --                                mempty
                        --                            , protoOfunctionDefinitionAnnotation =
                        --                                Nothing
                        --                            , protoOfunctionDefinitionType =
                        --                                With [] ()
                        --                            , protoOfunctionDefinitionPatterns =
                        --                                PVariable mempty (Label () "n") :| []
                        --                            , protoOfunctionDefinitionExpression =
                        --                                EFFICall
                        --                                  mempty
                        --                                  undefined
                        --                                  undefined
                        --                                  undefined
                        --                                  undefined
                        --                            }
                        --                        )
                        --                    , ProtoDFunction
                        --                        mempty
                        --                        "negate"
                        --                        ( ProtoFunctionDefinition
                        --                            { protoOfunctionDefinitionMetadata =
                        --                                mempty
                        --                            , protoOfunctionDefinitionAnnotation =
                        --                                Nothing
                        --                            , protoOfunctionDefinitionType =
                        --                                With [] ()
                        --                            , protoOfunctionDefinitionPatterns =
                        --                                PVariable mempty (Label () "n") :| []
                        --                            , protoOfunctionDefinitionExpression =
                        --                                undefined
                        --                            }
                        --                        )
                        --                    , ProtoDFunction
                        --                        mempty
                        --                        "(+)"
                        --                        ( ProtoFunctionDefinition
                        --                            { protoOfunctionDefinitionMetadata =
                        --                                mempty
                        --                            , protoOfunctionDefinitionAnnotation =
                        --                                Nothing
                        --                            , protoOfunctionDefinitionType =
                        --                                With [] ()
                        --                            , protoOfunctionDefinitionPatterns =
                        --                                PVariable mempty (Label () "m") <| PVariable mempty (Label () "n") :| []
                        --                            , protoOfunctionDefinitionExpression =
                        --                                undefined
                        --                            }
                        --                        )
                        --                    , ProtoDFunction
                        --                        mempty
                        --                        "(-)"
                        --                        ( ProtoFunctionDefinition
                        --                            { protoOfunctionDefinitionMetadata =
                        --                                mempty
                        --                            , protoOfunctionDefinitionAnnotation =
                        --                                Nothing
                        --                            , protoOfunctionDefinitionType =
                        --                                With [] ()
                        --                            , protoOfunctionDefinitionPatterns =
                        --                                PVariable mempty (Label () "m") <| PVariable mempty (Label () "n") :| []
                        --                            , protoOfunctionDefinitionExpression =
                        --                                undefined
                        --                            }
                        --                        )
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
