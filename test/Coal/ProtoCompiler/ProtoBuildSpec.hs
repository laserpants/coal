{-# LANGUAGE OverloadedStrings #-}

module Coal.ProtoCompiler.ProtoBuildSpec where

import Coal.Common.Label (Label (..))
import Coal.Language
import Coal.Language.Module.Import (Import (..))
import Coal.Language.Module.Path (Path (..))
import Coal.ProtoLanguage.ProtoDefinition (ProtoDefinition (..), ProtoFunctionDefinition (..))
import Coal.ProtoLanguage.ProtoModule (ProtoModule (..))
import Data.List.NonEmpty (NonEmpty (..), (<|))

testModule0 :: (Monoid a) => ProtoModule a Kind ()
testModule0 =
  ProtoModule
    { protoOmodulePath = Path ["IO"]
    , protoOmoduleDefinitions =
        []
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
                { protoOunctionDefinitionMetadata = mempty
                , protoOunctionDefinitionAnnotation = Nothing
                , protoOunctionDefinitionType = With [] ()
                , protoOunctionDefinitionPatterns =
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
                { protoOunctionDefinitionMetadata =
                    mempty
                , protoOunctionDefinitionAnnotation =
                    Just (With [] (TIntrinsic IInt32))
                , protoOunctionDefinitionType =
                    With [] ()
                , protoOunctionDefinitionPatterns =
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
                          undefined
                          undefined
                          undefined
                          <| EClause
                            undefined
                            undefined
                            undefined
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
                { protoOunctionDefinitionMetadata = mempty
                , protoOunctionDefinitionAnnotation = Nothing
                , protoOunctionDefinitionType = With [] ()
                , protoOunctionDefinitionPatterns = undefined
                , protoOfunctionDefinitionExpression =
                    EApplication
                      mempty
                      ()
                      undefined
                      undefined
                }
            )
        , ProtoDFunction
            mempty
            "unpack"
            ( ProtoFunctionDefinition
                { protoOunctionDefinitionMetadata = mempty
                , protoOunctionDefinitionAnnotation = Nothing
                , protoOunctionDefinitionType = With [] ()
                , protoOunctionDefinitionPatterns = undefined
                , protoOfunctionDefinitionExpression =
                    EApplication
                      mempty
                      ()
                      undefined
                      undefined
                }
            )
        ]
    }
