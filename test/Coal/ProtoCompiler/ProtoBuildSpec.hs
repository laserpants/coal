{-# LANGUAGE OverloadedStrings #-}

module Coal.ProtoCompiler.ProtoBuildSpec where

import Coal.Language.Expression
import Coal.Language.Module.Import (Import (..))
import Coal.Language.Module.Path (Path (..))
import Coal.ProtoLanguage.ProtoDefinition (ProtoDefinition (..), ProtoFunctionDefinition (..))
import Coal.ProtoLanguage.ProtoModule (ProtoModule (..))

testModule1 :: (Monoid a) => ProtoModule a ()
testModule1 =
  ProtoModule
    { protoOmodulePath = Path ["Main"]
    , protoOmoduleDefinitions =
        [ ProtoDImport
            mempty
            (Path ["Math"])
            [ ImportName mempty "factorial"
            ]
        , ProtoDImport
            mempty
            (Path ["IO"])
            [ ImportName mempty "println_int32"
            ]
        , ProtoDFunction
            mempty
            "main"
            ( ProtoFunctionDefinition
                { protoOfunctionDefinitionExpression =
                    EApplication
                      mempty
                      ()
                      undefined
                      undefined
                }
            )
        ]
    }

testModule2 :: (Monoid a) => ProtoModule a ()
testModule2 =
  ProtoModule
    { protoOmodulePath = Path ["Math"]
    , protoOmoduleDefinitions =
        [ ProtoDImport
            mempty
            (Path ["Nat"])
            [ ImportName mempty "pack"
            , ImportName mempty "unpack"
            ]
        , ProtoDFunction
            mempty
            "factorial"
            ( ProtoFunctionDefinition
                { protoOfunctionDefinitionExpression =
                    EFold
                      mempty
                      ()
                      undefined
                      undefined
                }
            )
        ]
    }

testModule3 :: (Monoid a) => ProtoModule a ()
testModule3 =
  ProtoModule
    { protoOmodulePath = Path ["Nat"]
    , protoOmoduleDefinitions =
        [ ProtoDFunction
            mempty
            "pack"
            ( ProtoFunctionDefinition
                { protoOfunctionDefinitionExpression =
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
                { protoOfunctionDefinitionExpression =
                    EApplication
                      mempty
                      ()
                      undefined
                      undefined
                }
            )
        ]
    }
