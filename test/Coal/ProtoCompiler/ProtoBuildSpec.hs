{-# LANGUAGE OverloadedStrings #-}

module Coal.ProtoCompiler.ProtoBuildSpec where

import Coal.Language.Module.Path (Path (..))
import Coal.ProtoLanguage.ProtoDefinition (ProtoDefinition (..), ProtoFunctionDefinition (..))
import Coal.ProtoLanguage.ProtoModule (ProtoModule (..))

testModule1 :: (Monoid a) => ProtoModule a t
testModule1 =
  ProtoModule
    { protoOmodulePath = Path ["Main"]
    , protoOmoduleDefinitions =
        [ ProtoDFunction
            mempty
            "main"
            ( ProtoFunctionDefinition
                { protoOfunctionDefinitionExpression = undefined
                }
            )
        ]
    }

testModule2 :: (Monoid a) => ProtoModule a t
testModule2 =
  ProtoModule
    { protoOmodulePath = Path ["Math"]
    , protoOmoduleDefinitions =
        [ ProtoDFunction
            mempty
            "factorial"
            ( ProtoFunctionDefinition
                { protoOfunctionDefinitionExpression = undefined
                }
            )
        ]
    }

testModule3 :: (Monoid a) => ProtoModule a t
testModule3 =
  ProtoModule
    { protoOmodulePath = Path ["Nat"]
    , protoOmoduleDefinitions =
        [ ProtoDFunction
            mempty
            "pack"
            ( ProtoFunctionDefinition
                { protoOfunctionDefinitionExpression = undefined
                }
            )
        , ProtoDFunction
            mempty
            "unpack"
            ( ProtoFunctionDefinition
                { protoOfunctionDefinitionExpression = undefined
                }
            )
        ]
    }
