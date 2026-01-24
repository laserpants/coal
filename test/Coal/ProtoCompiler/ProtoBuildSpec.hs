{-# LANGUAGE OverloadedStrings #-}

module Coal.ProtoCompiler.ProtoBuildSpec where

import Coal.Language.Module.Path (Path (..))
import Coal.ProtoLanguage.ProtoDefinition (ProtoDefinition (..))
import Coal.ProtoLanguage.ProtoModule (ProtoModule (..))

testModule1 :: (Monoid a) => ProtoModule a
testModule1 =
  ProtoModule
    { protoOmodulePath = Path ["Main"]
    , protoOmoduleDefinitions =
        [ ProtoDFunction
            mempty
            "main"
        ]
    }

testModule2 :: (Monoid a) => ProtoModule a
testModule2 =
  ProtoModule
    { protoOmodulePath = Path ["Math"]
    , protoOmoduleDefinitions =
        [ ProtoDFunction
            mempty
            "factorial"
        ]
    }

testModule3 :: (Monoid a) => ProtoModule a
testModule3 =
  ProtoModule
    { protoOmodulePath = Path ["Nat"]
    , protoOmoduleDefinitions =
        [ ProtoDFunction
            mempty
            "pack"
        , ProtoDFunction
            mempty
            "unpack"
        ]
    }
