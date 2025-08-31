{-# LANGUAGE OverloadedStrings #-}

module Coal.Compiler.Transform.Definition.FoldSpec where

import Control.Monad.Identity
import Coal.Common.Label (Label (..), labelName)
import Coal.Common.Supply (suppliedName)
import Coal.Compiler.Transform.Definition.Fold
import Coal.Compiler.Transform.Expression
import Coal.Compiler.Transform.Flattening (flattenApplication)
import Coal.Compiler.Transform.Tree (replace)
import Coal.Language 
import Coal.Language.Module (Constant (..), Definition (..), Function (..), Module (..))
import Control.Monad.RWS (RWS, runRWS)
import Control.Monad.Reader (MonadReader)
import Control.Monad.State (MonadState)
import Control.Monad.Writer (execWriter, tell)
import Data.Data (Data)
import Data.Generics.Uniplate.Data (transform, transformM)
import Data.List.NonEmpty (NonEmpty (..), (<|))
import Extra (Dictionary, Name, const2)

cfixture1 =
  DFold
    "encode_json_value"
    ( EClause
        ()
        (
          PConstructor () (Label () "JsonNull") []
        )
        ( CPlain
            ()
            []
            (ELiteral () (LString "null"))
            :| []
        )
        <| EClause
          ()
          (
            PConstructor () (Label () "JsonBool") [ PLiteral () (LBool False) ]
          )
          ( CPlain
              ()
              []
              (ELiteral () (LString "false"))
              :| []
          )
        <| EClause
          ()
          (
            PConstructor () (Label () "JsonBool") [ PLiteral () (LBool True) ]
          )
          ( CPlain
              ()
              []
              (ELiteral () (LString "true"))
              :| []
          )
        <| EClause
          ()
          (
            PConstructor () (Label () "JsonArray") [ PNamedAtVariable () "encode_json_array" (Label () "values") ]
          )
          ( CPlain
              ()
              []
              (EVariable () (Label () "values"))
              :| []
          )
        <| EClause
          ()
          (
            PConstructor () (Label () "JsonObject") [ PNamedAtVariable () "encode_json_object" (Label () "key_value_pairs")  ]
          )
          ( CPlain
              ()
              []
              (EVariable () (Label () "key_value_pairs"))
              :| []
          )
        :| []
    )
    Nothing

runTestA = runIdentity (compileTopLevelFolds cfixture1)

