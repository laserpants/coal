{-# LANGUAGE OverloadedStrings #-}

module Coal.TypeSystem.Constraint.Generation.ELambdaConstraintsSpec where

import Coal.Common.Label (Label (..))
import Coal.Language
import Coal.TypeSystem.Constraint
import Coal.TypeSystem.Constraint.Generation
import Coal.TypeSystem.Constraint.Generation.InferenceRule
import Data.Either (lefts, rights)

import qualified Coal.Common.Environment as Environment


