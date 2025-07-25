module Noll.Compiler.PatternMatchingSpec.TestRunner (compilePatterns) where

import Data.Data (Data)
import Lang.Common.Label (Label (..))
import Noll.Compiler.PatternMatching
import Noll.Compiler.PatternMatching.Envelope
import Noll.Compiler.PatternMatching.Equation
import Noll.Compiler.PatternMatching.Rule
import Noll.Language (Expression (..))

compilePatterns :: (TypeProxy t, Ord t, Data t, Data a, Monoid a) => [Label t] -> [PatternEquation (Expression a) t] -> EnvelopeExpression (Expression a) t -> MatchMonad (Expression a t)
compilePatterns ls ps e = compileEnvelope <$> matchPatterns ls ps e
