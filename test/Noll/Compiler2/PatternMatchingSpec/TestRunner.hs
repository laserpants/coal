module Noll.Compiler2.PatternMatchingSpec.TestRunner (compilePatterns) where

import Data.Data (Data)
import Lang.Label (Label (..))
import Noll.Compiler2.PatternMatching
import Noll.Compiler2.PatternMatching.Envelope
import Noll.Compiler2.PatternMatching.Equation
import Noll.Compiler2.PatternMatching.Rule
import Noll.Language (Expression (..))

compilePatterns :: (TypeProxy t, Ord t, Data t, Data a, Monoid a) => [Label t] -> [PatternEquation (Expression a) t] -> EnvelopeExpression (Expression a) t -> MatchMonad (Expression a t)
compilePatterns ls ps e = compileEnvelope <$> matchPatterns ls ps e
