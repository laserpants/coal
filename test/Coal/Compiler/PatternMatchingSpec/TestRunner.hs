module Coal.Compiler.PatternMatchingSpec.TestRunner (compilePatterns) where

import Data.Data (Data)
import Coal.Common.Label (Label (..))
import Coal.Compiler.PatternMatching
import Coal.Compiler.PatternMatching.Envelope
import Coal.Compiler.PatternMatching.Equation
import Coal.Compiler.PatternMatching.Rule
import Coal.Language (Expression (..))

compilePatterns :: (TypeProxy t, Ord t, Data t, Data a, Monoid a) => [Label t] -> [PatternEquation (Expression a) t] -> EnvelopeExpression (Expression a) t -> MatchMonad (Expression a t)
compilePatterns ls ps e = compileEnvelope <$> matchPatterns ls ps e
