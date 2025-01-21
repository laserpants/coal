module Noll.Compiler.PatternMatchingSpec.TestRunner where

import Noll.Compiler.PatternMatching
import Noll.Label (Label (..))
import Noll.Language (Expression (..), Primitive (..))

compilePatterns :: (TypeProxy t, Ord t, Monoid a) => [Label t] -> [PatternEquation (Expression a) t] -> EnvelopeExpression (Expression a) t -> MatchMonad (Expression a t)
compilePatterns ls ps e = compileEnvelope <$> matchPatterns ls ps e
