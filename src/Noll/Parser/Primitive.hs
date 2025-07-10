{-# LANGUAGE OverloadedStrings #-}

module Noll.Parser.Primitive where

import Control.Monad.Combinators.Expr
import Data.Functor (($>))
import Lang.Common.List1 (NonEmpty (..))
import Lang.Label (Label (..))
import Noll.Language
import Noll.Parser
import Noll.Parser.Identifier
import Noll.Parser.Symbol
import Text.Megaparsec (try, (<|>))

primitiveParser =
  undefined
