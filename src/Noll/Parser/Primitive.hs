{-# LANGUAGE OverloadedStrings #-}

module Noll.Parser.Primitive where

import Control.Monad.Combinators.Expr
import Data.Functor (($>))
import Noll.Language
import Noll.Parser
import Noll.Parser.Symbol
import Lang.Common.List1 (NonEmpty (..))
import Lang.Label (Label (..))
import Text.Megaparsec ((<|>), try)
import Noll.Parser.Identifier

primitiveParser =
  undefined
