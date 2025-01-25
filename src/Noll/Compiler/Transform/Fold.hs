module Noll.Compiler.Transform.Fold where

import Noll.Common.List1 (List1 (..))
import Noll.Language (Clause (..), Expression (..))

expandFoldExpression :: List1 (Expression a t) -> List1 (Clause Expression a t) -> Expression a t
expandFoldExpression =
  undefined
