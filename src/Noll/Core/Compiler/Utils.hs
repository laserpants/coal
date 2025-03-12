{-# LANGUAGE LambdaCase #-}

module Noll.Core.Compiler.Utils (muteTypes, muteObjectTypes) where

import Data.Functor.Foldable (cata)
import Noll.Core.Language (Clause (..), Expr, ExprF (..), Focus (..), Type, overBindingLabel)
import Noll.Core.Language.Object (Object (..))
import Noll.Label (Label (..))

import qualified Noll.Core.Language as Core

muteTypes :: Expr Type -> Expr ()
muteTypes =
  cata $
    \case
      EVar (Label _ name) ->
        Core.var (Label () name)
      ELet vs e ->
        Core.let_ (overBindingLabel muteLabelTypes <$> vs) e
      ELit p ->
        Core.lit p
      ELam lls e ->
        Core.lam (muteLabelTypes <$> lls) e
      EApp _ a es ->
        Core.app () a es
      EIf e1 e2 e3 ->
        Core.if_ e1 e2 e3
      EOp op ->
        Core.op op
      EMat _ e1 cs ->
        Core.match () e1 (muteClauseTypes <$> cs)
      EExt f e1 e2 ->
        Core.ext f e1 e2
      ENil ->
        Core.nil
      ESel (Focus name ll1 ll2) e1 e2 ->
        Core.sel (Focus name (muteLabelTypes ll1) (muteLabelTypes ll2)) e1 e2
      ECall ll es e ->
        Core.call (muteLabelTypes ll) es e
      EMem e ->
        Core.mem e

muteClauseTypes :: Clause Type (Expr ()) -> Clause () (Expr ())
muteClauseTypes (Clause lls e) = Clause (muteLabelTypes <$> lls) e

muteLabelTypes :: Label Type -> Label ()
muteLabelTypes (Label _ name) = Label () name

muteObjectTypes :: Object Type (Expr Type) -> Object () (Expr ())
muteObjectTypes =
  \case
    OFunction name lls e ->
      OFunction name (muteLabelTypes <$> lls) (muteTypes e)
    OConstant name e ->
      OConstant name (muteTypes e)
    OExternal name _ ->
      OExternal name ()
